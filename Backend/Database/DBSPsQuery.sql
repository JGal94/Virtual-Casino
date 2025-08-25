USE Casino
GO
CREATE OR ALTER PROCEDURE dbo.usp_Asiento_CrearSimple
  @IdTipo SMALLINT,
  @ReferenciaExterna NVARCHAR(100) = NULL,
  @Movs dbo.TVP_Movimiento READONLY,
  @IdAsiento BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  DECLARE @sumDebe DECIMAL(19,4), @sumHaber DECIMAL(19,4);

  SELECT @sumDebe = COALESCE(SUM(Debe),0), @sumHaber = COALESCE(SUM(Haber),0) FROM @Movs;
  IF @sumDebe <> @sumHaber
  BEGIN
    THROW 50001, 'El asiento no cuadra (∑Debe != ∑Haber).', 1;
  END

  -- Validar cuentas
  IF EXISTS (
    SELECT 1
    FROM @Movs m
    LEFT JOIN dbo.Cuentas c ON c.IdCuenta = m.IdCuenta
    WHERE c.IdCuenta IS NULL
  )
  BEGIN
    THROW 50002, 'Alguna IdCuenta no existe.', 1;
  END

  -- Transacción
  BEGIN TRAN;
    INSERT INTO dbo.Asientos(IdTipo, Fecha, ReferenciaExterna)
    VALUES(@IdTipo, SYSUTCDATETIME(), @ReferenciaExterna);

    SET @IdAsiento = SCOPE_IDENTITY();

    INSERT INTO dbo.Movimientos(IdAsiento, IdCuenta, Debe, Haber, Ts)
    SELECT @IdAsiento, IdCuenta, Debe, Haber, SYSUTCDATETIME()
    FROM @Movs;

    ;WITH deltas AS (
      SELECT IdCuenta, SUM(Debe - Haber) AS Delta
      FROM @Movs GROUP BY IdCuenta
    )
    UPDATE c
      SET c.SaldoCache = c.SaldoCache + d.Delta
    FROM dbo.Cuentas c
    JOIN deltas d ON d.IdCuenta = c.IdCuenta;
  COMMIT;
END
GO


-- Abrir cuenta (1 por moneda por usuario)
CREATE OR ALTER PROCEDURE dbo.usp_Cuenta_Abrir
  @IdUsuario INT,
  @IdMoneda  INT,
  @IdCuenta  BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF EXISTS (SELECT 1 FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda)
  BEGIN
    SELECT @IdCuenta = IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;
    RETURN;
  END
  INSERT INTO dbo.Cuentas(IdUsuario, IdMoneda, SaldoCache, Bloqueada)
  VALUES(@IdUsuario, @IdMoneda, 0, 0);
  SET @IdCuenta = SCOPE_IDENTITY();
END
GO

-- Obtener saldo efectivo (desde movimientos) y cache
CREATE OR ALTER PROCEDURE dbo.usp_Cuenta_ObtenerSaldo
  @IdUsuario INT,
  @IdMoneda  INT
AS
BEGIN
  SET NOCOUNT ON;
  DECLARE @IdCuenta BIGINT;
  SELECT @IdCuenta = IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;
  IF @IdCuenta IS NULL
  BEGIN
    SELECT CAST(NULL AS BIGINT) AS IdCuenta, CAST(NULL AS DECIMAL(19,4)) AS SaldoCache,
           CAST(NULL AS DECIMAL(19,4)) AS SaldoEfectivo;
    RETURN;
  END

  SELECT
    c.IdCuenta,
    c.SaldoCache,
    (SELECT COALESCE(SUM(Debe - Haber),0) FROM dbo.Movimientos WHERE IdCuenta=c.IdCuenta) AS SaldoEfectivo
  FROM dbo.Cuentas c
  WHERE c.IdCuenta=@IdCuenta;
END
GO

-- Recalcular SaldoCache desde Movimientos (por si se descuadra)
CREATE OR ALTER PROCEDURE dbo.usp_Billetera_SincronizarSaldoCache
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  ;WITH recal AS (
    SELECT IdCuenta, SUM(Debe - Haber) AS Nuevo
    FROM dbo.Movimientos GROUP BY IdCuenta
  )
  UPDATE c
    SET c.SaldoCache = COALESCE(r.Nuevo,0)
  FROM dbo.Cuentas c
  LEFT JOIN recal r ON r.IdCuenta = c.IdCuenta;
END
GO


CREATE OR ALTER PROCEDURE dbo.usp_Ronda_Abrir
  @IdJuego INT,
  @IdMesa  INT = NULL,
  @IdRonda BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  INSERT INTO dbo.Rondas(IdJuego, IdMesa, Inicio, Estado)
  VALUES(@IdJuego, @IdMesa, SYSUTCDATETIME(), N'open');
  SET @IdRonda = SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Ronda_Cerrar
  @IdRonda BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Rondas
    SET Fin = SYSUTCDATETIME(),
        Estado = N'closed'
  WHERE IdRonda=@IdRonda AND Estado <> N'closed';
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Ronda_RegistrarResultado
  @IdRonda BIGINT,
  @Payload NVARCHAR(MAX),
  @Checksum VARBINARY(32) = NULL
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  MERGE dbo.ResultadosRonda AS tgt
  USING (SELECT @IdRonda AS IdRonda) AS src
  ON tgt.IdRonda = src.IdRonda
  WHEN MATCHED THEN
    UPDATE SET PayloadResultado=@Payload, Checksum=@Checksum, Ts=SYSUTCDATETIME()
  WHEN NOT MATCHED THEN
    INSERT(IdRonda, PayloadResultado, Checksum, Ts)
    VALUES(@IdRonda, @Payload, @Checksum, SYSUTCDATETIME());
END
GO


-- Colocar apuesta: descuenta saldo del usuario y acredita banca (cuenta casa juego)
/* =========================
   usp_Apuesta_Colocar (patch)
   ========================= */
CREATE OR ALTER PROCEDURE dbo.usp_Apuesta_Colocar
  @IdUsuario INT,
  @IdRonda   BIGINT,
  @IdMoneda  INT,
  @Importe   DECIMAL(19,4),
  @IdTipoAsiento SMALLINT,
  @IdCuentaCasaJuego BIGINT,
  @Detalle dbo.TVP_ApuestaDetalle READONLY,
  @IdApuesta BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;

  IF @Importe <= 0 THROW 50010, N'Importe inválido.', 1;

  DECLARE @estado NVARCHAR(20);
  SELECT @estado = Estado FROM dbo.Rondas WHERE IdRonda=@IdRonda;
  IF @estado IS NULL OR @estado <> N'open' THROW 50011, N'Ronda no abierta.', 1;

  DECLARE @IdCuentaUsuario BIGINT;
  SELECT @IdCuentaUsuario = IdCuenta
  FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;
  IF @IdCuentaUsuario IS NULL THROW 50012, N'El usuario no tiene cuenta para esa moneda.', 1;

  DECLARE @saldo DECIMAL(19,4);
  SELECT @saldo = SaldoCache FROM dbo.Cuentas WITH (UPDLOCK, ROWLOCK) WHERE IdCuenta=@IdCuentaUsuario;
  IF @saldo < @Importe THROW 50013, N'Saldo insuficiente.', 1;

  BEGIN TRAN;
    INSERT INTO dbo.Apuestas(IdUsuario, IdRonda, IdMoneda, Importe, Estado, Ts)
    VALUES(@IdUsuario, @IdRonda, @IdMoneda, @Importe, N'placed', SYSUTCDATETIME());
    SET @IdApuesta = SCOPE_IDENTITY();

    INSERT INTO dbo.ApuestaDetalle(IdApuesta, Seleccion, Multiplicador)
    SELECT @IdApuesta, Seleccion, Multiplicador FROM @Detalle;

    DECLARE @Movs dbo.TVP_Movimiento, @IdAsiento BIGINT, @Ref NVARCHAR(100);
    SET @Ref = N'APUESTA-' + CONVERT(NVARCHAR(40), @IdApuesta);

    INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaCasaJuego, @Importe, 0);
    INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaUsuario, 0, @Importe);

    EXEC dbo.usp_Asiento_CrearSimple
      @IdTipo=@IdTipoAsiento,
      @ReferenciaExterna=@Ref,
      @Movs=@Movs,
      @IdAsiento=@IdAsiento OUTPUT;
  COMMIT;
END
GO



-- Liquidar ronda (payout por multiplicador del detalle: usa MAX como simplificación)
/* =========================
   usp_Ronda_Liquidar (patch)
   ========================= */
CREATE OR ALTER PROCEDURE dbo.usp_Ronda_Liquidar
  @IdRonda BIGINT,
  @IdTipoAsientoPremio SMALLINT,
  @IdCuentaCasaJuego BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;

  IF NOT EXISTS (SELECT 1 FROM dbo.Rondas WHERE IdRonda=@IdRonda)
    THROW 50020, N'Ronda inexistente.', 1;

  DECLARE @t TABLE(
    IdApuesta BIGINT PRIMARY KEY,
    IdUsuario INT,
    IdMoneda  INT,
    Importe   DECIMAL(19,4),
    Payout    DECIMAL(19,4)
  );

  INSERT INTO @t(IdApuesta, IdUsuario, IdMoneda, Importe, Payout)
  SELECT a.IdApuesta, a.IdUsuario, a.IdMoneda, a.Importe,
         a.Importe * COALESCE(MAX(ad.Multiplicador), 0)
  FROM dbo.Apuestas a
  LEFT JOIN dbo.ApuestaDetalle ad ON ad.IdApuesta = a.IdApuesta
  WHERE a.IdRonda=@IdRonda AND a.Estado=N'placed'
  GROUP BY a.IdApuesta, a.IdUsuario, a.IdMoneda, a.Importe;

  DECLARE @IdApuesta BIGINT, @IdUsuario INT, @IdMoneda INT, @Importe DECIMAL(19,4), @Payout DECIMAL(19,4);

  DECLARE cur CURSOR LOCAL FAST_FORWARD FOR
    SELECT IdApuesta, IdUsuario, IdMoneda, Importe, Payout FROM @t;
  OPEN cur;
  FETCH NEXT FROM cur INTO @IdApuesta, @IdUsuario, @IdMoneda, @Importe, @Payout;
  WHILE @@FETCH_STATUS = 0
  BEGIN
    UPDATE dbo.Apuestas
      SET Estado = CASE WHEN @Payout > 0 THEN N'won' ELSE N'lost' END
    WHERE IdApuesta=@IdApuesta;

    IF @Payout > 0
    BEGIN
      DECLARE @IdCuentaUsuario BIGINT;
      SELECT @IdCuentaUsuario = IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;

      DECLARE @Movs dbo.TVP_Movimiento, @IdAsiento BIGINT, @Ref NVARCHAR(100);
      SET @Ref = N'PAGO_APUESTA-' + CONVERT(NVARCHAR(40), @IdApuesta);

      INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaUsuario, @Payout, 0);
      INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaCasaJuego, 0, @Payout);

      EXEC dbo.usp_Asiento_CrearSimple
        @IdTipo=@IdTipoAsientoPremio,
        @ReferenciaExterna=@Ref,
        @Movs=@Movs,
        @IdAsiento=@IdAsiento OUTPUT;
    END

    FETCH NEXT FROM cur INTO @IdApuesta, @IdUsuario, @IdMoneda, @Importe, @Payout;
  END
  CLOSE cur; DEALLOCATE cur;

  EXEC dbo.usp_Ronda_Cerrar @IdRonda=@IdRonda;
END
GO


/* ===========================================================
   CASINO VIRTUAL – SPs núcleo (SQL Server)
   =========================================================== */
SET ANSI_NULLS ON;
SET QUOTED_IDENTIFIER ON;
GO

/* ===========================================================
   0) TIPOS TVP
   =========================================================== */
IF TYPE_ID('dbo.TVP_Movimiento') IS NULL
  EXEC ('CREATE TYPE dbo.TVP_Movimiento AS TABLE(
           IdCuenta BIGINT NOT NULL,
           Debe     DECIMAL(19,4) NOT NULL DEFAULT(0),
           Haber    DECIMAL(19,4) NOT NULL DEFAULT(0)
        )');
GO

IF TYPE_ID('dbo.TVP_ApuestaDetalle') IS NULL
  EXEC ('CREATE TYPE dbo.TVP_ApuestaDetalle AS TABLE(
           Seleccion     NVARCHAR(80) NOT NULL,
           Multiplicador DECIMAL(10,4) NULL
        )');
GO

/* ===========================================================
   1) USUARIOS & SESIONES
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Usuario_Crear
  @Email NVARCHAR(256),
  @HashPwd VARBINARY(MAX),
  @IdUsuario INT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF EXISTS (SELECT 1 FROM dbo.Usuarios WHERE Email=@Email)
    THROW 50050, N'El email ya está registrado.', 1;

  INSERT INTO dbo.Usuarios(Email, HashPwd, Activo, EmailVerificado, FechaAlta)
  VALUES(@Email, @HashPwd, 0, 0, SYSUTCDATETIME());

  SET @IdUsuario = SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Usuario_ActualizarPerfil
  @IdUsuario INT,
  @NuevoEmail NVARCHAR(256) = NULL
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF @NuevoEmail IS NOT NULL AND EXISTS(SELECT 1 FROM dbo.Usuarios WHERE Email=@NuevoEmail AND IdUsuario<>@IdUsuario)
    THROW 50051, N'Email ya utilizado por otro usuario.', 1;

  UPDATE dbo.Usuarios
     SET Email = COALESCE(@NuevoEmail, Email)
   WHERE IdUsuario=@IdUsuario;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Usuario_CambiarPassword
  @IdUsuario INT,
  @HashPwd VARBINARY(MAX)
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Usuarios SET HashPwd=@HashPwd WHERE IdUsuario=@IdUsuario;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Usuario_Bloquear
  @IdUsuario INT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Usuarios SET Activo=0 WHERE IdUsuario=@IdUsuario;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Usuario_Desbloquear
  @IdUsuario INT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Usuarios SET Activo=1 WHERE IdUsuario=@IdUsuario;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Sesion_Iniciar
  @IdUsuario INT,
  @IP NVARCHAR(64)=NULL,
  @UserAgent NVARCHAR(256)=NULL,
  @IdSesion BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  INSERT INTO dbo.Sesiones(IdUsuario, IP, UserAgent, Inicio, Valido)
  VALUES(@IdUsuario, @IP, @UserAgent, SYSUTCDATETIME(), 1);
  SET @IdSesion = SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Sesion_Cerrar
  @IdSesion BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Sesiones SET Fin=SYSUTCDATETIME(), Valido=0 WHERE IdSesion=@IdSesion;
END
GO

/* ===========================================================
   2) VERIFICACIÓN POR E-MAIL (códigos)
   Usa la tabla dbo.VerificationToken del DDL.
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Verificacion_Email_Iniciar
  @UserId INT,
  @Email NVARCHAR(256),
  @Purpose TINYINT = 1,      -- 1 = EmailVerify
  @TTL_Minutes INT = 15,
  @DebugReturnCode BIT = 0,  -- SOLO DEV
  @TokenId UNIQUEIDENTIFIER OUTPUT,
  @Dev_Code NVARCHAR(6) OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  DECLARE @now DATETIME2(3)=SYSUTCDATETIME();

  -- Reusar si hay uno vigente
  SELECT TOP(1) @TokenId = TokenId
  FROM dbo.VerificationToken WITH (UPDLOCK, HOLDLOCK)
  WHERE UserId=@UserId AND Purpose=@Purpose AND Status=0 AND ExpiresAt>@now
  ORDER BY SentAt DESC;

  IF @TokenId IS NULL
  BEGIN
    DECLARE @code NVARCHAR(6);
    DECLARE @rb VARBINARY(4)=CRYPT_GEN_RANDOM(4);
    DECLARE @n INT = ABS(CAST(@rb AS INT)) % 1000000;
    SET @code = RIGHT('000000'+CONVERT(NVARCHAR(6), @n), 6);

    DECLARE @salt VARBINARY(16)=CRYPT_GEN_RANDOM(16);
    DECLARE @hash VARBINARY(32)=HASHBYTES('SHA2_256', @salt + CONVERT(VARBINARY(100), @code));

    DECLARE @Ins TABLE(TokenId UNIQUEIDENTIFIER);
    INSERT INTO dbo.VerificationToken
      (UserId,Purpose,Channel,Target,CodeHash,Salt,ExpiresAt,Status,MaxAttempts,SentAt,CreatedAt)
    OUTPUT inserted.TokenId INTO @Ins
    VALUES
      (@UserId,@Purpose,1,@Email,@hash,@salt,DATEADD(MINUTE,@TTL_Minutes,@now),0,5,@now,@now);

    SELECT @TokenId = TokenId FROM @Ins;

    IF @DebugReturnCode=1 SET @Dev_Code=@code;
  END
  ELSE
  BEGIN
    UPDATE dbo.VerificationToken SET SentAt=@now WHERE TokenId=@TokenId;
    IF @DebugReturnCode=1 SET @Dev_Code=NULL;
  END
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Verificacion_Email_Confirmar
  @UserId INT,
  @Code NVARCHAR(6),
  @Purpose TINYINT = 1,
  @Ok BIT OUTPUT,
  @Mensaje NVARCHAR(200) OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  DECLARE @now DATETIME2(3)=SYSUTCDATETIME();

  DECLARE @TokenId UNIQUEIDENTIFIER, @Salt VARBINARY(16), @Hash VARBINARY(32),
          @Attempts INT, @MaxAttempts TINYINT, @ExpiresAt DATETIME2(3), @Status TINYINT;

  SELECT TOP(1)
    @TokenId=TokenId, @Salt=Salt, @Hash=CodeHash,
    @Attempts=Attempts, @MaxAttempts=MaxAttempts,
    @ExpiresAt=ExpiresAt, @Status=Status
  FROM dbo.VerificationToken WITH (UPDLOCK, HOLDLOCK)
  WHERE UserId=@UserId AND Purpose=@Purpose AND Status=0
  ORDER BY SentAt DESC;

  IF @TokenId IS NULL
  BEGIN SET @Ok=0; SET @Mensaje=N'No hay código pendiente.'; RETURN; END

  IF @ExpiresAt<=@now
  BEGIN UPDATE dbo.VerificationToken SET Status=2 WHERE TokenId=@TokenId;
        SET @Ok=0; SET @Mensaje=N'Código expirado.'; RETURN; END

  IF @Attempts>=@MaxAttempts
  BEGIN UPDATE dbo.VerificationToken SET Status=3 WHERE TokenId=@TokenId;
        SET @Ok=0; SET @Mensaje=N'Demasiados intentos.'; RETURN; END

  DECLARE @calc VARBINARY(32)=HASHBYTES('SHA2_256', @Salt + CONVERT(VARBINARY(16), @Code));
  IF @calc=@Hash
  BEGIN
    UPDATE dbo.VerificationToken SET Status=1, ConsumedAt=@now WHERE TokenId=@TokenId;
    UPDATE dbo.Usuarios SET EmailVerificado=1, Activo=1, FechaActivacion=@now WHERE IdUsuario=@UserId;
    SET @Ok=1; SET @Mensaje=N'Cuenta verificada.';
  END
  ELSE
  BEGIN
    UPDATE dbo.VerificationToken
      SET Attempts = Attempts + 1,
          Status = CASE WHEN Attempts + 1 >= MaxAttempts THEN 3 ELSE Status END
    WHERE TokenId=@TokenId;
    SET @Ok=0; SET @Mensaje=N'Código incorrecto.';
  END
END
GO

/* ===========================================================
   3) CUENTAS & CONTABILIDAD
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Cuenta_Abrir
  @IdUsuario INT,
  @IdMoneda  INT,
  @IdCuenta  BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF EXISTS(SELECT 1 FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda)
  BEGIN
    SELECT @IdCuenta=IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;
    RETURN;
  END
  INSERT INTO dbo.Cuentas(IdUsuario, IdMoneda, SaldoCache, Bloqueada)
  VALUES(@IdUsuario, @IdMoneda, 0, 0);
  SET @IdCuenta = SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Cuenta_Bloquear
  @IdCuenta BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Cuentas SET Bloqueada=1 WHERE IdCuenta=@IdCuenta;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Cuenta_Desbloquear
  @IdCuenta BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Cuentas SET Bloqueada=0 WHERE IdCuenta=@IdCuenta;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Cuenta_ObtenerSaldo
  @IdUsuario INT,
  @IdMoneda  INT
AS
BEGIN
  SET NOCOUNT ON;
  DECLARE @IdCuenta BIGINT;
  SELECT @IdCuenta=IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;

  SELECT
    c.IdCuenta,
    c.SaldoCache,
    (SELECT COALESCE(SUM(Debe - Haber),0) FROM dbo.Movimientos WHERE IdCuenta=c.IdCuenta) AS SaldoEfectivo
  FROM dbo.Cuentas c
  WHERE c.IdCuenta=@IdCuenta;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Billetera_SincronizarSaldoCache
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  ;WITH recal AS (
    SELECT IdCuenta, SUM(Debe - Haber) AS Nuevo
    FROM dbo.Movimientos GROUP BY IdCuenta
  )
  UPDATE c
     SET c.SaldoCache = COALESCE(r.Nuevo,0)
  FROM dbo.Cuentas c
  LEFT JOIN recal r ON r.IdCuenta=c.IdCuenta;
END
GO

/* Asiento contable genérico (valida ∑Debe=∑Haber y ajusta SaldoCache) */
CREATE OR ALTER PROCEDURE dbo.usp_Asiento_CrearSimple
  @IdTipo SMALLINT,
  @ReferenciaExterna NVARCHAR(100)=NULL,
  @Movs dbo.TVP_Movimiento READONLY,
  @IdAsiento BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;

  DECLARE @sumDebe DECIMAL(19,4), @sumHaber DECIMAL(19,4);
  SELECT @sumDebe=COALESCE(SUM(Debe),0), @sumHaber=COALESCE(SUM(Haber),0) FROM @Movs;
  IF @sumDebe<>@sumHaber THROW 50001, N'El asiento no cuadra.', 1;

  IF EXISTS (
    SELECT 1 FROM @Movs m
    LEFT JOIN dbo.Cuentas c ON c.IdCuenta=m.IdCuenta
    WHERE c.IdCuenta IS NULL
  ) THROW 50002, N'Cuenta inexistente en movimientos.', 1;

  BEGIN TRAN;
    INSERT INTO dbo.Asientos(IdTipo, Fecha, ReferenciaExterna)
    VALUES(@IdTipo, SYSUTCDATETIME(), @ReferenciaExterna);
    SET @IdAsiento = SCOPE_IDENTITY();

    INSERT INTO dbo.Movimientos(IdAsiento, IdCuenta, Debe, Haber, Ts)
    SELECT @IdAsiento, IdCuenta, Debe, Haber, SYSUTCDATETIME() FROM @Movs;

    ;WITH d AS (
      SELECT IdCuenta, SUM(Debe-Haber) Delta FROM @Movs GROUP BY IdCuenta
    )
    UPDATE c SET c.SaldoCache = c.SaldoCache + d.Delta
    FROM dbo.Cuentas c JOIN d ON d.IdCuenta=c.IdCuenta;
  COMMIT;
END
GO

/* Listado simple de movimientos por cuenta */
CREATE OR ALTER PROCEDURE dbo.usp_Movimiento_ListarPorCuenta
  @IdCuenta BIGINT,
  @Desde DATETIME2(3)=NULL,
  @Hasta DATETIME2(3)=NULL,
  @TopN INT = 200
AS
BEGIN
  SET NOCOUNT ON;
  SELECT TOP(@TopN) m.IdMovimiento, m.IdAsiento, m.Debe, m.Haber, m.Ts
  FROM dbo.Movimientos m
  WHERE m.IdCuenta=@IdCuenta
    AND (@Desde IS NULL OR m.Ts>=@Desde)
    AND (@Hasta IS NULL OR m.Ts<@Hasta)
  ORDER BY m.Ts DESC;
END
GO

/* ===========================================================
   4) JUEGOS / MESAS / RONDAS / RESULTADOS
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_ProveedorJuego_Crear
  @Nombre NVARCHAR(120),
  @IdProveedor INT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF EXISTS(SELECT 1 FROM dbo.ProveedoresJuego WHERE Nombre=@Nombre)
  BEGIN SELECT @IdProveedor=IdProveedor FROM dbo.ProveedoresJuego WHERE Nombre=@Nombre; RETURN; END
  INSERT INTO dbo.ProveedoresJuego(Nombre) VALUES(@Nombre);
  SET @IdProveedor = SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Juego_Crear
  @IdProveedor INT,
  @Tipo NVARCHAR(40),
  @Nombre NVARCHAR(120),
  @RtpTeorico DECIMAL(5,2)=NULL,
  @IdJuego INT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  INSERT INTO dbo.Juegos(IdProveedor,Tipo,Nombre,RtpTeorico)
  VALUES(@IdProveedor,@Tipo,@Nombre,@RtpTeorico);
  SET @IdJuego = SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Mesa_Abrir
  @IdJuego INT,
  @Nombre NVARCHAR(80),
  @IdMesa INT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  INSERT INTO dbo.Mesas(IdJuego,Nombre,Estado) VALUES(@IdJuego,@Nombre,N'open');
  SET @IdMesa = SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Mesa_Cerrar
  @IdMesa INT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Mesas SET Estado=N'closed' WHERE IdMesa=@IdMesa;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Ronda_Abrir
  @IdJuego INT,
  @IdMesa  INT = NULL,
  @IdRonda BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  INSERT INTO dbo.Rondas(IdJuego, IdMesa, Inicio, Estado)
  VALUES(@IdJuego, @IdMesa, SYSUTCDATETIME(), N'open');
  SET @IdRonda = SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Ronda_Cerrar
  @IdRonda BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Rondas SET Fin=SYSUTCDATETIME(), Estado=N'closed'
  WHERE IdRonda=@IdRonda AND Estado<>N'closed';
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Ronda_RegistrarResultado
  @IdRonda BIGINT,
  @Payload NVARCHAR(MAX),
  @Checksum VARBINARY(32)=NULL
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  MERGE dbo.ResultadosRonda AS tgt
  USING (SELECT @IdRonda AS IdRonda) AS src
  ON tgt.IdRonda=src.IdRonda
  WHEN MATCHED THEN UPDATE SET PayloadResultado=@Payload, Checksum=@Checksum, Ts=SYSUTCDATETIME()
  WHEN NOT MATCHED THEN INSERT(IdRonda,PayloadResultado,Checksum,Ts)
                      VALUES(@IdRonda,@Payload,@Checksum,SYSUTCDATETIME());
END
GO

/* ===========================================================
   5) APUESTAS (colocar / cancelar / listar) y LIQUIDACIÓN
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Apuesta_Colocar
  @IdUsuario INT,
  @IdRonda   BIGINT,
  @IdMoneda  INT,
  @Importe   DECIMAL(19,4),
  @IdTipoAsiento SMALLINT,
  @IdCuentaCasaJuego BIGINT,
  @Detalle dbo.TVP_ApuestaDetalle READONLY,
  @IdApuesta BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;

  IF @Importe <= 0 THROW 50010, N'Importe inválido.', 1;

  DECLARE @estado NVARCHAR(20);
  SELECT @estado = Estado FROM dbo.Rondas WHERE IdRonda=@IdRonda;
  IF @estado IS NULL OR @estado <> N'open' THROW 50011, N'Ronda no abierta.', 1;

  DECLARE @IdCuentaUsuario BIGINT;
  SELECT @IdCuentaUsuario = IdCuenta
  FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;
  IF @IdCuentaUsuario IS NULL THROW 50012, N'El usuario no tiene cuenta para esa moneda.', 1;

  DECLARE @saldo DECIMAL(19,4);
  SELECT @saldo = SaldoCache FROM dbo.Cuentas WITH (UPDLOCK, ROWLOCK) WHERE IdCuenta=@IdCuentaUsuario;
  IF @saldo < @Importe THROW 50013, N'Saldo insuficiente.', 1;

  BEGIN TRAN;
    INSERT INTO dbo.Apuestas(IdUsuario, IdRonda, IdMoneda, Importe, Estado, Ts)
    VALUES(@IdUsuario, @IdRonda, @IdMoneda, @Importe, N'placed', SYSUTCDATETIME());
    SET @IdApuesta = SCOPE_IDENTITY();

    INSERT INTO dbo.ApuestaDetalle(IdApuesta, Seleccion, Multiplicador)
    SELECT @IdApuesta, Seleccion, Multiplicador FROM @Detalle;

    DECLARE @Movs dbo.TVP_Movimiento, @IdAsiento BIGINT, @Ref NVARCHAR(100);
    SET @Ref = N'APUESTA:' + CAST(@IdApuesta AS NVARCHAR(40));

    INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaCasaJuego, @Importe, 0);
    INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaUsuario, 0, @Importe);

    EXEC dbo.usp_Asiento_CrearSimple
      @IdTipo=@IdTipoAsiento,
      @ReferenciaExterna=@Ref,
      @Movs=@Movs,
      @IdAsiento=@IdAsiento OUTPUT;
  COMMIT;
END
GO

/* Reembolso/cancelación de apuesta (si la ronda fue anulada antes de liquidar) */
CREATE OR ALTER PROCEDURE dbo.usp_Apuesta_Cancelar
  @IdApuesta BIGINT,
  @IdTipoAsiento SMALLINT,
  @IdCuentaCasaJuego BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;

  DECLARE @Estado NVARCHAR(20), @IdUsuario INT, @IdMoneda INT, @Importe DECIMAL(19,4);
  SELECT @Estado=Estado, @IdUsuario=IdUsuario, @IdMoneda=IdMoneda, @Importe=Importe
  FROM dbo.Apuestas WHERE IdApuesta=@IdApuesta;

  IF @Estado IS NULL THROW 50060, N'Apuesta inexistente.', 1;
  IF @Estado<>N'placed' THROW 50061, N'La apuesta no puede cancelarse (estado actual).', 1;

  DECLARE @IdCuentaUsuario BIGINT;
  SELECT @IdCuentaUsuario=IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;

  DECLARE @Movs dbo.TVP_Movimiento, @IdAsiento BIGINT, @Ref NVARCHAR(100);
  SET @Ref = N'APUESTA_CANCELADA-' + CONVERT(NVARCHAR(40), @IdApuesta);

  INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaUsuario, @Importe, 0);
  INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaCasaJuego, 0, @Importe);

  EXEC dbo.usp_Asiento_CrearSimple
    @IdTipo=@IdTipoAsiento,
    @ReferenciaExterna=@Ref,
    @Movs=@Movs,
    @IdAsiento=@IdAsiento OUTPUT;

  UPDATE dbo.Apuestas SET Estado=N'cancelled' WHERE IdApuesta=@IdApuesta;
END
GO


/* Liquidación simplificada de ronda:
   payout = Importe * MAX(Multiplicador) por apuesta */
CREATE OR ALTER PROCEDURE dbo.usp_Ronda_Liquidar
  @IdRonda BIGINT,
  @IdTipoAsientoPremio SMALLINT,
  @IdCuentaCasaJuego BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;

  IF NOT EXISTS (SELECT 1 FROM dbo.Rondas WHERE IdRonda=@IdRonda)
    THROW 50020, N'Ronda inexistente.', 1;

  DECLARE @t TABLE(
    IdApuesta BIGINT PRIMARY KEY,
    IdUsuario INT,
    IdMoneda  INT,
    Importe   DECIMAL(19,4),
    Payout    DECIMAL(19,4)
  );

  INSERT INTO @t(IdApuesta, IdUsuario, IdMoneda, Importe, Payout)
  SELECT a.IdApuesta, a.IdUsuario, a.IdMoneda, a.Importe,
         a.Importe * COALESCE(MAX(ad.Multiplicador), 0)
  FROM dbo.Apuestas a
  LEFT JOIN dbo.ApuestaDetalle ad ON ad.IdApuesta = a.IdApuesta
  WHERE a.IdRonda=@IdRonda AND a.Estado=N'placed'
  GROUP BY a.IdApuesta, a.IdUsuario, a.IdMoneda, a.Importe;

  DECLARE @IdApuesta BIGINT, @IdUsuario INT, @IdMoneda INT, @Importe DECIMAL(19,4), @Payout DECIMAL(19,4);

  DECLARE cur CURSOR LOCAL FAST_FORWARD FOR
    SELECT IdApuesta, IdUsuario, IdMoneda, Importe, Payout FROM @t;
  OPEN cur;
  FETCH NEXT FROM cur INTO @IdApuesta, @IdUsuario, @IdMoneda, @Importe, @Payout;
  WHILE @@FETCH_STATUS = 0
  BEGIN
    UPDATE dbo.Apuestas
      SET Estado = CASE WHEN @Payout > 0 THEN N'won' ELSE N'lost' END
    WHERE IdApuesta=@IdApuesta;

    IF @Payout > 0
    BEGIN
      DECLARE @IdCuentaUsuario BIGINT;
      SELECT @IdCuentaUsuario = IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;

      DECLARE @Movs dbo.TVP_Movimiento, @IdAsiento BIGINT, @Ref NVARCHAR(100);
      SET @Ref = N'PAGO_APUESTA:' + CAST(@IdApuesta AS NVARCHAR(40));

      INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaUsuario, @Payout, 0);
      INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaCasaJuego, 0, @Payout);

      EXEC dbo.usp_Asiento_CrearSimple
        @IdTipo=@IdTipoAsientoPremio,
        @ReferenciaExterna=@Ref,
        @Movs=@Movs,
        @IdAsiento=@IdAsiento OUTPUT;
    END

    FETCH NEXT FROM cur INTO @IdApuesta, @IdUsuario, @IdMoneda, @Importe, @Payout;
  END
  CLOSE cur; DEALLOCATE cur;

  EXEC dbo.usp_Ronda_Cerrar @IdRonda=@IdRonda;
END
GO

/* ===========================================================
   6) PAGOS: DEPÓSITOS & RETIROS
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Deposito_Iniciar
  @IdUsuario INT,
  @IdMetodo  INT,
  @IdMoneda  INT,
  @Monto     DECIMAL(19,4),
  @IdDeposito BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF @Monto<=0 THROW 50030, N'Monto inválido.', 1;

  INSERT INTO dbo.Depositos(IdUsuario,IdMetodo,IdMoneda,Monto,Estado,Ts)
  VALUES(@IdUsuario,@IdMetodo,@IdMoneda,@Monto,N'pending',SYSUTCDATETIME());

  SET @IdDeposito=SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Deposito_Confirmar
  @IdDeposito BIGINT = NULL,
  @ExternalId NVARCHAR(100) = NULL,
  @Monto      DECIMAL(19,4) = NULL,
  @IdTipoAsiento SMALLINT,
  @IdCuentaCasaCaja BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;

  DECLARE @IdDep BIGINT;
  IF @IdDeposito IS NOT NULL SET @IdDep = @IdDeposito;
  ELSE IF @ExternalId IS NOT NULL SELECT @IdDep = IdDeposito FROM dbo.Depositos WHERE ExternalId=@ExternalId;
  ELSE THROW 50031, N'Debe indicar @IdDeposito o @ExternalId.', 1;

  DECLARE @estado NVARCHAR(20), @IdUsuario INT, @IdMoneda INT, @m DECIMAL(19,4);
  SELECT @estado=Estado, @IdUsuario=IdUsuario, @IdMoneda=IdMoneda, @m=Monto
  FROM dbo.Depositos WHERE IdDeposito=@IdDep;

  IF @estado IS NULL THROW 50032, N'Depósito no existe.', 1;
  IF @estado = N'confirmed' RETURN;
  IF @estado <> N'pending' THROW 50033, N'Estado no válido.', 1;
  IF @Monto IS NOT NULL AND @Monto <> @m THROW 50034, N'Monto no coincide.', 1;

  DECLARE @IdCuentaUsuario BIGINT;
  SELECT @IdCuentaUsuario = IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;
  IF @IdCuentaUsuario IS NULL THROW 50035, N'Cuenta de usuario no existe.', 1;

  DECLARE @Movs dbo.TVP_Movimiento, @IdAsiento BIGINT, @Ref NVARCHAR(100);
  SET @Ref = N'DEPOSITO-' + CONVERT(NVARCHAR(40), @IdDep);

  INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaUsuario, @m, 0);
  INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaCasaCaja, 0, @m);

  EXEC dbo.usp_Asiento_CrearSimple
    @IdTipo=@IdTipoAsiento,
    @ReferenciaExterna=@Ref,
    @Movs=@Movs,
    @IdAsiento=@IdAsiento OUTPUT;

  UPDATE dbo.Depositos
    SET Estado=N'confirmed',
        Ts = SYSUTCDATETIME(),
        ExternalId = COALESCE(@ExternalId, ExternalId)
  WHERE IdDeposito=@IdDep;
END
GO


CREATE OR ALTER PROCEDURE dbo.usp_Deposito_Rechazar
  @IdDeposito BIGINT,
  @Motivo NVARCHAR(200)=NULL
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Depositos SET Estado=N'rejected', Ts=SYSUTCDATETIME()
  WHERE IdDeposito=@IdDeposito AND Estado=N'pending';
  -- Motivo puede registrarse en Auditorias/Logs si deseas
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Retiro_Solicitar
  @IdUsuario INT,
  @IdMetodo  INT,
  @IdMoneda  INT,
  @Monto     DECIMAL(19,4),
  @IdTipoAsientoRetencion SMALLINT,
  @IdCuentaCasaRetencion BIGINT,
  @IdRetiro BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;

  IF @Monto <= 0 THROW 50040, N'Monto inválido.', 1;

  DECLARE @IdCuentaUsuario BIGINT, @saldo DECIMAL(19,4);
  SELECT @IdCuentaUsuario = IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;
  IF @IdCuentaUsuario IS NULL THROW 50041, N'Cuenta del usuario no existe.', 1;

  SELECT @saldo = SaldoCache FROM dbo.Cuentas WITH (UPDLOCK, ROWLOCK) WHERE IdCuenta=@IdCuentaUsuario;
  IF @saldo < @Monto THROW 50042, N'Saldo insuficiente.', 1;

  BEGIN TRAN;
    INSERT INTO dbo.Retiros(IdUsuario, IdMetodo, IdMoneda, Monto, Estado, Ts)
    VALUES(@IdUsuario, @IdMetodo, @IdMoneda, @Monto, N'requested', SYSUTCDATETIME());
    SET @IdRetiro = SCOPE_IDENTITY();

    DECLARE @Movs dbo.TVP_Movimiento, @IdAsiento BIGINT, @Ref NVARCHAR(100);
    SET @Ref = N'RETIRO_REQ-' + CONVERT(NVARCHAR(40), @IdRetiro);

    INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaCasaRetencion, @Monto, 0);
    INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaUsuario, 0, @Monto);

    EXEC dbo.usp_Asiento_CrearSimple
      @IdTipo=@IdTipoAsientoRetencion,
      @ReferenciaExterna=@Ref,
      @Movs=@Movs,
      @IdAsiento=@IdAsiento OUTPUT;
  COMMIT;
END
GO


CREATE OR ALTER PROCEDURE dbo.usp_Retiro_Aprobar
  @IdRetiro BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  DECLARE @estado NVARCHAR(20);
  SELECT @estado=Estado FROM dbo.Retiros WHERE IdRetiro=@IdRetiro;
  IF @estado IS NULL THROW 50043, N'Retiro no existe.', 1;
  IF @estado NOT IN (N'requested',N'approved') THROW 50044, N'Estado inválido.', 1;

  UPDATE dbo.Retiros SET Estado=N'paid', Ts=SYSUTCDATETIME() WHERE IdRetiro=@IdRetiro;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Retiro_Rechazar
  @IdRetiro BIGINT,
  @IdTipoAsientoReversa SMALLINT,
  @IdCuentaCasaRetencion BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;

  DECLARE @estado NVARCHAR(20), @IdUsuario INT, @IdMoneda INT, @Monto DECIMAL(19,4);
  SELECT @estado=Estado, @IdUsuario=IdUsuario, @IdMoneda=IdMoneda, @Monto=Monto
  FROM dbo.Retiros WHERE IdRetiro=@IdRetiro;

  IF @estado IS NULL THROW 50045, N'Retiro inexistente.', 1;
  IF @estado<>N'requested' THROW 50046, N'Solo retiros solicitados pueden rechazarse.', 1;

  DECLARE @IdCuentaUsuario BIGINT;
  SELECT @IdCuentaUsuario=IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda;

  DECLARE @Movs dbo.TVP_Movimiento, @IdAsiento BIGINT, @Ref NVARCHAR(120);
  SET @Ref = N'RETIRO_RECHAZADO-' + CONVERT(NVARCHAR(40), @IdRetiro);

  INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaUsuario, @Monto, 0);
  INSERT INTO @Movs(IdCuenta, Debe, Haber) VALUES(@IdCuentaCasaRetencion, 0, @Monto);

  EXEC dbo.usp_Asiento_CrearSimple
    @IdTipo=@IdTipoAsientoReversa,
    @ReferenciaExterna=@Ref,
    @Movs=@Movs,
    @IdAsiento=@IdAsiento OUTPUT;

  UPDATE dbo.Retiros SET Estado=N'rejected', Ts=SYSUTCDATETIME() WHERE IdRetiro=@IdRetiro;
END
GO


/* ===========================================================
   7) SEED BÁSICO DEL SISTEMA (CASA, tipos, cuentas)
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Sistema_SeedBasico
  @EmailCasa NVARCHAR(256) = N'house@casino.local',
  @IdUsuarioCasa INT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;

  /* Tipos de transacción mínimos */
  DECLARE @tipos TABLE(N NVARCHAR(80));
  INSERT INTO @tipos(N) VALUES(N'deposito'),(N'apuesta'),(N'premio'),(N'retencion'),(N'liberar_retencion');

  MERGE dbo.TiposTransaccion AS t
  USING (SELECT N FROM @tipos) s ON t.Nombre=s.N
  WHEN NOT MATCHED THEN INSERT(Nombre) VALUES(s.N);

  /* Usuario CASA */
  IF NOT EXISTS(SELECT 1 FROM dbo.Usuarios WHERE Email=@EmailCasa)
  BEGIN
    INSERT INTO dbo.Usuarios(Email,HashPwd,Activo,EmailVerificado,FechaAlta)
    VALUES(@EmailCasa, 0x01, 1, 1, SYSUTCDATETIME());
  END
  SELECT @IdUsuarioCasa=IdUsuario FROM dbo.Usuarios WHERE Email=@EmailCasa;

  /* Crear cuentas de CASA para cada moneda disponible */
  INSERT INTO dbo.Cuentas(IdUsuario,IdMoneda,SaldoCache,Bloqueada)
  SELECT @IdUsuarioCasa, m.IdMoneda, 0, 0
  FROM dbo.Monedas m
  WHERE NOT EXISTS (
    SELECT 1 FROM dbo.Cuentas c
    WHERE c.IdUsuario=@IdUsuarioCasa AND c.IdMoneda=m.IdMoneda
  );
END
GO


---------------------------------------
/* ===========================================================
   CRYPTO-ONLY MODE – Ajustes e Inserts
   =========================================================== */
SET NOCOUNT ON;
SET XACT_ABORT ON;

/* ===========================================================
   (A) OPCIONAL: Parche de precisión a 8 decimales
   (recomendado para operar con cripto)
   =========================================================== */
------------------------------
-- Movimientos.Debe/Haber -> DECIMAL(38,8)
------------------------------
DECLARE @dfDebe sysname, @dfHaber sysname;
SELECT @dfDebe  = dc.name
FROM sys.default_constraints dc
JOIN sys.columns c ON c.object_id = dc.parent_object_id AND c.column_id = dc.parent_column_id
WHERE dc.parent_object_id = OBJECT_ID('dbo.Movimientos') AND c.name = 'Debe';
IF @dfDebe IS NOT NULL EXEC('ALTER TABLE dbo.Movimientos DROP CONSTRAINT ' + QUOTENAME(@dfDebe));

SELECT @dfHaber = dc.name
FROM sys.default_constraints dc
JOIN sys.columns c ON c.object_id = dc.parent_object_id AND c.column_id = dc.parent_column_id
WHERE dc.parent_object_id = OBJECT_ID('dbo.Movimientos') AND c.name = 'Haber';
IF @dfHaber IS NOT NULL EXEC('ALTER TABLE dbo.Movimientos DROP CONSTRAINT ' + QUOTENAME(@dfHaber));

ALTER TABLE dbo.Movimientos ALTER COLUMN Debe  DECIMAL(38,8) NOT NULL;
ALTER TABLE dbo.Movimientos ALTER COLUMN Haber DECIMAL(38,8) NOT NULL;

ALTER TABLE dbo.Movimientos ADD CONSTRAINT DF_Mov_Debe  DEFAULT(0) FOR Debe;
ALTER TABLE dbo.Movimientos ADD CONSTRAINT DF_Mov_Haber DEFAULT(0) FOR Haber;

------------------------------
-- Cuentas.SaldoCache -> DECIMAL(38,8)
------------------------------
ALTER TABLE dbo.Cuentas ALTER COLUMN SaldoCache DECIMAL(38,8) NOT NULL;

------------------------------
-- Apuestas.Importe, Depositos.Monto, Retiros.Monto -> DECIMAL(38,8)
------------------------------
ALTER TABLE dbo.Apuestas  ALTER COLUMN Importe DECIMAL(38,8) NOT NULL;
ALTER TABLE dbo.Depositos ALTER COLUMN Monto   DECIMAL(38,8) NOT NULL;
ALTER TABLE dbo.Retiros   ALTER COLUMN Monto   DECIMAL(38,8) NOT NULL;

-- (Opcional) Ajusta también: JackpotContribuciones.Monto / JackpotPagos.Monto / ConversionesBono.MontoConvertido si los usarás con cripto.
/*
ALTER TABLE dbo.JackpotContribuciones ALTER COLUMN Monto DECIMAL(38,8) NOT NULL;
ALTER TABLE dbo.JackpotPagos         ALTER COLUMN Monto DECIMAL(38,8) NOT NULL;
ALTER TABLE dbo.ConversionesBono     ALTER COLUMN MontoConvertido DECIMAL(38,8) NOT NULL;
*/

/* ===========================================================
   (B) Seeds CRYPTO-ONLY (BTC / ETH / LTC) + Proveedores/Metodos
   =========================================================== */
-- Monedas cripto (usa 8 decimales)
IF NOT EXISTS (SELECT 1 FROM dbo.Monedas WHERE CodigoISO='BTC')
  INSERT INTO dbo.Monedas(CodigoISO,Decimales) VALUES ('BTC',8);
IF NOT EXISTS (SELECT 1 FROM dbo.Monedas WHERE CodigoISO='ETH')
  INSERT INTO dbo.Monedas(CodigoISO,Decimales) VALUES ('ETH',8);  -- simplificado a 8
IF NOT EXISTS (SELECT 1 FROM dbo.Monedas WHERE CodigoISO='LTC')
  INSERT INTO dbo.Monedas(CodigoISO,Decimales) VALUES ('LTC',8);

-- (Opcional) Desactivar/ignorar fiat existentes en la app.
-- Si no quieres borrar, basta con no ofrecerlos en UI.
-- Si deseas borrarlos y no tienen datos relacionados, puedes hacerlo manualmente.

-- Proveedores de pago cripto
IF NOT EXISTS (SELECT 1 FROM dbo.ProveedoresPago WHERE Nombre=N'CryptoGateway')
  INSERT INTO dbo.ProveedoresPago(Nombre,Tipo) VALUES (N'CryptoGateway',N'crypto');
IF NOT EXISTS (SELECT 1 FROM dbo.ProveedoresPago WHERE Nombre=N'HotWallet')
  INSERT INTO dbo.ProveedoresPago(Nombre,Tipo) VALUES (N'HotWallet',N'crypto');

DECLARE @ProvCrypto INT = (SELECT IdProveedorPago FROM dbo.ProveedoresPago WHERE Nombre=N'CryptoGateway');
DECLARE @ProvHot    INT = (SELECT IdProveedorPago FROM dbo.ProveedoresPago WHERE Nombre=N'HotWallet');

-- Métodos de pago (config JSON de ejemplo)
IF NOT EXISTS (SELECT 1 FROM dbo.MetodosPago WHERE IdProveedorPago=@ProvCrypto)
  INSERT INTO dbo.MetodosPago(IdProveedorPago,Config)
  VALUES(@ProvCrypto,N'{"networks":["BTC","ETH","LTC"],"confirmations":{"BTC":2,"ETH":12,"LTC":6}}');

IF NOT EXISTS (SELECT 1 FROM dbo.MetodosPago WHERE IdProveedorPago=@ProvHot)
  INSERT INTO dbo.MetodosPago(IdProveedorPago,Config)
  VALUES(@ProvHot,N'{"hotWallet":"on","withdraw_policy":"manual_approval"}');

-- Proveedores de juegos (si no existen)
IF NOT EXISTS (SELECT 1 FROM dbo.ProveedoresJuego WHERE Nombre=N'Evolution')
  INSERT INTO dbo.ProveedoresJuego(Nombre) VALUES (N'Evolution');
IF NOT EXISTS (SELECT 1 FROM dbo.ProveedoresJuego WHERE Nombre=N'Pragmatic Play')
  INSERT INTO dbo.ProveedoresJuego(Nombre) VALUES (N'Pragmatic Play');

-- Juegos/mesas mínimos
IF NOT EXISTS (SELECT 1 FROM dbo.Juegos WHERE Nombre=N'Ruleta Live')
  INSERT INTO dbo.Juegos(IdProveedor,Tipo,Nombre,RtpTeorico)
  VALUES((SELECT IdProveedor FROM dbo.ProveedoresJuego WHERE Nombre=N'Evolution'),N'live',N'Ruleta Live',NULL);

IF NOT EXISTS (SELECT 1 FROM dbo.Mesas WHERE Nombre=N'Mesa Crypto 1')
  INSERT INTO dbo.Mesas(IdJuego,Nombre,Estado)
  VALUES((SELECT IdJuego FROM dbo.Juegos WHERE Nombre=N'Ruleta Live'),N'Mesa Crypto 1',N'open');

/* ===========================================================
   (C) CASA + usuarios demo + cuentas cripto
   =========================================================== */
DECLARE @IdCasa INT;
EXEC dbo.usp_Sistema_SeedBasico @IdUsuarioCasa = @IdCasa OUTPUT; -- crea CASA y abre cuentas por cada moneda (ahora BTC/ETH/LTC)

-- Usuarios demo
IF NOT EXISTS (SELECT 1 FROM dbo.Usuarios WHERE Email=N'alice@crypto.local')
  INSERT INTO dbo.Usuarios(Email,HashPwd,Activo,EmailVerificado,FechaAlta,FechaActivacion)
  VALUES (N'alice@crypto.local',0x01,1,1,SYSUTCDATETIME(),SYSUTCDATETIME());

DECLARE @IdAlice INT = (SELECT IdUsuario FROM dbo.Usuarios WHERE Email=N'alice@crypto.local');

-- Abre cuentas cripto para Alice
DECLARE @IdBTC INT = (SELECT IdMoneda FROM dbo.Monedas WHERE CodigoISO='BTC');
DECLARE @IdETH INT = (SELECT IdMoneda FROM dbo.Monedas WHERE CodigoISO='ETH');
DECLARE @IdLTC INT = (SELECT IdMoneda FROM dbo.Monedas WHERE CodigoISO='LTC');

DECLARE @CtaAliceBTC BIGINT, @CtaAliceETH BIGINT, @CtaAliceLTC BIGINT;
EXEC dbo.usp_Cuenta_Abrir @IdUsuario=@IdAlice, @IdMoneda=@IdBTC, @IdCuenta=@CtaAliceBTC OUTPUT;
EXEC dbo.usp_Cuenta_Abrir @IdUsuario=@IdAlice, @IdMoneda=@IdETH, @IdCuenta=@CtaAliceETH OUTPUT;
EXEC dbo.usp_Cuenta_Abrir @IdUsuario=@IdAlice, @IdMoneda=@IdLTC, @IdCuenta=@CtaAliceLTC OUTPUT;

-- Cuentas de CASA por moneda
DECLARE @CtaCasaBTC BIGINT = (SELECT IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdCasa AND IdMoneda=@IdBTC);
DECLARE @CtaCasaETH BIGINT = (SELECT IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdCasa AND IdMoneda=@IdETH);
DECLARE @CtaCasaLTC BIGINT = (SELECT IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdCasa AND IdMoneda=@IdLTC);

/* ===========================================================
   (D) Flujo demo 100% cripto: Depósito BTC -> Apuesta -> Liquidación -> Retiro BTC
   =========================================================== */
-- Tipos
DECLARE @TT_Deposito  SMALLINT = (SELECT IdTipo FROM dbo.TiposTransaccion WHERE Nombre=N'deposito');
DECLARE @TT_Apuesta   SMALLINT = (SELECT IdTipo FROM dbo.TiposTransaccion WHERE Nombre=N'apuesta');
DECLARE @TT_Premio    SMALLINT = (SELECT IdTipo FROM dbo.TiposTransaccion WHERE Nombre=N'premio');
DECLARE @TT_Retencion SMALLINT = (SELECT IdTipo FROM dbo.TiposTransaccion WHERE Nombre=N'retencion');

-- Método cripto
DECLARE @MetodoCrypto INT = (SELECT TOP(1) IdMetodo FROM dbo.MetodosPago WHERE IdProveedorPago=@ProvCrypto);

-- 1) Depósito BTC de 0.01 con tx hash en ExternalId
DECLARE @IdDep BIGINT;
EXEC dbo.usp_Deposito_Iniciar
  @IdUsuario=@IdAlice, @IdMetodo=@MetodoCrypto, @IdMoneda=@IdBTC, @Monto=0.01, @IdDeposito=@IdDep OUTPUT;

-- Confirma: usa cuenta de CASA BTC como contrapartida, y tx hash como ExternalId
EXEC dbo.usp_Deposito_Confirmar
  @IdDeposito=@IdDep,
  @ExternalId=N'btc:4b8f1c...txhash_demo',  -- guarda el txhash aquí
  @Monto=0.01,
  @IdTipoAsiento=@TT_Deposito,
  @IdCuentaCasaCaja=@CtaCasaBTC;

-- 2) Abrir ronda y apostar 0.001 BTC
DECLARE @IdRonda BIGINT;
EXEC dbo.usp_Ronda_Abrir
  @IdJuego=(SELECT IdJuego FROM dbo.Juegos WHERE Nombre=N'Ruleta Live'),
  @IdMesa =(SELECT IdMesa  FROM dbo.Mesas  WHERE Nombre=N'Mesa Crypto 1'),
  @IdRonda=@IdRonda OUTPUT;

DECLARE @det dbo.TVP_ApuestaDetalle;
INSERT INTO @det(Seleccion,Multiplicador) VALUES (N'rojo',2.0);

DECLARE @IdApuesta BIGINT;
EXEC dbo.usp_Apuesta_Colocar
  @IdUsuario=@IdAlice, @IdRonda=@IdRonda, @IdMoneda=@IdBTC,
  @Importe=0.001, @IdTipoAsiento=@TT_Apuesta, @IdCuentaCasaJuego=@CtaCasaBTC,
  @Detalle=@det, @IdApuesta=@IdApuesta OUTPUT;

-- 3) Resultado (opcional payload) + liquidación (si ganó, pagará 0.002)
EXEC dbo.usp_Ronda_RegistrarResultado @IdRonda=@IdRonda, @Payload=N'{"spin":"R12","color":"rojo"}';
EXEC dbo.usp_Ronda_Liquidar @IdRonda=@IdRonda, @IdTipoAsientoPremio=@TT_Premio, @IdCuentaCasaJuego=@CtaCasaBTC;

-- 4) Retiro BTC: solicita 0.005 BTC (cuenta Retención = CASA BTC)
DECLARE @IdRetiro BIGINT;
EXEC dbo.usp_Retiro_Solicitar
  @IdUsuario=@IdAlice, @IdMetodo=@MetodoCrypto, @IdMoneda=@IdBTC, @Monto=0.005,
  @IdTipoAsientoRetencion=@TT_Retencion, @IdCuentaCasaRetencion=@CtaCasaBTC,
  @IdRetiro=@IdRetiro OUTPUT;

-- (Aquí tu sistema emite la transacción on-chain desde tu hot wallet)
-- Cuando confirmes on-chain, marca pagado:
EXEC dbo.usp_Retiro_Aprobar @IdRetiro=@IdRetiro;

SELECT 'CRYPTO SEED OK' AS Status;

