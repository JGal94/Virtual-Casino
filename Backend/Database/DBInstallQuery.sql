USE Casino

GO

/* ===========================================================
   CASINO VIRTUAL � INSTALL (SQL Server)
   Esquema + TVPs + SPs + On-chain + Seeds + Auditor�a
   =========================================================== */
SET NOCOUNT ON;
SET XACT_ABORT ON;
GO

/* ===========================================================
   0) ESQUEMA (Tablas b�sicas)
   =========================================================== */
IF OBJECT_ID('dbo.Usuarios','U') IS NULL
BEGIN
  CREATE TABLE dbo.Usuarios(
    IdUsuario INT IDENTITY(1,1) PRIMARY KEY,
    Email NVARCHAR(256) NOT NULL UNIQUE,
    HashPwd VARBINARY(MAX) NULL,
    Activo BIT NOT NULL DEFAULT(0),
    EmailVerificado BIT NOT NULL DEFAULT(0),
    FechaAlta DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
    FechaActivacion DATETIME2(3) NULL
  );
END
GO

IF OBJECT_ID('dbo.Monedas','U') IS NULL
BEGIN
  CREATE TABLE dbo.Monedas(
    IdMoneda INT IDENTITY(1,1) PRIMARY KEY,
    CodigoISO NVARCHAR(10) NOT NULL UNIQUE,
    Decimales TINYINT NOT NULL
  );
END
GO

IF OBJECT_ID('dbo.TiposTransaccion','U') IS NULL
BEGIN
  CREATE TABLE dbo.TiposTransaccion(
    IdTipo SMALLINT IDENTITY(1,1) PRIMARY KEY,
    Nombre NVARCHAR(80) NOT NULL UNIQUE
  );
END
GO

IF OBJECT_ID('dbo.Cuentas','U') IS NULL
BEGIN
  CREATE TABLE dbo.Cuentas(
    IdCuenta BIGINT IDENTITY(1,1) PRIMARY KEY,
    IdUsuario INT NOT NULL,
    IdMoneda  INT NOT NULL,
    SaldoCache DECIMAL(38,8) NOT NULL DEFAULT(0),
    Bloqueada BIT NOT NULL DEFAULT(0),
    CONSTRAINT FK_Cta_Usr FOREIGN KEY(IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
    CONSTRAINT FK_Cta_Mon FOREIGN KEY(IdMoneda)  REFERENCES dbo.Monedas(IdMoneda),
    CONSTRAINT UX_Cta UNIQUE(IdUsuario,IdMoneda)
  );
END
GO

IF OBJECT_ID('dbo.Asientos','U') IS NULL
BEGIN
  CREATE TABLE dbo.Asientos(
    IdAsiento BIGINT IDENTITY(1,1) PRIMARY KEY,
    IdTipo SMALLINT NOT NULL,
    Fecha DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
    ReferenciaExterna NVARCHAR(100) NULL,
    CONSTRAINT FK_Asi_Tipo FOREIGN KEY(IdTipo) REFERENCES dbo.TiposTransaccion(IdTipo)
  );
END
GO

IF OBJECT_ID('dbo.Movimientos','U') IS NULL
BEGIN
  CREATE TABLE dbo.Movimientos(
    IdMovimiento BIGINT IDENTITY(1,1) PRIMARY KEY,
    IdAsiento BIGINT NOT NULL,
    IdCuenta BIGINT NOT NULL,
    Debe  DECIMAL(38,8) NOT NULL DEFAULT(0),
    Haber DECIMAL(38,8) NOT NULL DEFAULT(0),
    Ts DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
    CONSTRAINT FK_Mov_Asi FOREIGN KEY(IdAsiento) REFERENCES dbo.Asientos(IdAsiento),
    CONSTRAINT FK_Mov_Cta FOREIGN KEY(IdCuenta)  REFERENCES dbo.Cuentas(IdCuenta)
  );
  CREATE INDEX IX_Mov_Cta_Ts ON dbo.Movimientos(IdCuenta, Ts DESC);
END
GO

/* --- Juegos / Mesas / Rondas / Apuestas --- */
IF OBJECT_ID('dbo.ProveedoresJuego','U') IS NULL
BEGIN
  CREATE TABLE dbo.ProveedoresJuego(
    IdProveedor INT IDENTITY(1,1) PRIMARY KEY,
    Nombre NVARCHAR(120) NOT NULL UNIQUE
  );
END
GO

IF OBJECT_ID('dbo.Juegos','U') IS NULL
BEGIN
  CREATE TABLE dbo.Juegos(
    IdJuego INT IDENTITY(1,1) PRIMARY KEY,
    IdProveedor INT NOT NULL,
    Tipo NVARCHAR(40) NOT NULL,
    Nombre NVARCHAR(120) NOT NULL UNIQUE,
    RtpTeorico DECIMAL(5,2) NULL,
    CONSTRAINT FK_Jue_Prov FOREIGN KEY(IdProveedor) REFERENCES dbo.ProveedoresJuego(IdProveedor)
  );
END
GO

IF OBJECT_ID('dbo.Mesas','U') IS NULL
BEGIN
  CREATE TABLE dbo.Mesas(
    IdMesa INT IDENTITY(1,1) PRIMARY KEY,
    IdJuego INT NOT NULL,
    Nombre NVARCHAR(80) NOT NULL,
    Estado NVARCHAR(20) NOT NULL,
    CONSTRAINT FK_Mesa_Jue FOREIGN KEY(IdJuego) REFERENCES dbo.Juegos(IdJuego),
    CONSTRAINT UX_Mesa UNIQUE(IdJuego,Nombre)
  );
END
GO

IF OBJECT_ID('dbo.Rondas','U') IS NULL
BEGIN
  CREATE TABLE dbo.Rondas(
    IdRonda BIGINT IDENTITY(1,1) PRIMARY KEY,
    IdJuego INT NOT NULL,
    IdMesa  INT NULL,
    Inicio DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
    Fin    DATETIME2(3) NULL,
    Estado NVARCHAR(20) NOT NULL,
    CONSTRAINT FK_Ron_Jue FOREIGN KEY(IdJuego) REFERENCES dbo.Juegos(IdJuego),
    CONSTRAINT FK_Ron_Mesa FOREIGN KEY(IdMesa)  REFERENCES dbo.Mesas(IdMesa)
  );
END
GO

IF OBJECT_ID('dbo.ResultadosRonda','U') IS NULL
BEGIN
  CREATE TABLE dbo.ResultadosRonda(
    IdRonda BIGINT NOT NULL PRIMARY KEY,
    PayloadResultado NVARCHAR(MAX) NULL,
    Checksum VARBINARY(32) NULL,
    Ts DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
    CONSTRAINT FK_Res_Ron FOREIGN KEY(IdRonda) REFERENCES dbo.Rondas(IdRonda)
  );
END
GO

IF OBJECT_ID('dbo.Apuestas','U') IS NULL
BEGIN
  CREATE TABLE dbo.Apuestas(
    IdApuesta BIGINT IDENTITY(1,1) PRIMARY KEY,
    IdUsuario INT NOT NULL,
    IdRonda   BIGINT NOT NULL,
    IdMoneda  INT NOT NULL,
    Importe   DECIMAL(38,8) NOT NULL,
    Estado NVARCHAR(20) NOT NULL,
    Ts DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
    CONSTRAINT FK_Apu_Usr FOREIGN KEY(IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
    CONSTRAINT FK_Apu_Ron FOREIGN KEY(IdRonda)   REFERENCES dbo.Rondas(IdRonda),
    CONSTRAINT FK_Apu_Mon FOREIGN KEY(IdMoneda)  REFERENCES dbo.Monedas(IdMoneda)
  );
END
GO

IF OBJECT_ID('dbo.ApuestaDetalle','U') IS NULL
BEGIN
  CREATE TABLE dbo.ApuestaDetalle(
    IdDetalle BIGINT IDENTITY(1,1) PRIMARY KEY,
    IdApuesta BIGINT NOT NULL,
    Seleccion NVARCHAR(80) NOT NULL,
    Multiplicador DECIMAL(10,4) NULL,
    CONSTRAINT FK_ApDet_Apu FOREIGN KEY(IdApuesta) REFERENCES dbo.Apuestas(IdApuesta)
  );
END
GO

/* --- Pagos (Dep�sitos / Retiros) --- */
IF OBJECT_ID('dbo.ProveedoresPago','U') IS NULL
BEGIN
  CREATE TABLE dbo.ProveedoresPago(
    IdProveedorPago INT IDENTITY(1,1) PRIMARY KEY,
    Nombre NVARCHAR(120) NOT NULL UNIQUE,
    Tipo NVARCHAR(30) NOT NULL  -- 'crypto'
  );
END
GO

IF OBJECT_ID('dbo.MetodosPago','U') IS NULL
BEGIN
  CREATE TABLE dbo.MetodosPago(
    IdMetodo INT IDENTITY(1,1) PRIMARY KEY,
    IdProveedorPago INT NOT NULL,
    Config NVARCHAR(4000) NULL,
    CONSTRAINT FK_Met_Pago FOREIGN KEY(IdProveedorPago) REFERENCES dbo.ProveedoresPago(IdProveedorPago)
  );
END
GO

IF OBJECT_ID('dbo.Depositos','U') IS NULL
BEGIN
  CREATE TABLE dbo.Depositos(
    IdDeposito BIGINT IDENTITY(1,1) PRIMARY KEY,
    IdUsuario INT NOT NULL,
    IdMetodo  INT NOT NULL,
    IdMoneda  INT NOT NULL,
    Monto     DECIMAL(38,8) NOT NULL,
    Estado NVARCHAR(20) NOT NULL DEFAULT N'pending',
    Ts DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
    ExternalId NVARCHAR(100) NULL,
    -- ON-CHAIN
    NetworkCode   NVARCHAR(10)  NULL,
    TxHash        NVARCHAR(100) NULL,
    FromAddress   NVARCHAR(128) NULL,
    ToAddress     NVARCHAR(128) NULL,
    Confirmations INT NOT NULL DEFAULT(0),
    FeeNetwork    DECIMAL(38,8) NULL,
    CONSTRAINT FK_Dep_Usr FOREIGN KEY(IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
    CONSTRAINT FK_Dep_Met FOREIGN KEY(IdMetodo)  REFERENCES dbo.MetodosPago(IdMetodo),
    CONSTRAINT FK_Dep_Mon FOREIGN KEY(IdMoneda)  REFERENCES dbo.Monedas(IdMoneda)
  );
  CREATE UNIQUE INDEX UX_Dep_Tx ON dbo.Depositos(NetworkCode, TxHash) WHERE TxHash IS NOT NULL;
END
GO

IF OBJECT_ID('dbo.Retiros','U') IS NULL
BEGIN
  CREATE TABLE dbo.Retiros(
    IdRetiro BIGINT IDENTITY(1,1) PRIMARY KEY,
    IdUsuario INT NOT NULL,
    IdMetodo  INT NOT NULL,
    IdMoneda  INT NOT NULL,
    Monto     DECIMAL(38,8) NOT NULL,
    Estado NVARCHAR(20) NOT NULL DEFAULT N'requested',
    Ts DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
    -- ON-CHAIN
    NetworkCode   NVARCHAR(10)  NULL,
    TxHash        NVARCHAR(100) NULL,
    ToAddress     NVARCHAR(128) NULL,
    Confirmations INT NOT NULL DEFAULT(0),
    FeeNetwork    DECIMAL(38,8) NULL,
    CONSTRAINT FK_Ret_Usr FOREIGN KEY(IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
    CONSTRAINT FK_Ret_Met FOREIGN KEY(IdMetodo)  REFERENCES dbo.MetodosPago(IdMetodo),
    CONSTRAINT FK_Ret_Mon FOREIGN KEY(IdMoneda)  REFERENCES dbo.Monedas(IdMoneda)
  );
  CREATE UNIQUE INDEX UX_Ret_Tx ON dbo.Retiros(NetworkCode, TxHash) WHERE TxHash IS NOT NULL;
END
GO

/* --- Verificaci�n por email (tokens) --- */
IF OBJECT_ID('dbo.VerificationToken','U') IS NULL
BEGIN
  CREATE TABLE dbo.VerificationToken(
    TokenId UNIQUEIDENTIFIER NOT NULL ROWGUIDCOL DEFAULT NEWSEQUENTIALID() PRIMARY KEY,
    UserId INT NOT NULL,
    Purpose TINYINT NOT NULL,  -- 1=EmailVerify
    Channel TINYINT NOT NULL,  -- 1=email
    Target NVARCHAR(256) NOT NULL,
    CodeHash VARBINARY(32) NOT NULL,
    Salt VARBINARY(16) NOT NULL,
    Attempts INT NOT NULL DEFAULT(0),
    MaxAttempts TINYINT NOT NULL DEFAULT(5),
    ExpiresAt DATETIME2(3) NOT NULL,
    Status TINYINT NOT NULL DEFAULT(0), -- 0=pending,1=ok,2=expired,3=blocked
    SentAt DATETIME2(3) NULL,
    ConsumedAt DATETIME2(3) NULL,
    CreatedAt DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
    CONSTRAINT FK_Verif_User FOREIGN KEY(UserId) REFERENCES dbo.Usuarios(IdUsuario)
  );
END
GO

/* --- Pol�tica on-chain por moneda (confirmaciones requeridas) --- */
IF OBJECT_ID('dbo.CryptoNetworkPolicy','U') IS NULL
BEGIN
  CREATE TABLE dbo.CryptoNetworkPolicy(
    IdMoneda INT NOT NULL PRIMARY KEY,
    NetworkCode NVARCHAR(10) NOT NULL,
    RequiredConfirmations INT NOT NULL DEFAULT(2),
    CONSTRAINT FK_CNP_Mon FOREIGN KEY(IdMoneda) REFERENCES dbo.Monedas(IdMoneda)
  );
END
GO

/* ===========================================================
   1) TIPOS TVP
   =========================================================== */
IF TYPE_ID('dbo.TVP_Movimiento') IS NULL
  EXEC ('CREATE TYPE dbo.TVP_Movimiento AS TABLE(
           IdCuenta BIGINT NOT NULL,
           Debe     DECIMAL(38,8) NOT NULL DEFAULT(0),
           Haber    DECIMAL(38,8) NOT NULL DEFAULT(0)
        )');
GO
IF TYPE_ID('dbo.TVP_ApuestaDetalle') IS NULL
  EXEC ('CREATE TYPE dbo.TVP_ApuestaDetalle AS TABLE(
           Seleccion     NVARCHAR(80) NOT NULL,
           Multiplicador DECIMAL(10,4) NULL
        )');
GO

/* ===========================================================
   2) SPs � Usuarios / Verificaci�n
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Usuario_Crear
  @Email NVARCHAR(256),
  @HashPwd VARBINARY(MAX),
  @IdUsuario INT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF EXISTS (SELECT 1 FROM dbo.Usuarios WHERE Email=@Email)
    THROW 50050, N'El email ya est� registrado.', 1;

  INSERT INTO dbo.Usuarios(Email, HashPwd, Activo, EmailVerificado, FechaAlta)
  VALUES(@Email, @HashPwd, 0, 0, SYSUTCDATETIME());
  SET @IdUsuario = SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Verificacion_Email_Iniciar
  @UserId INT, @Email NVARCHAR(256),
  @Purpose TINYINT = 1, @TTL_Minutes INT = 15,
  @DebugReturnCode BIT = 0,
  @TokenId UNIQUEIDENTIFIER OUTPUT,
  @Dev_Code NVARCHAR(6) OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  DECLARE @now DATETIME2(3)=SYSUTCDATETIME();

  SELECT TOP(1) @TokenId=TokenId
  FROM dbo.VerificationToken WITH (UPDLOCK,HOLDLOCK)
  WHERE UserId=@UserId AND Purpose=@Purpose AND Status=0 AND ExpiresAt>@now
  ORDER BY SentAt DESC;

  IF @TokenId IS NULL
  BEGIN
    DECLARE @code NVARCHAR(6);
    DECLARE @rb VARBINARY(4)=CRYPT_GEN_RANDOM(4);
    DECLARE @n INT=ABS(CAST(@rb AS INT)) % 1000000;
    SET @code = RIGHT('000000'+CONVERT(NVARCHAR(6),@n),6);

    DECLARE @salt VARBINARY(16)=CRYPT_GEN_RANDOM(16);
    DECLARE @hash VARBINARY(32)=HASHBYTES('SHA2_256', @salt + CONVERT(VARBINARY(100),@code));

    DECLARE @t TABLE(TokenId UNIQUEIDENTIFIER);
    INSERT INTO dbo.VerificationToken(UserId,Purpose,Channel,Target,CodeHash,Salt,ExpiresAt,Status,MaxAttempts,SentAt,CreatedAt)
    OUTPUT inserted.TokenId INTO @t
    VALUES(@UserId,@Purpose,1,@Email,@hash,@salt,DATEADD(MINUTE,@TTL_Minutes,@now),0,5,@now,@now);

    SELECT @TokenId=TokenId FROM @t;
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
  @UserId INT, @Code NVARCHAR(6),
  @Purpose TINYINT = 1,
  @Ok BIT OUTPUT, @Mensaje NVARCHAR(200) OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  DECLARE @now DATETIME2(3)=SYSUTCDATETIME();

  DECLARE @TokenId UNIQUEIDENTIFIER,@Salt VARBINARY(16),@Hash VARBINARY(32),
          @Attempts INT,@MaxAttempts TINYINT,@ExpiresAt DATETIME2(3),@Status TINYINT;

  SELECT TOP(1) @TokenId=TokenId,@Salt=Salt,@Hash=CodeHash,@Attempts=Attempts,
                 @MaxAttempts=MaxAttempts,@ExpiresAt=ExpiresAt,@Status=Status
  FROM dbo.VerificationToken WITH(UPDLOCK,HOLDLOCK)
  WHERE UserId=@UserId AND Purpose=@Purpose AND Status=0
  ORDER BY SentAt DESC;

  IF @TokenId IS NULL BEGIN SET @Ok=0; SET @Mensaje=N'No hay c�digo pendiente.'; RETURN; END
  IF @ExpiresAt<=@now BEGIN UPDATE dbo.VerificationToken SET Status=2 WHERE TokenId=@TokenId; SET @Ok=0; SET @Mensaje=N'C�digo expirado.'; RETURN; END
  IF @Attempts>=@MaxAttempts BEGIN UPDATE dbo.VerificationToken SET Status=3 WHERE TokenId=@TokenId; SET @Ok=0; SET @Mensaje=N'Demasiados intentos.'; RETURN; END

  DECLARE @calc VARBINARY(32)=HASHBYTES('SHA2_256', @Salt + CONVERT(VARBINARY(100),@Code));
  IF @calc=@Hash
  BEGIN
    UPDATE dbo.VerificationToken SET Status=1, ConsumedAt=@now WHERE TokenId=@TokenId;
    UPDATE dbo.Usuarios SET EmailVerificado=1, Activo=1, FechaActivacion=@now WHERE IdUsuario=@UserId;
    SET @Ok=1; SET @Mensaje=N'Cuenta verificada.';
  END
  ELSE
  BEGIN
    UPDATE dbo.VerificationToken
      SET Attempts=Attempts+1, Status=CASE WHEN Attempts+1>=MaxAttempts THEN 3 ELSE Status END
    WHERE TokenId=@TokenId;
    SET @Ok=0; SET @Mensaje=N'C�digo incorrecto.';
  END
END
GO

/* ===========================================================
   3) SPs � Cuentas & Contabilidad
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Cuenta_Abrir
  @IdUsuario INT, @IdMoneda INT, @IdCuenta BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF EXISTS (SELECT 1 FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda)
  BEGIN SELECT @IdCuenta=IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda; RETURN; END
  INSERT INTO dbo.Cuentas(IdUsuario,IdMoneda,SaldoCache,Bloqueada) VALUES(@IdUsuario,@IdMoneda,0,0);
  SET @IdCuenta=SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Cuenta_ObtenerSaldo
  @IdUsuario INT, @IdMoneda INT
AS
BEGIN
  SET NOCOUNT ON;
  DECLARE @IdCuenta BIGINT = (SELECT IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda);
  SELECT
    c.IdCuenta,
    c.SaldoCache,
    (SELECT COALESCE(SUM(Debe - Haber),0) FROM dbo.Movimientos WHERE IdCuenta=c.IdCuenta) AS SaldoEfectivo
  FROM dbo.Cuentas c WHERE c.IdCuenta=@IdCuenta;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Billetera_SincronizarSaldoCache
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  ;WITH r AS (SELECT IdCuenta, SUM(Debe-Haber) s FROM dbo.Movimientos GROUP BY IdCuenta)
  UPDATE c SET c.SaldoCache=COALESCE(r.s,0)
  FROM dbo.Cuentas c LEFT JOIN r ON r.IdCuenta=c.IdCuenta;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Asiento_CrearSimple
  @IdTipo SMALLINT,
  @ReferenciaExterna NVARCHAR(100)=NULL,
  @Movs dbo.TVP_Movimiento READONLY,
  @IdAsiento BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  DECLARE @sumD DECIMAL(38,8), @sumH DECIMAL(38,8);
  SELECT @sumD=COALESCE(SUM(Debe),0), @sumH=COALESCE(SUM(Haber),0) FROM @Movs;
  IF @sumD<>@sumH THROW 50001, N'El asiento no cuadra.', 1;

  IF EXISTS (SELECT 1 FROM @Movs m LEFT JOIN dbo.Cuentas c ON c.IdCuenta=m.IdCuenta WHERE c.IdCuenta IS NULL)
    THROW 50002, N'Cuenta inexistente en movimientos.', 1;

  BEGIN TRAN;
    INSERT INTO dbo.Asientos(IdTipo,Fecha,ReferenciaExterna) VALUES(@IdTipo,SYSUTCDATETIME(),@ReferenciaExterna);
    SET @IdAsiento=SCOPE_IDENTITY();

    INSERT INTO dbo.Movimientos(IdAsiento,IdCuenta,Debe,Haber,Ts)
    SELECT @IdAsiento, IdCuenta, Debe, Haber, SYSUTCDATETIME() FROM @Movs;

    ;WITH d AS (SELECT IdCuenta, SUM(Debe-Haber) Delta FROM @Movs GROUP BY IdCuenta)
    UPDATE c SET c.SaldoCache=c.SaldoCache + d.Delta
    FROM dbo.Cuentas c JOIN d ON d.IdCuenta=c.IdCuenta;
  COMMIT;
END
GO

/* ===========================================================
   4) SPs � Juegos / Rondas / Apuestas
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Mesa_Abrir
  @IdJuego INT, @Nombre NVARCHAR(80), @IdMesa INT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  INSERT INTO dbo.Mesas(IdJuego,Nombre,Estado) VALUES(@IdJuego,@Nombre,N'open');
  SET @IdMesa=SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Ronda_Abrir
  @IdJuego INT, @IdMesa INT = NULL, @IdRonda BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  INSERT INTO dbo.Rondas(IdJuego,IdMesa,Inicio,Estado) VALUES(@IdJuego,@IdMesa,SYSUTCDATETIME(),N'open');
  SET @IdRonda=SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Ronda_Cerrar
  @IdRonda BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  UPDATE dbo.Rondas SET Fin=SYSUTCDATETIME(), Estado=N'closed' WHERE IdRonda=@IdRonda AND Estado<>N'closed';
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Ronda_RegistrarResultado
  @IdRonda BIGINT, @Payload NVARCHAR(MAX), @Checksum VARBINARY(32)=NULL
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  MERGE dbo.ResultadosRonda AS t
  USING (SELECT @IdRonda AS IdRonda) s
  ON t.IdRonda=s.IdRonda
  WHEN MATCHED THEN UPDATE SET PayloadResultado=@Payload, Checksum=@Checksum, Ts=SYSUTCDATETIME()
  WHEN NOT MATCHED THEN INSERT(IdRonda,PayloadResultado,Checksum,Ts) VALUES(@IdRonda,@Payload,@Checksum,SYSUTCDATETIME());
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Apuesta_Colocar
  @IdUsuario INT, @IdRonda BIGINT, @IdMoneda INT, @Importe DECIMAL(38,8),
  @IdTipoAsiento SMALLINT, @IdCuentaCasaJuego BIGINT,
  @Detalle dbo.TVP_ApuestaDetalle READONLY,
  @IdApuesta BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;

  IF @Importe<=0 THROW 50010, N'Importe inv�lido.', 1;

  DECLARE @estado NVARCHAR(20)=(SELECT Estado FROM dbo.Rondas WHERE IdRonda=@IdRonda);
  IF @estado IS NULL OR @estado<>N'open' THROW 50011, N'Ronda no abierta.', 1;

  DECLARE @IdCuentaUsuario BIGINT=(SELECT IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda);
  IF @IdCuentaUsuario IS NULL THROW 50012, N'El usuario no tiene cuenta para esa moneda.', 1;
  IF EXISTS (SELECT 1 FROM dbo.Cuentas WHERE IdCuenta=@IdCuentaUsuario AND Bloqueada=1) THROW 50014, N'Cuenta bloqueada.', 1;

  DECLARE @saldo DECIMAL(38,8)=(SELECT SaldoCache FROM dbo.Cuentas WITH(UPDLOCK,ROWLOCK) WHERE IdCuenta=@IdCuentaUsuario);
  IF @saldo<@Importe THROW 50013, N'Saldo insuficiente.', 1;

  BEGIN TRAN;
    INSERT INTO dbo.Apuestas(IdUsuario,IdRonda,IdMoneda,Importe,Estado,Ts)
    VALUES(@IdUsuario,@IdRonda,@IdMoneda,@Importe,N'placed',SYSUTCDATETIME());
    SET @IdApuesta=SCOPE_IDENTITY();

    INSERT INTO dbo.ApuestaDetalle(IdApuesta,Seleccion,Multiplicador)
    SELECT @IdApuesta, Seleccion, Multiplicador FROM @Detalle;

    DECLARE @Movs dbo.TVP_Movimiento, @IdAsiento BIGINT, @Ref NVARCHAR(100);
    SET @Ref=N'APUESTA-' + CONVERT(NVARCHAR(40),@IdApuesta);

    INSERT INTO @Movs(IdCuenta,Debe,Haber) VALUES(@IdCuentaCasaJuego,@Importe,0);
    INSERT INTO @Movs(IdCuenta,Debe,Haber) VALUES(@IdCuentaUsuario,0,@Importe);

    EXEC dbo.usp_Asiento_CrearSimple @IdTipo=@IdTipoAsiento, @ReferenciaExterna=@Ref, @Movs=@Movs, @IdAsiento=@IdAsiento OUTPUT;
  COMMIT;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Ronda_Liquidar
  @IdRonda BIGINT, @IdTipoAsientoPremio SMALLINT, @IdCuentaCasaJuego BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF NOT EXISTS(SELECT 1 FROM dbo.Rondas WHERE IdRonda=@IdRonda) THROW 50020, N'Ronda inexistente.', 1;

  DECLARE @t TABLE(IdApuesta BIGINT PRIMARY KEY, IdUsuario INT, IdMoneda INT, Importe DECIMAL(38,8), Payout DECIMAL(38,8));

  INSERT INTO @t(IdApuesta,IdUsuario,IdMoneda,Importe,Payout)
  SELECT a.IdApuesta,a.IdUsuario,a.IdMoneda,a.Importe,a.Importe*COALESCE(MAX(d.Multiplicador),0)
  FROM dbo.Apuestas a LEFT JOIN dbo.ApuestaDetalle d ON d.IdApuesta=a.IdApuesta
  WHERE a.IdRonda=@IdRonda AND a.Estado=N'placed'
  GROUP BY a.IdApuesta,a.IdUsuario,a.IdMoneda,a.Importe;

  DECLARE @IdApuesta BIGINT,@IdUsuario INT,@IdMoneda INT,@Importe DECIMAL(38,8),@Payout DECIMAL(38,8);
  DECLARE cur CURSOR LOCAL FAST_FORWARD FOR SELECT IdApuesta,IdUsuario,IdMoneda,Importe,Payout FROM @t;
  OPEN cur; FETCH NEXT FROM cur INTO @IdApuesta,@IdUsuario,@IdMoneda,@Importe,@Payout;
  WHILE @@FETCH_STATUS=0
  BEGIN
    UPDATE dbo.Apuestas SET Estado=CASE WHEN @Payout>0 THEN N'won' ELSE N'lost' END WHERE IdApuesta=@IdApuesta;
    IF @Payout>0
    BEGIN
      DECLARE @IdCuentaUsuario BIGINT=(SELECT IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda);
      DECLARE @Movs dbo.TVP_Movimiento,@IdAsiento BIGINT,@Ref NVARCHAR(100);
      SET @Ref=N'PAGO_APUESTA-' + CONVERT(NVARCHAR(40),@IdApuesta);
      INSERT INTO @Movs(IdCuenta,Debe,Haber) VALUES(@IdCuentaUsuario,@Payout,0);
      INSERT INTO @Movs(IdCuenta,Debe,Haber) VALUES(@IdCuentaCasaJuego,0,@Payout);
      EXEC dbo.usp_Asiento_CrearSimple @IdTipo=@IdTipoAsientoPremio, @ReferenciaExterna=@Ref, @Movs=@Movs, @IdAsiento=@IdAsiento OUTPUT;
    END
    FETCH NEXT FROM cur INTO @IdApuesta,@IdUsuario,@IdMoneda,@Importe,@Payout;
  END
  CLOSE cur; DEALLOCATE cur;

  EXEC dbo.usp_Ronda_Cerrar @IdRonda=@IdRonda;
END
GO

/* ===========================================================
   5) SPs � Dep�sitos / Retiros
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Deposito_Iniciar
  @IdUsuario INT,@IdMetodo INT,@IdMoneda INT,@Monto DECIMAL(38,8),@IdDeposito BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF @Monto<=0 THROW 50030, N'Monto inv�lido.', 1;
  INSERT INTO dbo.Depositos(IdUsuario,IdMetodo,IdMoneda,Monto,Estado,Ts) VALUES(@IdUsuario,@IdMetodo,@IdMoneda,@Monto,N'pending',SYSUTCDATETIME());
  SET @IdDeposito=SCOPE_IDENTITY();
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Deposito_Confirmar
  @IdDeposito BIGINT = NULL, @ExternalId NVARCHAR(100) = NULL, @Monto DECIMAL(38,8) = NULL,
  @IdTipoAsiento SMALLINT, @IdCuentaCasaCaja BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  DECLARE @IdDep BIGINT;
  IF @IdDeposito IS NOT NULL SET @IdDep=@IdDeposito;
  ELSE IF @ExternalId IS NOT NULL SELECT @IdDep=IdDeposito FROM dbo.Depositos WHERE ExternalId=@ExternalId;
  ELSE THROW 50031, N'Debe indicar @IdDeposito o @ExternalId.', 1;

  DECLARE @estado NVARCHAR(20),@IdUsuario INT,@IdMoneda INT,@m DECIMAL(38,8);
  SELECT @estado=Estado,@IdUsuario=IdUsuario,@IdMoneda=IdMoneda,@m=Monto FROM dbo.Depositos WHERE IdDeposito=@IdDep;

  IF @estado IS NULL THROW 50032, N'Dep�sito no existe.', 1;
  IF @estado=N'confirmed' RETURN;
  IF @estado<>N'pending' THROW 50033, N'Estado no v�lido.', 1;
  IF @Monto IS NOT NULL AND @Monto<>@m THROW 50034, N'Monto no coincide.', 1;

  DECLARE @IdCuentaUsuario BIGINT=(SELECT IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda);
  IF @IdCuentaUsuario IS NULL THROW 50035, N'Cuenta de usuario no existe.', 1;

  DECLARE @Movs dbo.TVP_Movimiento,@IdAsiento BIGINT,@Ref NVARCHAR(100);
  SET @Ref=N'DEPOSITO-' + CONVERT(NVARCHAR(40),@IdDep);
  INSERT INTO @Movs(IdCuenta,Debe,Haber) VALUES(@IdCuentaUsuario,@m,0);
  INSERT INTO @Movs(IdCuenta,Debe,Haber) VALUES(@IdCuentaCasaCaja,0,@m);
  EXEC dbo.usp_Asiento_CrearSimple @IdTipo=@IdTipoAsiento,@ReferenciaExterna=@Ref,@Movs=@Movs,@IdAsiento=@IdAsiento OUTPUT;

  UPDATE dbo.Depositos SET Estado=N'confirmed', Ts=SYSUTCDATETIME(), ExternalId=COALESCE(@ExternalId,ExternalId) WHERE IdDeposito=@IdDep;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Retiro_Solicitar
  @IdUsuario INT,@IdMetodo INT,@IdMoneda INT,@Monto DECIMAL(38,8),
  @IdTipoAsientoRetencion SMALLINT,@IdCuentaCasaRetencion BIGINT,@IdRetiro BIGINT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON; SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;
  IF @Monto<=0 THROW 50040, N'Monto inv�lido.', 1;
  DECLARE @IdCuentaUsuario BIGINT=(SELECT IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdUsuario AND IdMoneda=@IdMoneda);
  IF @IdCuentaUsuario IS NULL THROW 50041, N'Cuenta del usuario no existe.', 1;

  DECLARE @saldo DECIMAL(38,8)=(SELECT SaldoCache FROM dbo.Cuentas WITH(UPDLOCK,ROWLOCK) WHERE IdCuenta=@IdCuentaUsuario);
  IF @saldo<@Monto THROW 50042, N'Saldo insuficiente.', 1;

  BEGIN TRAN;
    INSERT INTO dbo.Retiros(IdUsuario,IdMetodo,IdMoneda,Monto,Estado,Ts) VALUES(@IdUsuario,@IdMetodo,@IdMoneda,@Monto,N'requested',SYSUTCDATETIME());
    SET @IdRetiro=SCOPE_IDENTITY();

    DECLARE @Movs dbo.TVP_Movimiento,@IdAsiento BIGINT,@Ref NVARCHAR(100);
    SET @Ref=N'RETIRO_REQ-' + CONVERT(NVARCHAR(40),@IdRetiro);
    INSERT INTO @Movs(IdCuenta,Debe,Haber) VALUES(@IdCuentaCasaRetencion,@Monto,0); -- retenci�n en casa
    INSERT INTO @Movs(IdCuenta,Debe,Haber) VALUES(@IdCuentaUsuario,0,@Monto);
    EXEC dbo.usp_Asiento_CrearSimple @IdTipo=@IdTipoAsientoRetencion,@ReferenciaExterna=@Ref,@Movs=@Movs,@IdAsiento=@IdAsiento OUTPUT;
  COMMIT;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Retiro_Aprobar
  @IdRetiro BIGINT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  DECLARE @estado NVARCHAR(20)=(SELECT Estado FROM dbo.Retiros WHERE IdRetiro=@IdRetiro);
  IF @estado IS NULL THROW 50043, N'Retiro no existe.', 1;
  IF @estado NOT IN (N'requested',N'approved') THROW 50044, N'Estado inv�lido.', 1;
  UPDATE dbo.Retiros SET Estado=N'paid', Ts=SYSUTCDATETIME() WHERE IdRetiro=@IdRetiro;
END
GO

/* ===========================================================
   6) SPs � On-chain (confirmaciones)
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Crypto_Deposito_RegistrarConfirmacion
  @TxHash NVARCHAR(100), @NetworkCode NVARCHAR(10), @Confirmations INT,
  @MontoOnChain DECIMAL(38,8) = NULL, @IdCuentaCasaCaja BIGINT = NULL,
  @Ok BIT OUTPUT, @Mensaje NVARCHAR(200) OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  IF @TxHash IS NULL OR LTRIM(RTRIM(@TxHash))=N'' BEGIN SET @Ok=0; SET @Mensaje=N'TxHash requerido.'; RETURN; END

  DECLARE @IdDep BIGINT;
  SELECT TOP(1) @IdDep=IdDeposito FROM dbo.Depositos WHERE TxHash=@TxHash AND NetworkCode=@NetworkCode;

  IF @IdDep IS NULL
  BEGIN
    SELECT TOP(1) @IdDep=IdDeposito FROM dbo.Depositos WHERE ExternalId=@TxHash OR ExternalId=@NetworkCode+N':'+@TxHash;
    IF @IdDep IS NOT NULL UPDATE dbo.Depositos SET TxHash=@TxHash, NetworkCode=@NetworkCode WHERE IdDeposito=@IdDep;
  END

  IF @IdDep IS NULL BEGIN SET @Ok=0; SET @Mensaje=N'No se encontr� dep�sito para ese TxHash.'; RETURN; END

  DECLARE @Estado NVARCHAR(20),@IdUsuario INT,@IdMoneda INT,@Monto DECIMAL(38,8);
  SELECT @Estado=Estado,@IdUsuario=IdUsuario,@IdMoneda=IdMoneda,@Monto=Monto FROM dbo.Depositos WHERE IdDeposito=@IdDep;

  UPDATE dbo.Depositos SET Confirmations = CASE WHEN @Confirmations>Confirmations THEN @Confirmations ELSE Confirmations END WHERE IdDeposito=@IdDep;

  IF @MontoOnChain IS NOT NULL AND @Estado=N'pending' AND @MontoOnChain<>@Monto
  BEGIN SET @Ok=0; SET @Mensaje=N'Monto on-chain no coincide.'; RETURN; END

  IF @Estado=N'confirmed' BEGIN SET @Ok=1; SET @Mensaje=N'Ya confirmado.'; RETURN; END
  IF @Estado<>N'pending'  BEGIN SET @Ok=0; SET @Mensaje=N'Estado no pending.'; RETURN; END

  DECLARE @Req INT=(SELECT RequiredConfirmations FROM dbo.CryptoNetworkPolicy WHERE IdMoneda=@IdMoneda);
  IF @Req IS NULL SET @Req=1;
  IF @Confirmations<@Req BEGIN SET @Ok=1; SET @Mensaje=N'Confirmaciones insuficientes.'; RETURN; END

  IF @IdCuentaCasaCaja IS NULL
  BEGIN
    DECLARE @IdCasa INT=(SELECT TOP(1) IdUsuario FROM dbo.Usuarios WHERE Email=N'house@casino.local');
    SELECT @IdCuentaCasaCaja=IdCuenta FROM dbo.Cuentas WHERE IdUsuario=@IdCasa AND IdMoneda=@IdMoneda;
  END
  IF @IdCuentaCasaCaja IS NULL BEGIN SET @Ok=0; SET @Mensaje=N'Falta cuenta CASA para la moneda.'; RETURN; END

  DECLARE @ExtId NVARCHAR(200)=@NetworkCode+N':'+@TxHash;
  DECLARE @IdTipoDeposito SMALLINT=(SELECT IdTipo FROM dbo.TiposTransaccion WHERE Nombre=N'deposito');

  EXEC dbo.usp_Deposito_Confirmar @IdDeposito=@IdDep,@ExternalId=@ExtId,@Monto=@Monto,@IdTipoAsiento=@IdTipoDeposito,@IdCuentaCasaCaja=@IdCuentaCasaCaja;

  SET @Ok=1; SET @Mensaje=N'Dep�sito confirmado por confirmaciones.';
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Crypto_Retiro_MarcarTx
  @IdRetiro BIGINT, @NetworkCode NVARCHAR(10), @TxHash NVARCHAR(100),
  @FeeNetwork DECIMAL(38,8)=NULL, @Confirmations INT=0,
  @Ok BIT OUTPUT, @Mensaje NVARCHAR(200) OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;
  DECLARE @Estado NVARCHAR(20),@IdMoneda INT;
  SELECT @Estado=Estado,@IdMoneda=IdMoneda FROM dbo.Retiros WHERE IdRetiro=@IdRetiro;

  IF @Estado IS NULL BEGIN SET @Ok=0; SET @Mensaje=N'Retiro inexistente.'; RETURN; END
  IF @Estado NOT IN (N'requested',N'paid') BEGIN SET @Ok=0; SET @Mensaje=N'Estado incompatible.'; RETURN; END

  UPDATE dbo.Retiros
     SET NetworkCode=@NetworkCode,
         TxHash=COALESCE(TxHash,@TxHash),
         FeeNetwork=COALESCE(@FeeNetwork,FeeNetwork),
         Confirmations = CASE WHEN @Confirmations>Confirmations THEN @Confirmations ELSE Confirmations END
   WHERE IdRetiro=@IdRetiro;

  DECLARE @Req INT=(SELECT RequiredConfirmations FROM dbo.CryptoNetworkPolicy WHERE IdMoneda=@IdMoneda);
  IF @Req IS NULL SET @Req=1;

  IF @Confirmations>=@Req AND @Estado=N'requested'
  BEGIN
    EXEC dbo.usp_Retiro_Aprobar @IdRetiro=@IdRetiro;
    SET @Ok=1; SET @Mensaje=N'Retiro marcado como pagado.';
    RETURN;
  END

  SET @Ok=1; SET @Mensaje=N'TX registrada; sin confirmaciones suficientes a�n.';
END
GO

/* ===========================================================
   7) Seeds: CASA + BTC/ETH/LTC + proveedores + mesa demo
   =========================================================== */
CREATE OR ALTER PROCEDURE dbo.usp_Sistema_SeedBasico
  @EmailCasa NVARCHAR(256)=N'house@casino.local', @IdUsuarioCasa INT OUTPUT
AS
BEGIN
  SET NOCOUNT ON; SET XACT_ABORT ON;

  -- Tipos de transacci�n m�nimos
  MERGE dbo.TiposTransaccion AS t
  USING (VALUES(N'deposito'),(N'apuesta'),(N'premio'),(N'retencion'),(N'liberar_retencion')) s(N)
  ON t.Nombre=s.N
  WHEN NOT MATCHED THEN INSERT(Nombre) VALUES(s.N);

  -- Monedas cripto
  IF NOT EXISTS(SELECT 1 FROM dbo.Monedas WHERE CodigoISO='BTC') INSERT INTO dbo.Monedas(CodigoISO,Decimales) VALUES('BTC',8);
  IF NOT EXISTS(SELECT 1 FROM dbo.Monedas WHERE CodigoISO='ETH') INSERT INTO dbo.Monedas(CodigoISO,Decimales) VALUES('ETH',8);
  IF NOT EXISTS(SELECT 1 FROM dbo.Monedas WHERE CodigoISO='LTC') INSERT INTO dbo.Monedas(CodigoISO,Decimales) VALUES('LTC',8);

  -- Casa
  IF NOT EXISTS(SELECT 1 FROM dbo.Usuarios WHERE Email=@EmailCasa)
    INSERT INTO dbo.Usuarios(Email,HashPwd,Activo,EmailVerificado,FechaAlta) VALUES(@EmailCasa,0x01,1,1,SYSUTCDATETIME());
  SELECT @IdUsuarioCasa=IdUsuario FROM dbo.Usuarios WHERE Email=@EmailCasa;

  -- Cuentas de casa por cada moneda
  INSERT INTO dbo.Cuentas(IdUsuario,IdMoneda,SaldoCache,Bloqueada)
  SELECT @IdUsuarioCasa, m.IdMoneda, 0, 0
  FROM dbo.Monedas m
  WHERE NOT EXISTS(SELECT 1 FROM dbo.Cuentas c WHERE c.IdUsuario=@IdUsuarioCasa AND c.IdMoneda=m.IdMoneda);

  -- Pol�tica on-chain por defecto
  MERGE dbo.CryptoNetworkPolicy AS tgt
  USING (
    SELECT IdMoneda, CodigoISO,
           CASE CodigoISO WHEN 'BTC' THEN 2 WHEN 'ETH' THEN 12 WHEN 'LTC' THEN 6 ELSE 1 END AS Req
    FROM dbo.Monedas
  ) src(IdMoneda,CodigoISO,Req)
  ON tgt.IdMoneda=src.IdMoneda
  WHEN MATCHED THEN UPDATE SET NetworkCode=src.CodigoISO, RequiredConfirmations=src.Req
  WHEN NOT MATCHED THEN INSERT(IdMoneda,NetworkCode,RequiredConfirmations) VALUES(src.IdMoneda,src.CodigoISO,src.Req);

  -- Proveedor de juego y mesa demo
  IF NOT EXISTS(SELECT 1 FROM dbo.ProveedoresJuego WHERE Nombre=N'Evolution') INSERT INTO dbo.ProveedoresJuego(Nombre) VALUES(N'Evolution');
  IF NOT EXISTS(SELECT 1 FROM dbo.Juegos WHERE Nombre=N'Ruleta Live')
    INSERT INTO dbo.Juegos(IdProveedor,Tipo,Nombre,RtpTeorico) VALUES((SELECT IdProveedor FROM dbo.ProveedoresJuego WHERE Nombre=N'Evolution'),N'live',N'Ruleta Live',NULL);
  IF NOT EXISTS(SELECT 1 FROM dbo.Mesas WHERE Nombre=N'Mesa Crypto 1')
    INSERT INTO dbo.Mesas(IdJuego,Nombre,Estado) VALUES((SELECT IdJuego FROM dbo.Juegos WHERE Nombre=N'Ruleta Live'),N'Mesa Crypto 1',N'open');

  -- Proveedores/m�todos cripto
  IF NOT EXISTS(SELECT 1 FROM dbo.ProveedoresPago WHERE Nombre=N'CryptoGateway') INSERT INTO dbo.ProveedoresPago(Nombre,Tipo) VALUES(N'CryptoGateway',N'crypto');
  IF NOT EXISTS(SELECT 1 FROM dbo.MetodosPago WHERE IdProveedorPago=(SELECT IdProveedorPago FROM dbo.ProveedoresPago WHERE Nombre=N'CryptoGateway'))
    INSERT INTO dbo.MetodosPago(IdProveedorPago,Config) VALUES((SELECT IdProveedorPago FROM dbo.ProveedoresPago WHERE Nombre=N'CryptoGateway'),N'hotWallet=on;withdraw=manual');
END
GO

/* ===========================================================
   8) Vistas & Reportes (auditor�a)
   =========================================================== */
CREATE OR ALTER VIEW dbo.v_Saldos_UsuarioMoneda AS
SELECT u.IdUsuario,u.Email,m.CodigoISO AS Moneda,c.IdCuenta,c.SaldoCache,
       (SELECT COALESCE(SUM(Debe-Haber),0) FROM dbo.Movimientos WHERE IdCuenta=c.IdCuenta) AS SaldoEfectivo
FROM dbo.Cuentas c JOIN dbo.Usuarios u ON u.IdUsuario=c.IdUsuario JOIN dbo.Monedas m ON m.IdMoneda=c.IdMoneda;
GO

CREATE OR ALTER VIEW dbo.v_BalancePorMoneda AS
SELECT m.CodigoISO AS Moneda,
       SUM(CASE WHEN u.Email=N'house@casino.local' THEN c.SaldoCache ELSE 0 END) AS SaldoCasa,
       SUM(CASE WHEN u.Email<>N'house@casino.local' THEN c.SaldoCache ELSE 0 END) AS SaldoClientes,
       SUM(c.SaldoCache) AS SumaTotal
FROM dbo.Cuentas c JOIN dbo.Usuarios u ON u.IdUsuario=c.IdUsuario JOIN dbo.Monedas m ON m.IdMoneda=c.IdMoneda
GROUP BY m.CodigoISO;
GO

CREATE OR ALTER VIEW dbo.v_MovimientosDetalle AS
SELECT m.IdMovimiento,m.Ts,a.IdAsiento,tt.Nombre AS TipoAsiento,a.ReferenciaExterna,
       c.IdCuenta,u.Email,mon.CodigoISO AS Moneda,m.Debe,m.Haber
FROM dbo.Movimientos m
JOIN dbo.Asientos a ON a.IdAsiento=m.IdAsiento
LEFT JOIN dbo.TiposTransaccion tt ON tt.IdTipo=a.IdTipo
JOIN dbo.Cuentas c ON c.IdCuenta=m.IdCuenta
JOIN dbo.Usuarios u ON u.IdUsuario=c.IdUsuario
JOIN dbo.Monedas mon ON mon.IdMoneda=c.IdMoneda;
GO

CREATE OR ALTER PROCEDURE dbo.usp_SanityCheck_Contable
AS
BEGIN
  SET NOCOUNT ON;
  SELECT c.IdCuenta,c.SaldoCache,COALESCE(SUM(m.Debe-m.Haber),0) AS SaldoCalc,
         c.SaldoCache-COALESCE(SUM(m.Debe-m.Haber),0) AS Diferencia
  FROM dbo.Cuentas c LEFT JOIN dbo.Movimientos m ON m.IdCuenta=c.IdCuenta
  GROUP BY c.IdCuenta,c.SaldoCache
  HAVING ABS(c.SaldoCache-COALESCE(SUM(m.Debe-m.Haber),0))>0.00000001;

  SELECT * FROM dbo.v_BalancePorMoneda;
END
GO

CREATE OR ALTER PROCEDURE dbo.usp_Reporte_GGR
  @Desde DATETIME2(3), @Hasta DATETIME2(3)
AS
BEGIN
  SET NOCOUNT ON;
  DECLARE @TT_Apuesta SMALLINT=(SELECT IdTipo FROM dbo.TiposTransaccion WHERE Nombre=N'apuesta');
  DECLARE @TT_Premio  SMALLINT=(SELECT IdTipo FROM dbo.TiposTransaccion WHERE Nombre=N'premio');
  DECLARE @IdCasa INT=(SELECT IdUsuario FROM dbo.Usuarios WHERE Email=N'house@casino.local');

  ;WITH MovCasa AS(
    SELECT m.Debe,m.Haber,a.IdTipo,c.IdMoneda
    FROM dbo.Movimientos m
    JOIN dbo.Asientos a ON a.IdAsiento=m.IdAsiento
    JOIN dbo.Cuentas c ON c.IdCuenta=m.IdCuenta
    WHERE c.IdUsuario=@IdCasa AND a.Fecha>=@Desde AND a.Fecha<@Hasta
  )
  SELECT mon.CodigoISO AS Moneda,
         SUM(CASE WHEN IdTipo=@TT_Apuesta THEN (Debe-Haber) ELSE 0 END) AS TotalApuestasCasa,
         SUM(CASE WHEN IdTipo=@TT_Premio  THEN (Debe-Haber) ELSE 0 END) AS TotalPremiosCasa,
         SUM(CASE WHEN IdTipo=@TT_Apuesta THEN (Debe-Haber) ELSE 0 END)
         -SUM(CASE WHEN IdTipo=@TT_Premio  THEN (Debe-Haber) ELSE 0 END) AS GGR
  FROM MovCasa mc JOIN dbo.Monedas mon ON mon.IdMoneda=mc.IdMoneda
  GROUP BY mon.CodigoISO ORDER BY mon.CodigoISO;
END
GO

/* ===========================================================
   9) Post-install: Seed r�pido (CASA + cripto + mesa)
   =========================================================== */
DECLARE @IdCasa INT;
EXEC dbo.usp_Sistema_SeedBasico @IdUsuarioCasa=@IdCasa OUTPUT;
PRINT CONCAT('Seed OK. IdUsuario CASA = ', @IdCasa);

