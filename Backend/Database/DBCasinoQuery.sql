USE  Casino

-- =========================================
-- Casino Virtual - DDL completo (SQL Server)
-- =========================================
SET ANSI_NULLS ON;
SET QUOTED_IDENTIFIER ON;
GO

-- =======================
-- 0) Tablas base / Catálogos
-- =======================
CREATE TABLE dbo.Monedas (
  IdMoneda       INT IDENTITY(1,1) PRIMARY KEY,
  CodigoISO      CHAR(3) NOT NULL UNIQUE,
  Decimales      TINYINT NOT NULL CHECK (Decimales BETWEEN 0 AND 8)
);
GO

CREATE TABLE dbo.TiposTransaccion (
  IdTipo         SMALLINT IDENTITY(1,1) PRIMARY KEY,
  Nombre         NVARCHAR(80) NOT NULL UNIQUE
);
GO

CREATE TABLE dbo.Roles (
  IdRol          INT IDENTITY(1,1) PRIMARY KEY,
  Nombre         NVARCHAR(80) NOT NULL UNIQUE
);
GO

CREATE TABLE dbo.ProveedoresJuego (
  IdProveedor    INT IDENTITY(1,1) PRIMARY KEY,
  Nombre         NVARCHAR(120) NOT NULL UNIQUE
);
GO

CREATE TABLE dbo.ProveedoresPago (
  IdProveedorPago INT IDENTITY(1,1) PRIMARY KEY,
  Nombre          NVARCHAR(120) NOT NULL UNIQUE,
  Tipo            NVARCHAR(20)  NOT NULL -- card/crypto/transfer
);
GO

-- =======================
-- 1) Núcleo (Usuarios & Auth)
-- =======================
CREATE TABLE dbo.Usuarios (
  IdUsuario        INT IDENTITY(1,1) PRIMARY KEY,
  Email            NVARCHAR(256) NOT NULL UNIQUE,
  HashPwd          VARBINARY(MAX) NOT NULL,
  Activo           BIT NOT NULL DEFAULT(0),
  EmailVerificado  BIT NOT NULL DEFAULT(0),
  FechaAlta        DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  FechaActivacion  DATETIME2(3) NULL
);
GO

CREATE TABLE dbo.Sesiones (
  IdSesion     BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario    INT NOT NULL,
  IP           NVARCHAR(64) NULL,
  UserAgent    NVARCHAR(256) NULL,
  Inicio       DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  Fin          DATETIME2(3) NULL,
  Valido       BIT NOT NULL DEFAULT(1),
  CONSTRAINT FK_Sesiones_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_Sesiones_IdUsuario_Inicio ON dbo.Sesiones (IdUsuario, Inicio DESC);
GO

CREATE TABLE dbo.UsuarioRoles (
  IdUsuario  INT NOT NULL,
  IdRol      INT NOT NULL,
  PRIMARY KEY (IdUsuario, IdRol),
  CONSTRAINT FK_UsrRoles_Usr FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
  CONSTRAINT FK_UsrRoles_Rol FOREIGN KEY (IdRol) REFERENCES dbo.Roles(IdRol)
);
GO

CREATE TABLE dbo.Dispositivos (
  IdDispositivo BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario     INT NOT NULL,
  Huella        NVARCHAR(128) NOT NULL,
  UltimoUso     DATETIME2(3) NULL,
  Riesgo        TINYINT NULL,
  CONSTRAINT FK_Dispositivos_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_Dispositivos_Usr ON dbo.Dispositivos(IdUsuario);
GO

CREATE TABLE dbo.VerificationToken (
  TokenId        UNIQUEIDENTIFIER NOT NULL ROWGUIDCOL DEFAULT NEWSEQUENTIALID() PRIMARY KEY,
  UserId         INT NOT NULL,
  Purpose        TINYINT NOT NULL,     -- 1=EmailVerify, 2=PwdReset, etc
  Channel        TINYINT NOT NULL,     -- 1=Email, 2=SMS
  Target         NVARCHAR(256) NULL,
  CodeHash       VARBINARY(32) NOT NULL,
  Salt           VARBINARY(16) NOT NULL,
  ExpiresAt      DATETIME2(3) NOT NULL,
  SentAt         DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  ConsumedAt     DATETIME2(3) NULL,
  Attempts       INT NOT NULL DEFAULT(0),
  MaxAttempts    TINYINT NOT NULL DEFAULT(5),
  Status         TINYINT NOT NULL DEFAULT(0), -- 0=PENDING,1=CONSUMED,2=EXPIRED,3=LOCKED
  IdempotencyKey UNIQUEIDENTIFIER NULL,
  CreatedAt      DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_VerifToken_Usuario FOREIGN KEY (UserId) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE UNIQUE INDEX UX_Verif_Pending ON dbo.VerificationToken(UserId, Purpose, Status) WHERE Status = 0;
CREATE INDEX IX_Verif_ExpiresAt ON dbo.VerificationToken(ExpiresAt);
GO

-- =======================
-- 2) Finanzas (Billetera & Contabilidad)
-- =======================
CREATE TABLE dbo.Cuentas (
  IdCuenta     BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario    INT NOT NULL,
  IdMoneda     INT NOT NULL,
  SaldoCache   DECIMAL(19,4) NOT NULL DEFAULT(0),
  Bloqueada    BIT NOT NULL DEFAULT(0),
  RowVersion   ROWVERSION,
  CONSTRAINT FK_Cuentas_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
  CONSTRAINT FK_Cuentas_Moneda  FOREIGN KEY (IdMoneda)  REFERENCES dbo.Monedas(IdMoneda),
  CONSTRAINT UX_Cuentas_U_M UNIQUE (IdUsuario, IdMoneda)
);
GO
CREATE INDEX IX_Cuentas_Moneda ON dbo.Cuentas(IdMoneda);
GO

CREATE TABLE dbo.Asientos (
  IdAsiento         BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdTipo            SMALLINT NOT NULL,
  Fecha             DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  ReferenciaExterna NVARCHAR(100) NULL,
  CONSTRAINT FK_Asientos_Tipo FOREIGN KEY (IdTipo) REFERENCES dbo.TiposTransaccion(IdTipo)
);
GO
CREATE INDEX IX_Asientos_Fecha ON dbo.Asientos(Fecha DESC);
GO

CREATE TABLE dbo.Movimientos (
  IdMovimiento BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdAsiento    BIGINT NOT NULL,
  IdCuenta     BIGINT NOT NULL,
  Debe         DECIMAL(19,4) NOT NULL DEFAULT(0),
  Haber        DECIMAL(19,4) NOT NULL DEFAULT(0),
  Ts           DATETIME2(3)  NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Mov_Asiento FOREIGN KEY (IdAsiento) REFERENCES dbo.Asientos(IdAsiento),
  CONSTRAINT FK_Mov_Cuenta  FOREIGN KEY (IdCuenta)  REFERENCES dbo.Cuentas(IdCuenta),
  CONSTRAINT CK_Mov_DebeHaber CHECK (
      (Debe = 0 AND Haber > 0) OR (Haber = 0 AND Debe > 0)
  )
);
GO
CREATE INDEX IX_Movimientos_Cuenta_Ts ON dbo.Movimientos(IdCuenta, Ts DESC);
CREATE INDEX IX_Movimientos_Asiento ON dbo.Movimientos(IdAsiento);
GO

CREATE TABLE dbo.TasasCambio (
  IdTasa          BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdMonedaOrigen  INT NOT NULL,
  IdMonedaDestino INT NOT NULL,
  Tasa            DECIMAL(19,8) NOT NULL CHECK (Tasa > 0),
  Fecha           DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_TC_Origen  FOREIGN KEY (IdMonedaOrigen)  REFERENCES dbo.Monedas(IdMoneda),
  CONSTRAINT FK_TC_Destino FOREIGN KEY (IdMonedaDestino) REFERENCES dbo.Monedas(IdMoneda),
  CONSTRAINT UX_TC_ParFecha UNIQUE (IdMonedaOrigen, IdMonedaDestino, Fecha)
);
GO

-- =======================
-- 3) Juegos & Apuestas
-- =======================
CREATE TABLE dbo.Juegos (
  IdJuego      INT IDENTITY(1,1) PRIMARY KEY,
  IdProveedor  INT NOT NULL,
  Tipo         NVARCHAR(40) NOT NULL,  -- slot/ruleta/blackjack/live
  RtpTeorico   DECIMAL(5,2) NULL,
  Nombre       NVARCHAR(120) NOT NULL,
  CONSTRAINT FK_Juegos_Proveedor FOREIGN KEY (IdProveedor) REFERENCES dbo.ProveedoresJuego(IdProveedor)
);
GO
CREATE INDEX IX_Juegos_Proveedor ON dbo.Juegos(IdProveedor);
GO

CREATE TABLE dbo.Mesas (
  IdMesa     INT IDENTITY(1,1) PRIMARY KEY,
  IdJuego    INT NOT NULL,
  Nombre     NVARCHAR(80) NOT NULL,
  Estado     NVARCHAR(20) NOT NULL,
  CONSTRAINT FK_Mesas_Juego FOREIGN KEY (IdJuego) REFERENCES dbo.Juegos(IdJuego)
);
GO
CREATE INDEX IX_Mesas_Juego ON dbo.Mesas(IdJuego);
GO

CREATE TABLE dbo.Rondas (
  IdRonda   BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdJuego   INT NOT NULL,
  IdMesa    INT NULL,
  Inicio    DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  Fin       DATETIME2(3) NULL,
  Estado    NVARCHAR(20) NOT NULL,
  CONSTRAINT FK_Rondas_Juego FOREIGN KEY (IdJuego) REFERENCES dbo.Juegos(IdJuego),
  CONSTRAINT FK_Rondas_Mesa  FOREIGN KEY (IdMesa)  REFERENCES dbo.Mesas(IdMesa)
);
GO
CREATE INDEX IX_Rondas_Juego_Inicio ON dbo.Rondas(IdJuego, Inicio DESC);
CREATE INDEX IX_Rondas_Mesa_Inicio  ON dbo.Rondas(IdMesa,  Inicio DESC);
GO

CREATE TABLE dbo.ResultadosRonda (
  IdRonda           BIGINT NOT NULL PRIMARY KEY,  -- 1:1 con Rondas
  PayloadResultado  NVARCHAR(MAX) NOT NULL,
  Checksum          VARBINARY(32) NULL,
  Ts                DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Resultados_Ronda FOREIGN KEY (IdRonda) REFERENCES dbo.Rondas(IdRonda)
);
GO

CREATE TABLE dbo.Apuestas (
  IdApuesta  BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario  INT NOT NULL,
  IdRonda    BIGINT NOT NULL,
  IdMoneda   INT NOT NULL,
  Importe    DECIMAL(19,4) NOT NULL CHECK (Importe > 0),
  Estado     NVARCHAR(20) NOT NULL, -- placed/won/lost/cancelled/settled
  Ts         DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Ap_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
  CONSTRAINT FK_Ap_Ronda   FOREIGN KEY (IdRonda)   REFERENCES dbo.Rondas(IdRonda),
  CONSTRAINT FK_Ap_Moneda  FOREIGN KEY (IdMoneda)  REFERENCES dbo.Monedas(IdMoneda)
);
GO
CREATE INDEX IX_Apuestas_Usr_Ts   ON dbo.Apuestas(IdUsuario, Ts DESC);
CREATE INDEX IX_Apuestas_Ronda    ON dbo.Apuestas(IdRonda);
CREATE INDEX IX_Apuestas_Moneda   ON dbo.Apuestas(IdMoneda);
GO

CREATE TABLE dbo.ApuestaDetalle (
  IdDetalle      BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdApuesta      BIGINT NOT NULL,
  Seleccion      NVARCHAR(80) NOT NULL,
  Multiplicador  DECIMAL(10,4) NULL,
  CONSTRAINT FK_Apd_Apuesta FOREIGN KEY (IdApuesta) REFERENCES dbo.Apuestas(IdApuesta)
);
GO
CREATE INDEX IX_ApuestaDetalle_Apuesta ON dbo.ApuestaDetalle(IdApuesta);
GO

CREATE TABLE dbo.Jackpots (
  IdJackpot  INT IDENTITY(1,1) PRIMARY KEY,
  Tipo       NVARCHAR(40) NOT NULL,
  Semilla    DECIMAL(19,4) NOT NULL DEFAULT(0),
  Estado     NVARCHAR(20) NOT NULL
);
GO

CREATE TABLE dbo.JackpotContribuciones (
  IdContrib  BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdJackpot  INT NOT NULL,
  IdApuesta  BIGINT NOT NULL,
  Monto      DECIMAL(19,4) NOT NULL CHECK (Monto > 0),
  Ts         DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_JC_Jackpot FOREIGN KEY (IdJackpot) REFERENCES dbo.Jackpots(IdJackpot),
  CONSTRAINT FK_JC_Apuesta FOREIGN KEY (IdApuesta) REFERENCES dbo.Apuestas(IdApuesta)
);
GO
CREATE INDEX IX_JC_Jackpot_Ts ON dbo.JackpotContribuciones(IdJackpot, Ts DESC);
GO

CREATE TABLE dbo.JackpotPagos (
  IdPago    BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdJackpot INT NOT NULL,
  IdUsuario INT NOT NULL,
  Monto     DECIMAL(19,4) NOT NULL CHECK (Monto > 0),
  Ts        DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_JP_Jackpot FOREIGN KEY (IdJackpot) REFERENCES dbo.Jackpots(IdJackpot),
  CONSTRAINT FK_JP_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_JP_Jackpot_Ts ON dbo.JackpotPagos(IdJackpot, Ts DESC);
CREATE INDEX IX_JP_Usuario_Ts ON dbo.JackpotPagos(IdUsuario, Ts DESC);
GO

-- =======================
-- 4) Bonos, Promos & Lealtad
-- =======================
CREATE TABLE dbo.Bonos (
  IdBono            INT IDENTITY(1,1) PRIMARY KEY,
  Nombre            NVARCHAR(120) NOT NULL,
  Tipo              NVARCHAR(20)  NOT NULL,  -- saldo/giros
  Porcentaje        DECIMAL(5,2) NULL,
  Tope              DECIMAL(19,4) NULL,
  WageringX         INT NULL,
  JuegosRestringidos NVARCHAR(MAX) NULL
);
GO

CREATE TABLE dbo.Cupones (
  IdCupon        INT IDENTITY(1,1) PRIMARY KEY,
  IdBono         INT NOT NULL,
  Codigo         NVARCHAR(40) NOT NULL UNIQUE,
  VigenciaDesde  DATETIME2(3) NOT NULL,
  VigenciaHasta  DATETIME2(3) NOT NULL,
  UsosMax        INT NULL,
  CONSTRAINT FK_Cupon_Bono FOREIGN KEY (IdBono) REFERENCES dbo.Bonos(IdBono)
);
GO

CREATE TABLE dbo.UsuarioBonos (
  IdUsuarioBono     BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario         INT NOT NULL,
  IdBono            INT NOT NULL,
  SaldoBono         DECIMAL(19,4) NOT NULL DEFAULT(0),
  WageringPendiente DECIMAL(19,4) NOT NULL DEFAULT(0),
  Estado            NVARCHAR(20) NOT NULL, -- activo/consumido/expirado/bloqueado
  CONSTRAINT FK_UB_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
  CONSTRAINT FK_UB_Bono    FOREIGN KEY (IdBono)    REFERENCES dbo.Bonos(IdBono)
);
GO
CREATE UNIQUE INDEX UX_UB_Usr_Bono ON dbo.UsuarioBonos(IdUsuario, IdBono) WHERE Estado IN (N'activo', N'bloqueado');
GO

CREATE TABLE dbo.ConversionesBono (
  IdConversion     BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuarioBono    BIGINT NOT NULL,
  MontoConvertido  DECIMAL(19,4) NOT NULL CHECK (MontoConvertido > 0),
  Ts               DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_CB_UB FOREIGN KEY (IdUsuarioBono) REFERENCES dbo.UsuarioBonos(IdUsuarioBono)
);
GO
CREATE INDEX IX_CB_UB_Ts ON dbo.ConversionesBono(IdUsuarioBono, Ts DESC);
GO

CREATE TABLE dbo.Promociones (
  IdPromo        INT IDENTITY(1,1) PRIMARY KEY,
  Nombre         NVARCHAR(120) NOT NULL,
  Reglas         NVARCHAR(MAX) NULL,
  VigenciaDesde  DATETIME2(3) NOT NULL,
  VigenciaHasta  DATETIME2(3) NOT NULL
);
GO

CREATE TABLE dbo.PuntosLealtad (
  IdUsuario              INT NOT NULL PRIMARY KEY,
  Puntos                 BIGINT NOT NULL DEFAULT(0),
  TsUltimaActualizacion  DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Puntos_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO

-- =======================
-- 5) Pagos (Depósitos/Retiros)
-- =======================
CREATE TABLE dbo.MetodosPago (
  IdMetodo        INT IDENTITY(1,1) PRIMARY KEY,
  IdProveedorPago INT NOT NULL,
  Config          NVARCHAR(MAX) NULL,
  CONSTRAINT FK_Metodo_ProveedorPago FOREIGN KEY (IdProveedorPago) REFERENCES dbo.ProveedoresPago(IdProveedorPago)
);
GO
CREATE INDEX IX_Metodos_Prov ON dbo.MetodosPago(IdProveedorPago);
GO

CREATE TABLE dbo.Depositos (
  IdDeposito  BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario   INT NOT NULL,
  IdMetodo    INT NOT NULL,
  IdMoneda    INT NOT NULL,
  Monto       DECIMAL(19,4) NOT NULL CHECK (Monto > 0),
  Estado      NVARCHAR(20) NOT NULL, -- pending/confirmed/rejected
  ExternalId  NVARCHAR(100) NULL,
  Ts          DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Dep_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
  CONSTRAINT FK_Dep_Metodo  FOREIGN KEY (IdMetodo)  REFERENCES dbo.MetodosPago(IdMetodo),
  CONSTRAINT FK_Dep_Moneda  FOREIGN KEY (IdMoneda)  REFERENCES dbo.Monedas(IdMoneda)
);
GO
CREATE INDEX IX_Depositos_Usr_Ts ON dbo.Depositos(IdUsuario, Ts DESC);
CREATE UNIQUE INDEX UX_Dep_ExternalId ON dbo.Depositos(ExternalId) WHERE ExternalId IS NOT NULL;
GO

CREATE TABLE dbo.Retiros (
  IdRetiro   BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario  INT NOT NULL,
  IdMetodo   INT NOT NULL,
  IdMoneda   INT NOT NULL,
  Monto      DECIMAL(19,4) NOT NULL CHECK (Monto > 0),
  Estado     NVARCHAR(20) NOT NULL, -- requested/approved/rejected/paid
  Ts         DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Ret_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
  CONSTRAINT FK_Ret_Metodo  FOREIGN KEY (IdMetodo)  REFERENCES dbo.MetodosPago(IdMetodo),
  CONSTRAINT FK_Ret_Moneda  FOREIGN KEY (IdMoneda)  REFERENCES dbo.Monedas(IdMoneda)
);
GO
CREATE INDEX IX_Retiros_Usr_Ts ON dbo.Retiros(IdUsuario, Ts DESC);
GO

CREATE TABLE dbo.WebhooksPago (
  IdWebhook       BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdProveedorPago INT NOT NULL,
  Evento          NVARCHAR(80) NOT NULL,
  Payload         NVARCHAR(MAX) NOT NULL,
  Procesado       BIT NOT NULL DEFAULT(0),
  Ts              DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Wh_ProvPago FOREIGN KEY (IdProveedorPago) REFERENCES dbo.ProveedoresPago(IdProveedorPago)
);
GO
CREATE INDEX IX_Wh_Prov_Ts ON dbo.WebhooksPago(IdProveedorPago, Ts DESC);
GO

-- =======================
-- 6) Cumplimiento, Riesgo & Juego Responsable
-- =======================
CREATE TABLE dbo.KYC_Documentos (
  IdDoc        BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario    INT NOT NULL,
  Tipo         NVARCHAR(40) NOT NULL,
  HashArchivo  VARBINARY(32) NOT NULL,
  Estado       NVARCHAR(20) NOT NULL, -- pending/approved/rejected
  Ts           DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_KYCDoc_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_KYCDoc_Usr_Ts ON dbo.KYC_Documentos(IdUsuario, Ts DESC);
GO

CREATE TABLE dbo.VerificacionesKYC (
  IdVerif    BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario  INT NOT NULL,
  Proveedor  NVARCHAR(80) NOT NULL,
  Resultado  NVARCHAR(40) NOT NULL,
  Ts         DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_KYCVer_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_KYCVer_Usr_Ts ON dbo.VerificacionesKYC(IdUsuario, Ts DESC);
GO

CREATE TABLE dbo.LimitesResponsable (
  IdUsuario         INT NOT NULL PRIMARY KEY,
  DepositoDiarioMax  DECIMAL(19,4) NULL,
  DepositoSemanalMax DECIMAL(19,4) NULL,
  DepositoMensualMax DECIMAL(19,4) NULL,
  PerdidaDiariaMax   DECIMAL(19,4) NULL,
  CONSTRAINT FK_LR_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO

CREATE TABLE dbo.Autoexclusiones (
  IdAuto    BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario INT NOT NULL,
  Desde     DATETIME2(3) NOT NULL,
  Hasta     DATETIME2(3) NOT NULL,
  Motivo    NVARCHAR(200) NULL,
  CONSTRAINT FK_Autoexc_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_Autoexc_Usr_Desde ON dbo.Autoexclusiones(IdUsuario, Desde DESC);
GO

CREATE TABLE dbo.EventosRiesgo (
  IdEvento  BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario INT NOT NULL,
  Tipo      NVARCHAR(40) NOT NULL,
  Score     INT NULL,
  Detalles  NVARCHAR(MAX) NULL,
  Ts        DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Riesgo_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_Riesgo_Usr_Ts ON dbo.EventosRiesgo(IdUsuario, Ts DESC);
GO

CREATE TABLE dbo.ListasBloqueo (
  IdBloqueo BIGINT IDENTITY(1,1) PRIMARY KEY,
  Tipo      NVARCHAR(20) NOT NULL,  -- IP/email/pais/tarjeta
  Valor     NVARCHAR(256) NOT NULL,
  Motivo    NVARCHAR(200) NULL,
  Ts        DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME()
);
GO
CREATE UNIQUE INDEX UX_Bloqueo_Tipo_Valor ON dbo.ListasBloqueo(Tipo, Valor);
GO

-- =======================
-- 7) Operación, Soporte & Auditoría
-- =======================
CREATE TABLE dbo.TicketsSoporte (
  IdTicket   BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario  INT NOT NULL,
  Estado     NVARCHAR(20) NOT NULL,   -- open/pending/closed
  Prioridad  NVARCHAR(20) NULL,
  TsCreacion DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Ticket_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_Tickets_Usr_Ts ON dbo.TicketsSoporte(IdUsuario, TsCreacion DESC);
GO

CREATE TABLE dbo.MensajesTicket (
  IdMensaje BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdTicket  BIGINT NOT NULL,
  DeUsuario BIT NOT NULL,
  Texto     NVARCHAR(MAX) NOT NULL,
  Ts        DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Msg_Ticket FOREIGN KEY (IdTicket) REFERENCES dbo.TicketsSoporte(IdTicket)
);
GO
CREATE INDEX IX_Msg_Ticket_Ts ON dbo.MensajesTicket(IdTicket, Ts ASC);
GO

CREATE TABLE dbo.Notificaciones (
  IdNotif   BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario INT NOT NULL,
  Canal     NVARCHAR(20) NOT NULL,    -- email/sms/push
  Titulo    NVARCHAR(120) NOT NULL,
  Cuerpo    NVARCHAR(MAX) NOT NULL,
  Leida     BIT NOT NULL DEFAULT(0),
  Ts        DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Notif_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_Notif_Usr_Leida_Ts ON dbo.Notificaciones(IdUsuario, Leida, Ts DESC);
GO

CREATE TABLE dbo.Auditorias (
  IdAud     BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario INT NULL, -- puede ser sistema
  Entidad   NVARCHAR(80) NOT NULL,
  IdEntidad NVARCHAR(64) NOT NULL,
  Accion    NVARCHAR(40) NOT NULL,
  Antes     NVARCHAR(MAX) NULL,
  Despues   NVARCHAR(MAX) NULL,
  Ts        DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_Aud_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_Aud_Entidad_Ts ON dbo.Auditorias(Entidad, Ts DESC);
GO

CREATE TABLE dbo.LogsSeguridad (
  IdLog    BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario INT NULL,
  IP       NVARCHAR(64) NULL,
  Evento   NVARCHAR(80) NOT NULL,
  Ts       DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_LogSec_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE INDEX IX_LogSec_Evento_Ts ON dbo.LogsSeguridad(Evento, Ts DESC);
GO

-- =======================
-- 8) Torneos & Misiones
-- =======================
CREATE TABLE dbo.Torneos (
  IdTorneo    INT IDENTITY(1,1) PRIMARY KEY,
  IdJuego     INT NULL, -- opcional si es multijuego
  Nombre      NVARCHAR(120) NOT NULL,
  RangoInicio DATETIME2(3) NOT NULL,
  RangoFin    DATETIME2(3) NOT NULL,
  Reglas      NVARCHAR(MAX) NULL,
  Premios     NVARCHAR(MAX) NULL,
  CONSTRAINT FK_Torneos_Juego FOREIGN KEY (IdJuego) REFERENCES dbo.Juegos(IdJuego)
);
GO

CREATE TABLE dbo.ParticipantesTorneo (
  IdParticipante BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdTorneo       INT NOT NULL,
  IdUsuario      INT NOT NULL,
  Estado         NVARCHAR(20) NOT NULL, -- enrolled/active/eliminated/won
  CONSTRAINT FK_PT_Torneo  FOREIGN KEY (IdTorneo)  REFERENCES dbo.Torneos(IdTorneo),
  CONSTRAINT FK_PT_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario)
);
GO
CREATE UNIQUE INDEX UX_PT_Torneo_Usuario ON dbo.ParticipantesTorneo(IdTorneo, IdUsuario);
GO

CREATE TABLE dbo.Misiones (
  IdMision     INT IDENTITY(1,1) PRIMARY KEY,
  Descripcion  NVARCHAR(200) NOT NULL,
  Recompensa   DECIMAL(19,4) NULL
);
GO

CREATE TABLE dbo.UsuarioMisiones (
  IdUsuarioMision BIGINT IDENTITY(1,1) PRIMARY KEY,
  IdUsuario       INT NOT NULL,
  IdMision        INT NOT NULL,
  Progreso        INT NOT NULL DEFAULT(0),
  Completada      BIT NOT NULL DEFAULT(0),
  Ts              DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT FK_UM_Usuario FOREIGN KEY (IdUsuario) REFERENCES dbo.Usuarios(IdUsuario),
  CONSTRAINT FK_UM_Mision  FOREIGN KEY (IdMision)  REFERENCES dbo.Misiones(IdMision)
);
GO
CREATE UNIQUE INDEX UX_UM_Usr_Mision ON dbo.UsuarioMisiones(IdUsuario, IdMision);
GO


-- Movimientos contables en lote
CREATE TYPE dbo.TVP_Movimiento AS TABLE(
  IdCuenta BIGINT NOT NULL,
  Debe     DECIMAL(19,4) NOT NULL DEFAULT(0),
  Haber    DECIMAL(19,4) NOT NULL DEFAULT(0)
);
GO

-- Detalle de apuesta (genérico)
CREATE TYPE dbo.TVP_ApuestaDetalle AS TABLE(
  Seleccion     NVARCHAR(80) NOT NULL,
  Multiplicador DECIMAL(10,4) NULL  -- 0 = pierde; >0 = factor de pago
);
GO
