/* ========== (Opcional) Crear BD y usarla ========== */
IF DB_ID('CasinoDB') IS NULL
BEGIN
  CREATE DATABASE CasinoDB;
END
GO
USE CasinoDB;
GO

/* ========== Esquema lógico ========== */
IF SCHEMA_ID('casino') IS NULL
  EXEC ('CREATE SCHEMA casino AUTHORIZATION dbo;');
GO

/* ========== Identidad y cumplimiento ========== */
CREATE TABLE casino.Usuario (
  UsuarioId UNIQUEIDENTIFIER NOT NULL 
    CONSTRAINT PK_Usuario PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  Email NVARCHAR(256) NOT NULL CONSTRAINT UQ_Usuario_Email UNIQUE,
  HashPassword VARBINARY(256) NOT NULL,
  Estado TINYINT NOT NULL CONSTRAINT CK_Usuario_Estado CHECK (Estado IN (1,2,3)),
  FechaCreacion DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  FechaUltimoLogin DATETIME2(3) NULL
);

CREATE TABLE casino.Rol (
  RolId TINYINT NOT NULL CONSTRAINT PK_Rol PRIMARY KEY,
  Nombre NVARCHAR(50) NOT NULL CONSTRAINT UQ_Rol_Nombre UNIQUE
);

CREATE TABLE casino.UsuarioRol (
  UsuarioId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_UsuarioRol_Usuario REFERENCES casino.Usuario(UsuarioId),
  RolId TINYINT NOT NULL
    CONSTRAINT FK_UsuarioRol_Rol REFERENCES casino.Rol(RolId),
  CONSTRAINT PK_UsuarioRol PRIMARY KEY (UsuarioId, RolId)
);

CREATE TABLE casino.UsuarioAutoexclusion (
  AutoexclusionId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT PK_UsuarioAutoexclusion PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  UsuarioId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_Autoex_Usuario REFERENCES casino.Usuario(UsuarioId),
  Desde DATETIME2(3) NOT NULL,
  Hasta DATETIME2(3) NOT NULL,
  Motivo NVARCHAR(200) NULL
);
CREATE INDEX IX_Autoex_Usuario_Hasta ON casino.UsuarioAutoexclusion(UsuarioId, Hasta);

CREATE TABLE casino.VerificacionToken (
  TokenId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT PK_VerificacionToken PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  UsuarioId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_VerifToken_Usuario REFERENCES casino.Usuario(UsuarioId),
  Tipo NVARCHAR(30) NOT NULL,
  Token NVARCHAR(200) NOT NULL,
  ExpiraEn DATETIME2(3) NOT NULL,
  Usado BIT NOT NULL DEFAULT 0,
  FechaCreacion DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME()
);
CREATE INDEX IX_VerifToken_Busqueda ON casino.VerificacionToken(UsuarioId, Tipo, Usado, ExpiraEn);

CREATE TABLE casino.AuditLog (
  AuditId BIGINT IDENTITY(1,1) NOT NULL CONSTRAINT PK_AuditLog PRIMARY KEY,
  Entidad NVARCHAR(60) NOT NULL,
  EntidadId UNIQUEIDENTIFIER NULL,
  UsuarioId UNIQUEIDENTIFIER NULL,
  Accion NVARCHAR(40) NOT NULL,
  Datos NVARCHAR(MAX) NULL
    CONSTRAINT CK_AuditLog_Datos_JSON CHECK (Datos IS NULL OR ISJSON(Datos)=1),
  Fecha DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME()
);

/* ========== Catálogo de Monedas y Wallets ========== */
CREATE TABLE casino.Moneda (
  MonedaId TINYINT NOT NULL CONSTRAINT PK_Moneda PRIMARY KEY,
  Codigo CHAR(6) NOT NULL CONSTRAINT UQ_Moneda_Codigo UNIQUE,
  Decimales TINYINT NOT NULL CONSTRAINT CK_Moneda_Dec CHECK (Decimales BETWEEN 0 AND 18)
);

CREATE TABLE casino.Wallet (
  WalletId UNIQUEIDENTIFIER NOT NULL 
    CONSTRAINT PK_Wallet PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  UsuarioId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_Wallet_Usuario REFERENCES casino.Usuario(UsuarioId),
  MonedaId TINYINT NOT NULL
    CONSTRAINT FK_Wallet_Moneda REFERENCES casino.Moneda(MonedaId),
  Estado TINYINT NOT NULL CONSTRAINT CK_Wallet_Estado CHECK (Estado IN (1,2)),
  CONSTRAINT UQ_Wallet_Usuario_Moneda UNIQUE (UsuarioId, MonedaId)
);
CREATE INDEX IX_Wallet_Usuario ON casino.Wallet(UsuarioId);

/* ========== Ledger (doble partida) ========== */
CREATE TABLE casino.LedgerTransaccion (
  TxId UNIQUEIDENTIFIER NOT NULL 
    CONSTRAINT PK_LedgerTx PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  Tipo NVARCHAR(40) NOT NULL,
  Estado TINYINT NOT NULL CONSTRAINT CK_LedgerTx_Estado CHECK (Estado IN (0,1,2)),
  RefExterna NVARCHAR(100) NULL,
  FechaCreacion DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME()
);
CREATE INDEX IX_LedgerTx_TipoEstado ON casino.LedgerTransaccion(Tipo, Estado, FechaCreacion);

CREATE TABLE casino.LedgerLinea (
  LineaId BIGINT IDENTITY(1,1) NOT NULL 
    CONSTRAINT PK_LedgerLinea PRIMARY KEY,
  TxId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_LedgerLinea_Tx REFERENCES casino.LedgerTransaccion(TxId),
  WalletId UNIQUEIDENTIFIER NULL
    CONSTRAINT FK_LedgerLinea_Wallet REFERENCES casino.Wallet(WalletId),
  MonedaId TINYINT NOT NULL
    CONSTRAINT FK_LedgerLinea_Moneda REFERENCES casino.Moneda(MonedaId),
  Importe DECIMAL(38,18) NOT NULL,
  Concepto NVARCHAR(120) NULL
);
CREATE INDEX IX_LedgerLinea_WalletMoneda ON casino.LedgerLinea(WalletId, MonedaId);
CREATE INDEX IX_LedgerLinea_Tx ON casino.LedgerLinea(TxId);
GO

/* ========== Vista de balance (debe iniciar batch) ========== */
CREATE OR ALTER VIEW casino.v_WalletBalance
AS
SELECT wl.WalletId,
       CAST(ISNULL(SUM(ln.Importe),0) AS DECIMAL(38,18)) AS Balance
FROM casino.Wallet wl
LEFT JOIN casino.LedgerLinea ln
  ON ln.WalletId = wl.WalletId
GROUP BY wl.WalletId;
GO

/* ========== Crypto on/off ramp ========== */
CREATE TABLE casino.CryptoDireccion (
  CryptoId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT PK_CryptoDireccion PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  WalletId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_Crypto_Wallet REFERENCES casino.Wallet(WalletId),
  Cadena NVARCHAR(30) NOT NULL,
  Direccion NVARCHAR(100) NOT NULL,
  TagMemo NVARCHAR(100) NULL,
  Unica BIT NOT NULL DEFAULT 1,
  CONSTRAINT UQ_Crypto_Uniq UNIQUE (WalletId, Cadena, Direccion)
);
CREATE INDEX IX_CryptoDireccion_Lookup ON casino.CryptoDireccion(Cadena, Direccion);

CREATE TABLE casino.Deposito (
  DepositoId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT PK_Deposito PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  WalletId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_Deposito_Wallet REFERENCES casino.Wallet(WalletId),
  MonedaId TINYINT NOT NULL
    CONSTRAINT FK_Deposito_Moneda REFERENCES casino.Moneda(MonedaId),
  Cadena NVARCHAR(30) NOT NULL,
  Direccion NVARCHAR(100) NOT NULL,
  TxHash NVARCHAR(120) NULL,
  Confirmaciones INT NULL,
  Estado TINYINT NOT NULL CONSTRAINT CK_Deposito_Estado CHECK (Estado IN (0,1,2)),
  FechaCreacion DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME()
);
CREATE INDEX IX_Deposito_WalletEstado ON casino.Deposito(WalletId, Estado);

CREATE TABLE casino.Retiro (
  RetiroId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT PK_Retiro PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  WalletId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_Retiro_Wallet REFERENCES casino.Wallet(WalletId),
  MonedaId TINYINT NOT NULL
    CONSTRAINT FK_Retiro_Moneda REFERENCES casino.Moneda(MonedaId),
  Cadena NVARCHAR(30) NOT NULL,
  Direccion NVARCHAR(150) NOT NULL,
  TxHash NVARCHAR(120) NULL,
  Estado TINYINT NOT NULL CONSTRAINT CK_Retiro_Estado CHECK (Estado IN (0,1,2,3)),
  FechaCreacion DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME()
);
CREATE INDEX IX_Retiro_WalletEstado ON casino.Retiro(WalletId, Estado);

/* ========== Juegos 1 jugador (Plinko/Slots) ========== */
CREATE TABLE casino.Juego (
  JuegoId SMALLINT NOT NULL CONSTRAINT PK_Juego PRIMARY KEY,
  Nombre NVARCHAR(80) NOT NULL CONSTRAINT UQ_Juego_Nombre UNIQUE,
  Activo BIT NOT NULL DEFAULT 1
);

CREATE TABLE casino.Ronda (
  RondaId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT PK_Ronda PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  JuegoId SMALLINT NOT NULL
    CONSTRAINT FK_Ronda_Juego REFERENCES casino.Juego(JuegoId),
  SeedServidor VARBINARY(32) NULL,
  SeedCliente VARBINARY(32) NULL,
  Nonce INT NOT NULL DEFAULT 0,
  FechaInicio DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  FechaCierre DATETIME2(3) NULL
);

CREATE TABLE casino.RondaResultado (
  RondaResultadoId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT PK_RondaResultado PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  RondaId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_RRes_Ronda REFERENCES casino.Ronda(RondaId),
  Resultado NVARCHAR(200) NOT NULL,
  Payload NVARCHAR(MAX) NULL
    CONSTRAINT CK_RRes_Payload_JSON CHECK (Payload IS NULL OR ISJSON(Payload)=1),
  Fecha DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT UQ_RRes_Ronda UNIQUE (RondaId)
);

CREATE TABLE casino.Apuesta (
  ApuestaId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT PK_Apuesta PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  UsuarioId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_Apuesta_Usuario REFERENCES casino.Usuario(UsuarioId),
  RondaId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_Apuesta_Ronda REFERENCES casino.Ronda(RondaId),
  MonedaId TINYINT NOT NULL
    CONSTRAINT FK_Apuesta_Moneda REFERENCES casino.Moneda(MonedaId),
  Importe DECIMAL(38,18) NOT NULL,
  Estado TINYINT NOT NULL CONSTRAINT CK_Apuesta_Estado CHECK (Estado IN (0,1,2,3)),
  Multiplicador DECIMAL(18,8) NULL,
  Ganancia DECIMAL(38,18) NULL,
  FechaApuesta DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME()
);
CREATE INDEX IX_Apuesta_UsuarioEstado ON casino.Apuesta(UsuarioId, Estado);
CREATE INDEX IX_Apuesta_Ronda ON casino.Apuesta(RondaId);

CREATE TABLE casino.ApuestaPago (
  PagoId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT PK_ApuestaPago PRIMARY KEY DEFAULT NEWSEQUENTIALID(),
  ApuestaId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_Apago_Apuesta REFERENCES casino.Apuesta(ApuestaId),
  TxId UNIQUEIDENTIFIER NOT NULL
    CONSTRAINT FK_Apago_Tx REFERENCES casino.LedgerTransaccion(TxId),
  Monto DECIMAL(38,18) NOT NULL,
  Fecha DATETIME2(3) NOT NULL DEFAULT SYSUTCDATETIME(),
  CONSTRAINT UQ_Apago_Apuesta UNIQUE (ApuestaId)
);
GO
