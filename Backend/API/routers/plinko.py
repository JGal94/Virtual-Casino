from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field
from typing import Literal
from decimal import Decimal
from deps.db import get_conn, exec_tsql   # <- relativo a tu raíz API

router = APIRouter(prefix="/plinko", tags=["plinko"])

class BetIn(BaseModel):
    user_id: int
    ronda_id: int
    moneda_id: int
    amount: Decimal = Field(gt=0, max_digits=38, decimal_places=8)
    risk: Literal["LOW","MED","HIGH"] = "MED"
    rows: int = 16
    client_seed: str = "web"

@router.post("/bet")
def place_bet(b: BetIn):
    seed = b.client_seed.replace("'", "''")
    risk = b.risk.replace("'", "''")
    sql = f"""
DECLARE @det dbo.TVP_ApuestaDetalle;
INSERT INTO @det(Seleccion,Multiplicador)
VALUES (N'plinko|risk={risk}|rows={b.rows}|client={seed}', NULL);

DECLARE @IdApuesta BIGINT;

-- variables para params del EXEC (evita subconsultas en la lista)
DECLARE @TT_Apuesta SMALLINT =
  (SELECT IdTipo FROM dbo.TiposTransaccion WHERE Nombre=N'apuesta');

DECLARE @CtaCasaJuego BIGINT =
  (SELECT IdCuenta
     FROM dbo.Cuentas
    WHERE IdUsuario = (SELECT IdUsuario FROM dbo.Usuarios WHERE Email=N'house@casino.local')
      AND IdMoneda  = {b.moneda_id});

EXEC dbo.usp_Apuesta_Colocar
  @IdUsuario={b.user_id},
  @IdRonda={b.ronda_id},
  @IdMoneda={b.moneda_id},
  @Importe={b.amount},
  @IdTipoAsiento=@TT_Apuesta,
  @IdCuentaCasaJuego=@CtaCasaJuego,
  @Detalle=@det,
  @IdApuesta=@IdApuesta OUTPUT;

SELECT @IdApuesta AS IdApuesta;
"""
    with get_conn() as conn:
        rows = exec_tsql(conn, sql)
        if not rows:
            raise HTTPException(400, "No se pudo colocar la apuesta")
        return {"bet_id": int(rows[0][0])}
