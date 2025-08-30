from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from deps.db import get_conn, exec_tsql

router = APIRouter(prefix="/rounds", tags=["rounds"])

class OpenRoundIn(BaseModel):
    juego_id: int
    mesa_id: int | None = None

@router.post("/open")
def open_round(data: OpenRoundIn):
    tsql = f"""
DECLARE @IdRonda BIGINT;
EXEC dbo.usp_Ronda_Abrir
  @IdJuego={data.juego_id},
  @IdMesa={('NULL' if data.mesa_id is None else data.mesa_id)},
  @IdRonda=@IdRonda OUTPUT;
SELECT @IdRonda AS IdRonda;
"""
    with get_conn() as conn:
        rows = exec_tsql(conn, tsql)
    if not rows:
        raise HTTPException(400, "No se pudo abrir la ronda")
    return {"round_id": int(rows[0][0])}
