import json
from fastapi import APIRouter
from deps.db import get_conn, exec_tsql
from services.rng import pf_roll, plinko_slot, plinko_multiplier, server_seed_hash

router = APIRouter(prefix="/admin", tags=["admin"])

@router.post("/plinko/resolve/{ronda_id}")
def resolve_round(ronda_id: int, risk: str="MED", rows: int=16, client_seed: str="srv", nonce: int | None=None):
    if nonce is None:
        nonce = ronda_id
    r = pf_roll(client_seed, nonce)
    slot = plinko_slot(rows, r)
    mult = plinko_multiplier(risk, rows, slot)

    payload = json.dumps({
        "game":"plinko","risk":risk,"rows":rows,"slot":slot,"multiplier":mult,
        "serverSeedHash": server_seed_hash(),"clientSeed": client_seed,"nonce": nonce
    })

    sql = f"""
EXEC dbo.usp_Ronda_RegistrarResultado @IdRonda={ronda_id}, @Payload=N'{payload.replace("'", "''")}';

UPDATE d
SET d.Multiplicador = {mult},
    d.Seleccion = CONCAT(d.Seleccion, N'|slot={slot}')
FROM dbo.ApuestaDetalle d
JOIN dbo.Apuestas a ON a.IdApuesta=d.IdApuesta
WHERE a.IdRonda={ronda_id} AND a.Estado=N'placed';

EXEC dbo.usp_Ronda_Liquidar
  @IdRonda={ronda_id},
  @IdTipoAsientoPremio=(SELECT IdTipo FROM dbo.TiposTransaccion WHERE Nombre=N'premio'),
  @IdCuentaCasaJuego=(SELECT IdCuenta FROM dbo.Cuentas WHERE IdUsuario=(SELECT IdUsuario FROM dbo.Usuarios WHERE Email=N'house@casino.local') AND IdMoneda=(SELECT TOP(1) IdMoneda FROM dbo.Apuestas WHERE IdRonda={ronda_id}));
"""
    with get_conn() as conn:
        exec_tsql(conn, sql)
    return {"slot": slot, "multiplier": mult}
