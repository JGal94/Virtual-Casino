from fastapi import APIRouter
from pydantic import BaseModel, Field
from decimal import Decimal
from typing import Optional
from deps.db import get_conn, exec_tsql

router = APIRouter(prefix="/wallet", tags=["wallet"])

class ConfirmIn(BaseModel):
    txhash: str
    network: str
    confirmations: int = Field(ge=0)
    amount_onchain: Optional[Decimal] = Field(default=None, max_digits=38, decimal_places=8)

@router.post("/deposit/confirm")
def confirm_deposit(c: ConfirmIn):
    sql = """
DECLARE @ok bit, @msg nvarchar(200);
EXEC dbo.usp_Crypto_Deposito_RegistrarConfirmacion
  @TxHash=?, @NetworkCode=?, @Confirmations=?, @MontoOnChain=?, @IdCuentaCasaCaja=NULL,
  @Ok=@ok OUTPUT, @Mensaje=@msg OUTPUT;
SELECT @ok, @msg;
"""
    with get_conn() as conn:
        rows = exec_tsql(conn, sql, (c.txhash, c.network, int(c.confirmations), c.amount_onchain))
        return {"ok": bool(rows[0][0]), "message": rows[0][1]}
