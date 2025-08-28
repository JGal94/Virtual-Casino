import os, hmac, hashlib, math
SERVER_SEED = os.getenv("SERVER_SEED", "dev-seed")

def pf_roll(client_seed: str, nonce: int) -> float:
    msg = f"{client_seed}:{nonce}".encode()
    h = hmac.new(SERVER_SEED.encode(), msg, hashlib.sha256).hexdigest()
    return int(h[:13], 16) / float(0x1_0000_0000_0000)  # [0,1)

def plinko_slot(rows: int, r: float) -> int:
    total = 2 ** rows
    acc = 0.0
    for k in range(rows + 1):
        acc += math.comb(rows, k) / total
        if r <= acc:
            return k
    return rows

PAYOUT = {
  "MED": {16: [0.3,0.5,0.8,0.9,1,1.2,1.7,2.5,5.6,2.5,1.7,1.2,1,0.9,0.8,0.5,0.3]}
}
def plinko_multiplier(risk: str, rows: int, slot: int) -> float:
    return float(PAYOUT[risk][rows][slot])

def server_seed_hash() -> str:
    return hashlib.sha256(SERVER_SEED.encode()).hexdigest()
