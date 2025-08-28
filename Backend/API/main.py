from fastapi import FastAPI, WebSocket, WebSocketDisconnect
from routers import plinko, admin, wallet   # <- sin "app."
from fastapi.responses import RedirectResponse


app = FastAPI(title="Casino API (Crypto)")

# Routers
app.include_router(plinko.router)
app.include_router(admin.router)
app.include_router(wallet.router)


@app.get("/", include_in_schema=False)
def root():
    return RedirectResponse(url="/docs")

@app.get("/healthz")
def healthz():
    return {"ok": True}


# WebSocket simple (igual que antes)
class Hub:
    def __init__(self):
        self.active: dict[str, set[WebSocket]] = {}
    async def connect(self, key: str, ws: WebSocket):
        await ws.accept()
        self.active.setdefault(key, set()).add(ws)
    def disconnect(self, key: str, ws: WebSocket):
        self.active.get(key, set()).discard(ws)
    async def send(self, key: str, msg: str):
        for ws in list(self.active.get(key, set())):
            try:
                await ws.send_text(msg)
            except:
                self.disconnect(key, ws)

hub = Hub()

@app.websocket("/ws/{mesa_id}")
async def ws_round(mesa_id: int, ws: WebSocket):
    key = f"mesa:{mesa_id}"
    await hub.connect(key, ws)
    try:
        while True:
            _ = await ws.receive_text()
    except WebSocketDisconnect:
        hub.disconnect(key, ws)
