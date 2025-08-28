import os, pyodbc
from contextlib import contextmanager
from dotenv import load_dotenv

load_dotenv()
CONN_STR = os.getenv("DB_DSN")

@contextmanager
def get_conn():
    conn = pyodbc.connect(CONN_STR, autocommit=False)
    try:
        yield conn
        conn.commit()
    except:
        conn.rollback()
        raise
    finally:
        conn.close()

def exec_tsql(conn, sql: str, params: tuple = ()):
    cur = conn.cursor()
    cur.execute(sql, params)
    try:
        rows = cur.fetchall()
    except pyodbc.ProgrammingError:
        rows = []
    return rows
