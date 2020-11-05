from flask import render_template
from app import app, sockets
from .speedtest import speedtest

import time

@app.route('/')
def index():
    return render_template('index.html')

@sockets.route('/ws/speedtest')
def handle_sockets(ws):
    for msg in speedtest():
        ws.send(msg)
