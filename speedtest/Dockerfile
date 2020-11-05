FROM python:3.8

WORKDIR /app

COPY requirements.txt .
RUN pip install -r requirements.txt

COPY download_speedtest_cli.sh .
RUN ./download_speedtest_cli.sh

COPY . /app
CMD gunicorn -k flask_sockets.worker --reload --bind 'speedtest:8000' main:app
