FROM python:3-onbuild
CMD [ "python", "./mainwindow.py" ]

WORKDIR /usr/src/app

RUN apt-get update -q && apt-get install -qy --no-install-recommends \
    python3-pyqt5 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN adduser --quiet --disabled-password myusername
