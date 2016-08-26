docker run -it --rm --name sdfkit -v /tmp/.X11-unix:/tmp/.X11-unix -v "$(pwd)":/usr/src/app -w /usr/src/app -e DISPLAY=$DISPLAY -u myusername sdfkit python3 mainwindow.py
