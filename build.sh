#!/bin/sh

emacs -Q --script ./build-site.el

# comment out if you don't want to server
python -m http.server -d public/
