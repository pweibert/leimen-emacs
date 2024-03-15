#!/bin/sh
# lsp mode python language support
python3 -m pip install python-language-server
python3 -m pip install python-lsp-server

# lsp mode python debugging
python3 -m pip install debugpy

# requirements to build vterm
sudo apt install libtool
sudo apt install libtool-bin

# requirement for lsp Dokerfile language-server
sudo apt install nodejs
