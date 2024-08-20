#!/bin/sh

# Check if on Ubuntu
DISTRO=$(lsb_release -i | grep -i -o Ubuntu | tr '[:upper:]' '[:lower:]')

if [ -z $(which python3) ];
then
    echo "Error: No python3 binary found on your system"
    exit 1
fi

# lsp mode python language support
python3 -m pip install python-language-server
python3 -m pip install python-lsp-server
python3 -m pip install flake8

# lsp mode python debugging
python3 -m pip install debugpy

# requirements to build vterm
sudo apt install libtool
sudo apt install libtool-bin

# requirement for lsp Dokerfile language-server
sudo apt install nodejs

