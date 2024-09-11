# Leimen Emacs

Modern code development every programming with AI assistance and system administration with Emacs. 

## Key features
- Use emacs as terminal multiplexer (with vterm, multi vterm)
- Programm in almost every lanaguage (with lsp-mode)
- Keep workflow and look and feel close for different languages and admin use-cases
- No graphic environment required, just a terminal

## Getting started
- Install emacs
  - e.g. `sudo snap --classic install emacs` in a snap environment
- Setup as default emacs config
  - `cd ~ && git clone https://github.com/pweibert/pauls-emacs-config.git`
  - `mv .emacs.d .emacs.d_backup`
  - `mv pauls-emacs-config .emacs.d && cd .emacs.d`
  - `./install_dependencies.sh`
- When opening a file in a unknown language emacs lsp package will propmpt you for the language server you want to use, download and install it on your system automatically e.g. ts-ls language server can be used for typescript development.    
