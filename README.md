# Leimen Emacs
Modern code development in almost every language with AI assistance with Emacs, as well as system administration with Emacs.

## Key features
- Use smart code completion using any local llm supported by ollama
- Use emacs as terminal multiplexer (with vterm, multi vterm)
- Programm in almost every lanaguage (with lsp-mode)
- Keep workflow and look and feel close for different languages and admin use-cases
- No graphic environment required, just a terminal

## Getting started
- Install emacs
  - e.g. `sudo snap --classic install emacs` in a snap environment
- Setup as default emacs config
  - `cd ~ && git clone https://github.com/pweibert/leimen-emacs.git`
  - `mv .emacs.d .emacs.d_backup`
  - `mv leimen-emacs .emacs.d && cd .emacs.d`
  - `./install_dependencies.sh`
- When opening a file in an unknown language emacs lsp package will propmpt you for the language server you want to use, download and install it on your system automatically e.g. ts-ls language server can be used for typescript development.
