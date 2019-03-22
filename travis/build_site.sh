#!/bin/bash

emacs --version
emacs --batch -f org-version --kill
cd travis
emacs --script install.el
emacs --script generate-html.el
emacs --batch -f org-version --kill
cd ../
mkdir -p deploy
# mv TODO.html deploy/todo.html
mv ~/.emacs.d/emacs.html deploy/index.html
cp travis/readtheorg.css deploy/
