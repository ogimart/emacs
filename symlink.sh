#!/bin/bash

DIR=`pwd`
mkdir -p ~/.emacs.d
cd ~
ln -s $DIR/init.el ~/.emacs.d/init.el
ln -s $DIR/config ~/.emacs.d/config

