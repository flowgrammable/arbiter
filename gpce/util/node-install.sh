#!/usr/bin/env sh
cd ~ && git clone http://github.com/joyent/node.git
cd node
git checkout -b v0.12.2
./configure
sudo make
sudo make install
