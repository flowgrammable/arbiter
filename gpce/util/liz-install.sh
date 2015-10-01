#!/usr/bin/env sh
cd /vagrant/liz_arbiter_snapshot
mkdir build
cd build
../configure --with-lpsolve=/opt/lpsolve
sudo make
sudo make install
