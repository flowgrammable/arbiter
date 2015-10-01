#!/usr/bin/env sh
cd ~ && git clone http://github.com/mininet/mininet
cd mininet/util && ./install.sh -f3
cd ~ && wget http://www.weebly.com/uploads/1/3/2/6/13260234/kc-cbench.tgz
tar -xvf kc-cbench.tgz
cd kc-bench
./configure
make
sudo make install
