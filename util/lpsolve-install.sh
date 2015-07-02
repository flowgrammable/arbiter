#!/usr/bin/env sh
cd ~
wget http://sourceforge.net/projects/lpsolve/files/lpsolve/5.5.2.0/lp_solve_5.5.2.0_dev_ux64.tar.gz
sudo mkdir -p /opt/lpsolve/lib
sudo mkdir -p /opt/lpsolve/include/lpsolve
tar xvf lp_solve_5.5.2.0_dev_ux64.tar.gz
sudo mv lib* /opt/lpsolve/lib
sudo mv *.h /opt/lpsolve/include/lpsolve
sudo touch /etc/ld.so.conf.d/lpsolve.conf
sudo echo '/opt/lpsolve/lib/' >> /etc/ld.so.conf.d/lpsolve.conf
sudo ldconfig
