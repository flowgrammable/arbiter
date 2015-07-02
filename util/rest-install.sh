#!/usr/bin/env sh
sudo add-apt-repository ppa:boost-latest/ppa -y
sudo apt-get update
sudo apt-get install git g++ make libboost1.54-all-dev libssl-dev cmake -y
sudo ldconfig
cd ~ && git clone https://git.codeplex.com/casablanca
cd casablanca/Release
mkdir build.release
cd build.release
sudo CXX=g++-4.9 cmake .. -DCMAKE_BUILD_TYPE=Release
sudo make
