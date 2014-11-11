#!/usr/bin/env bash

apt-get update
apt-get install -y python-software-properties  build-essential m4
add-apt-repository ppa:avsm/ppa
apt-get update
apt-get install -y ocaml opam python-networkx wireshark pip
cd ../
curl -O http://download.redis.io/releases/redis-2.8.17.tar.gz
tar -xzf redis-2.8.17.tar.gz
cd redis-2.8.17
make
pip install redis
cd ../../vagrant
