#!/usr/bin/env bash

# ruby-install
mkdir /tmp/ruby-install
cd /tmp
curl https://codeload.github.com/postmodern/ruby-install/tar.gz/v0.6.1 | tar -xz
cd /tmp/ruby-install-0.6.1
sudo make install
rm -rf /tmp/ruby-install

#Ruby
ruby-install --system ruby 2.4.0

#Bundler
gem install bundler -v 1.13.7 --no-ri --no-rdoc