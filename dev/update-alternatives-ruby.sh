#!/bin/sh

sudo update-alternatives --remove-all ruby
sudo update-alternatives --remove-all gem
sudo update-alternatives --remove-all irb
sudo update-alternatives --remove-all rake
sudo update-alternatives --remove-all rdoc
sudo update-alternatives --remove-all erb
sudo update-alternatives --remove-all testrb

sudo update-alternatives \
  --install /opt/bin/ruby   ruby   /usr/bin/ruby1.8 90\
  --slave   /opt/bin/gem    gem    /usr/bin/gem1.8\
  --slave   /opt/bin/irb    irb    /usr/bin/irb1.8\
  --slave   /opt/bin/rake   rake   /usr/bin/rake1.8\
  --slave   /opt/bin/rdoc   rdoc   /usr/bin/rdoc1.8\
  --slave   /opt/bin/erb    erb    /usr/bin/erb1.8\
  --slave   /opt/bin/testrb testrb /usr/bin/testrb1.8

sudo update-alternatives \
  --install /opt/bin/ruby   ruby   /usr/local/bin/ruby 100\
  --slave   /opt/bin/gem    gem    /usr/local/bin/gem\
  --slave   /opt/bin/irb    irb    /usr/local/bin/irb\
  --slave   /opt/bin/rake   rake   /usr/local/bin/rake\
  --slave   /opt/bin/rdoc   rdoc   /usr/local/bin/rdoc\
  --slave   /opt/bin/erb    erb    /usr/local/bin/erb\
  --slave   /opt/bin/testrb testrb /usr/local/bin/testrb

sudo update-alternatives \
  --install /opt/bin/ruby   ruby   /opt/ruby/1.9.1/bin/ruby 150\
  --slave   /opt/bin/gem    gem    /opt/ruby/1.9.1/bin/gem\
  --slave   /opt/bin/irb    irb    /opt/ruby/1.9.1/bin/irb\
  --slave   /opt/bin/rake   rake   /opt/ruby/1.9.1/bin/rake\
  --slave   /opt/bin/rdoc   rdoc   /opt/ruby/1.9.1/bin/rdoc\
  --slave   /opt/bin/erb    erb    /opt/ruby/1.9.1/bin/erb\
  --slave   /opt/bin/testrb testrb /opt/ruby/1.9.1/bin/testrb

