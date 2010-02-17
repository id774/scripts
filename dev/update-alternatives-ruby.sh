#!/bin/sh

sudo update-alternatives --remove-all ruby
sudo update-alternatives --remove-all gem
sudo update-alternatives --remove-all irb
sudo update-alternatives --remove-all rake
sudo update-alternatives --remove-all rdoc
sudo update-alternatives --remove-all erb
sudo update-alternatives --remove-all testrb

sudo update-alternatives \
  --install /usr/local/bin/ruby   ruby   /usr/bin/ruby1.8 100\
  --slave   /usr/local/bin/gem    gem    /usr/bin/gem1.8\
  --slave   /usr/local/bin/irb    irb    /usr/bin/irb1.8\
  --slave   /usr/local/bin/rake   rake   /usr/bin/rake1.8\
  --slave   /usr/local/bin/rdoc   rdoc   /usr/bin/rdoc1.8\
  --slave   /usr/local/bin/erb    erb    /usr/bin/erb1.8\
  --slave   /usr/local/bin/testrb testrb /usr/bin/testrb1.8

sudo update-alternatives \
  --install /usr/local/bin/ruby   ruby   /opt/ruby/1.8.7/bin/ruby 120\
  --slave   /usr/local/bin/gem    gem    /opt/ruby/1.8.7/bin/gem\
  --slave   /usr/local/bin/irb    irb    /opt/ruby/1.8.7/bin/irb\
  --slave   /usr/local/bin/rake   rake   /opt/ruby/1.8.7/bin/rake\
  --slave   /usr/local/bin/rdoc   rdoc   /opt/ruby/1.8.7/bin/rdoc\
  --slave   /usr/local/bin/erb    erb    /opt/ruby/1.8.7/bin/erb\
  --slave   /usr/local/bin/testrb testrb /opt/ruby/1.8.7/bin/testrb

sudo update-alternatives \
  --install /usr/local/bin/ruby   ruby   /opt/ruby/1.9.1/bin/ruby 150\
  --slave   /usr/local/bin/gem    gem    /opt/ruby/1.9.1/bin/gem\
  --slave   /usr/local/bin/irb    irb    /opt/ruby/1.9.1/bin/irb\
  --slave   /usr/local/bin/rake   rake   /opt/ruby/1.9.1/bin/rake\
  --slave   /usr/local/bin/rdoc   rdoc   /opt/ruby/1.9.1/bin/rdoc\
  --slave   /usr/local/bin/erb    erb    /opt/ruby/1.9.1/bin/erb\
  --slave   /usr/local/bin/testrb testrb /opt/ruby/1.9.1/bin/testrb

