#!/bin/sh

GEM=/opt/ruby/current/bin/gem
sudo $GEM uninstall -v 3.0.0 rspec-core rspec-expectations rspec-support
sudo $GEM uninstall -v 3.0.1 rspec-mocks rspec-rails

