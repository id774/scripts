#!/usr/bin/env ruby

require 'digest/md5'
p Digest::MD5.hexdigest(ARGF.read)
