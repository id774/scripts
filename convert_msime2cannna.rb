#!/usr/bin/env ruby
$:.unshift File.join(ENV['SCRIPTS'], 'lib') unless ENV['SCRIPTS'] == nil
$:.unshift File.join(File.dirname(__FILE__), 'lib')

require 'kconv'

while line = gets

  str = line.split(/\t/)

  next unless str[2]
  next unless str[2].toutf8.chop == "顔文字"

  print str[0].toutf8
  print " #KJ "
  puts str[1].toutf8.gsub(/ /, "\\ ")

end
