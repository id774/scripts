#!/usr/bin/env ruby

require 'open-uri'

def usage
  prog = __FILE__
  puts "Usage: #{prog} <URL>"
  exit 1
end

url = ARGV.shift
usage unless url

filename = url.split(/\//).last

open(url) do |source|
  open(filename, "w+b") do |o|
    o.print source.read
  end
end
