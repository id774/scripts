#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require "stringio"

pngname = ARGF.filename
image = open(pngname, "rb")

sign   = image.read(8)
length = image.read(4).unpack(">N")[0]
name  = image.read(4)
data  = image.read(length)

ihdr = StringIO.new(data)
width       = ihdr.read(4).unpack(">N")[0]
height      = ihdr.read(4).unpack(">N")[0]
bit_depth   = ihdr.read(1).unpack("h")[0].to_i
color_type  = ihdr.read(1).unpack("h")[0].to_i

case color_type
when 3
  color_type_name = "PNG-8"
when 2
  color_type_name = "PNG-24"
when 6
  color_type_name = "PNG-32"
else
  color_type_name = "UNKNOWN"
end

puts "横幅          %4d" % width
puts "高さ          %4d" % height
puts "ビット深度    %4d" % bit_depth
puts "カラータイプ  %s" % color_type_name
