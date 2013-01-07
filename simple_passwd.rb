#!/usr/bin/env ruby

def generate_passwd(length)
  puts [*0..9, *'a'..'z', *'A'..'Z', '_', '-', '!', '#', '&'].sample(length).join
end

if __FILE__ == $0
  len=ARGV.shift || 8; length=len.to_i
  generate_passwd(length)
end
