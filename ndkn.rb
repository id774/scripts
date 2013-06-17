# ndkn.rb - Nandoku ruby
# Author: Sora Harakami
# Licence: Public Licence

require 'base64'

abort "usage: #{File.basename(__FILE__)} str" if ARGV.size < 1

str_raw = ARGV[0]
r = rand(10)+1
str = Base64.encode64(Base64.encode64(str_raw.chars.to_a.map(&:ord)
                                                   .inspect).chars
                                                   .map(&:ord)
                                                   .map{|x| x+r }
                                                   .map(&:chr).join)

code = ""


code << "(require 'base64'; "
code << "eval(Base64.decode64(Base64.decode64('"
code << str.chomp
code << "').chars.map{|x| x.ord - #{r} }.map(&:chr).join)).map(&:chr).join(''))"

puts code

