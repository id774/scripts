#!/usr/bin/env ruby

########################################################################
# simple_password.rb: Simple Password Generator in Ruby
#
#  Description:
#  This script generates a random password. It can optionally exclude
#  special symbols from the password.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Added option to exclude special symbols and improved argument handling.
#  v1.0 2013-01-07
#       Initial release.
#
#  Usage:
#  ruby simple_password.rb [options] length
#  Options:
#    -s, --no-symbols: Do not include symbols in the password
#
########################################################################

require 'optparse'

def generate_passwd(length, use_symbols=true)
  chars = [*'0'..'9', *'a'..'z', *'A'..'Z']
  chars += ['_', '-', '!', '#', '&'] if use_symbols
  puts chars.sample(length).join
end

options = { use_symbols: true }
option_parser = OptionParser.new do |opts|
  opts.banner = "Usage: simple_password.rb [options] length"
  opts.on("-s", "--no-symbols", "Do not include symbols in the password") do
    options[:use_symbols] = false
  end
end

option_parser.parse!

if ARGV.length != 1 || !/\A\d+\z/.match?(ARGV[0])
  puts "Error: Length must be a number."
  puts option_parser
  exit
end

length = ARGV.shift.to_i
generate_passwd(length, options[:use_symbols])

