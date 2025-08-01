#!/usr/bin/env ruby

########################################################################
# simple_password.rb: Simple Password Generator in Ruby
#
#  Description:
#  This script generates a random password. It can optionally exclude
#  special symbols from the password. If symbols are enabled, at least
#  one symbol is guaranteed to be included in the password.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      simple_password.rb [options] length
#
#  Options:
#    -s, --no-symbols  Do not include symbols in the password
#
#  Requirements:
#  - Ruby Version: 2.0 or later
#
#  Version History:
#  v1.5 2025-07-03
#       Ensure at least one symbol is included when use_symbols is enabled.
#  v1.4 2025-07-01
#       Standardized termination behavior for consistent script execution.
#       Refactored into main function for consistency and maintainability.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2023-12-06
#       Added option to exclude special symbols and improved argument handling.
#  v1.0 2013-01-07
#       Initial release.
#
########################################################################

require 'optparse'

def usage
  script = File.expand_path(__FILE__)
  in_header = false
  File.foreach(script) do |line|
    if line.strip.start_with?('#' * 10)
      in_header = !in_header
      next
    end
    puts line.sub(/^# ?/, '') if in_header && line.strip.start_with?('#')
  end
  exit 0
end

def generate_passwd(length, use_symbols = true)
  if length <= 0
    warn "[ERROR] Length must be greater than zero."
    exit 1
  end

  base_chars = [*'0'..'9', *'a'..'z', *'A'..'Z']
  symbol_chars = ['_', '-', '!', '#', '&']

  if use_symbols
    if length == 1
      puts symbol_chars.sample
      return
    end

    chars = base_chars + symbol_chars
    password = [symbol_chars.sample] + Array.new(length - 1) { chars.sample }
    puts password.shuffle.join
  else
    puts Array.new(length) { base_chars.sample }.join
  end
end

def main
  if ARGV.empty? || ['-h', '--help', '-v', '--version'].include?(ARGV[0])
    usage
  end

  options = { use_symbols: true }
  option_parser = OptionParser.new do |opts|
    opts.banner = "Usage: simple_password.rb [options] length"
    opts.on("-s", "--no-symbols", "Do not include symbols in the password") do
      options[:use_symbols] = false
    end
  end

  option_parser.parse!

  if ARGV.length != 1 || ARGV[0] !~ /\A\d+\z/
    puts "[ERROR] Length must be a number."
    puts option_parser
    return 1
  end

  length = ARGV.shift.to_i
  generate_passwd(length, options[:use_symbols])
  return 0
end

exit(main) if __FILE__ == $0
