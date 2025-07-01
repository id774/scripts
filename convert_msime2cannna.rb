#!/usr/bin/env ruby

########################################################################
# convert_msime2canna.rb: Convert MS-IME Dictionary to Canna Format
#
#  Description:
#  This script converts a Microsoft IME dictionary file to a format
#  compatible with the Canna Japanese input method system. It specifically
#  targets entries labeled as "顔文字" (emoji) and formats them accordingly.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.2 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.1 2023-12-06
#       Refactored for improved readability and added detailed comments.
#  v1.0 2009-11-16
#       Initial release.
#
#  Usage:
#  ruby convert_msime2canna.rb < MS-IME_dictionary_file
#
########################################################################

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

if ARGV[0] && ['-h', '--help', '-v', '--version'].include?(ARGV[0])
  usage
end

require 'kconv'

while line = gets
  str = line.split(/\t/)
  next unless str[2]
  next unless str[2].toutf8.chop == "顔文字"

  print str[0].toutf8
  print " #KJ "
  puts str[1].toutf8.gsub(/ /, "\\ ")
end

exit 0
