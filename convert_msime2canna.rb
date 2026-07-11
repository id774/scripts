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
#  Usage:
#      convert_msime2canna.rb < MS-IME_dictionary_file
#
#  Requirements:
#  - Ruby Version: 2.0 or later
#
#  Version History:
#  v1.5 2026-07-11
#       Specify UTF-8 encoding when usage() reads the script's own source,
#       to avoid an Encoding::CompatibilityError under a non-UTF-8 locale.
#  v1.4 2025-07-13
#       Replaced deprecated Kconv#toutf8 with String#encode for modern Ruby compatibility.
#  v1.3 2025-07-01
#       Standardized termination behavior for consistent script execution.
#       Refactored into main function for consistency and maintainability.
#  v1.2 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.1 2023-12-06
#       Refactored for improved readability and added detailed comments.
#  v1.0 2009-11-16
#       Initial release.
#
########################################################################

def usage
  script = File.expand_path(__FILE__)
  in_header = false
  File.foreach(script, encoding: 'UTF-8') do |line|
    if line.strip.start_with?('#' * 10)
      in_header = !in_header
      next
    end
    puts line.sub(/^# ?/, '') if in_header && line.strip.start_with?('#')
  end
  exit 0
end

def main
  if ARGV[0] && ['-h', '--help', '-v', '--version'].include?(ARGV[0])
    usage
  end

  while line = gets
    str = line.split(/\t/)
    next unless str[2]
    next unless str[2].encode("UTF-8", invalid: :replace, undef: :replace, replace: '').chomp == "顔文字"

    print str[0].encode("UTF-8", invalid: :replace, undef: :replace, replace: '')
    print " #KJ "
    puts str[1].encode("UTF-8", invalid: :replace, undef: :replace, replace: '').gsub(/ /, "\\ ")
  end

  return 0
end

exit(main) if __FILE__ == $0
