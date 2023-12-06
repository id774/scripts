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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Refactored for improved readability and added detailed comments.
#  v1.0 2009-11-16
#       Initial release.
#
#  Usage:
#  ruby convert_msime2canna.rb < MS-IME_dictionary_file
#
########################################################################

require 'kconv'

while line = gets
  # Split the line into components
  str = line.split(/\t/)

  # Skip lines without a third column or not labeled as "顔文字"
  next unless str[2]
  next unless str[2].toutf8.chop == "顔文字"

  # Output the converted line in Canna format
  print str[0].toutf8  # Original word
  print " #KJ "       # Canna keyword
  puts str[1].toutf8.gsub(/ /, "\\ ")  # Converted reading with escaped spaces
end

