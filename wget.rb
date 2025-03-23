#!/usr/bin/env ruby

########################################################################
# wget.rb: Simple Ruby-based File Downloader
#
#  Description:
#  This Ruby script downloads a file from a given URL and saves it
#  locally. It is a basic implementation similar to the wget command.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Refactored for improved readability and added detailed comments.
#  v1.0 2012-02-29
#       Initial release.
#
#  Usage:
#  ruby wget.rb <URL>
#  Example: ruby wget.rb http://example.com/file.txt
#
########################################################################

require 'open-uri'

# Prints the usage message and exits the script
def usage
  prog = __FILE__
  puts "Usage: #{prog} <URL>"
  exit 1
end

# Check if URL is provided
url = ARGV.shift
usage unless url

# Extract filename from URL
filename = url.split(/\//).last

# Download and save the file
open(url) do |source|
  open(filename, "w+b") do |o|
    o.print source.read
  end
end

