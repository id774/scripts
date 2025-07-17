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
#  Usage:
#      wget.rb <URL>
#  Example: ruby wget.rb http://example.com/file.txt
#
#  Requirements:
#  - Ruby Version: 2.4 or later
#
#  Version History:
#  v1.2 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.1 2023-12-06
#       Refactored for improved readability and added detailed comments.
#  v1.0 2012-02-29
#       Initial release.
#
########################################################################

require 'open-uri'

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

# Compatibility wrapper for URI.open (Ruby 2.5+) vs Kernel.open (older versions)
def open_uri_compat(url, &block)
  if URI.respond_to?(:open)
    URI.open(url, &block)
  else
    Kernel.open(url, &block)
  end
end

def main
  if ARGV.empty? || ['-h', '--help', '-v', '--version'].include?(ARGV[0])
    usage
  end

  url = ARGV.shift
  filename = url.split(/\//).last

  open_uri_compat(url) do |source|
    File.open(filename, "w+b") do |o|
      o.print source.read
    end
  end
  return 0
end

exit(main) if __FILE__ == $PROGRAM_NAME
