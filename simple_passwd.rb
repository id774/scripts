#!/usr/bin/env ruby

########################################################################
# simple_password.rb: Simple Password Generator in Ruby
#
#  Description:
#  This script generates a random password of a specified length.
#  It includes a mix of numbers, lowercase and uppercase letters,
#  and special characters (_, -, !, #, &).
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2013-01-07
#       Initial release.
#
#  Usage:
#  ruby simple_password.rb [length]
#  Default length is 8 if not specified.
#
########################################################################

def generate_passwd(length)
  # Generate a random password
  chars = [*0..9, *'a'..'z', *'A'..'Z', '_', '-', '!', '#', '&']
  puts chars.sample(length).join
end

if __FILE__ == $0
  length = ARGV.shift || 8
  generate_passwd(length.to_i)
end

