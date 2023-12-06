#!/usr/bin/env ruby

########################################################################
# rubyping.rb: Ping a Range of IP Addresses in a Subnet
#
#  Description:
#  This script pings a range of IP addresses within a specified subnet.
#  It's useful for quickly checking the status of multiple IPs.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Refactored for argument checking and added usage instructions.
#  v1.0 2008-08-22
#       Initial release.
#
#  Usage:
#  rubyping.rb <subnet> <start_ip> <end_ip>
#  Example: rubyping.rb 192.168.11. 1 32
#
#  Notes:
#  - Ensure you have permissions to send pings to the target IPs.
#  - This script may take time to complete based on the range specified.
#
########################################################################

if ARGV.length != 3
  puts "Usage: rubyping.rb <subnet> <start_ip> <end_ip>"
  puts "Example: rubyping.rb 192.168.11. 1 32"
  exit
end

SUBNET = ARGV[0]
FROM = ARGV[1].to_i
TO = ARGV[2].to_i
$status = Hash.new
$threads = Hash.new

# Ping each IP in the specified range
(TO).downto(FROM) do |n|
  ip = SUBNET + n.to_s
  $status[ip] = '-----'
  $threads[n] = Thread.start do 
    $status[ip] = 'alive' if system("ping -c 1 -i 1 #{ip} > /dev/null")
  end
end

# Wait for all threads to complete
$threads.values.each(&:join)

# Output the results
FROM.upto(TO) do |n|
  ip = SUBNET + n.to_s
  puts "#{ip} --> #{$status[ip]}"
end

