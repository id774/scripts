#!/usr/bin/env ruby

########################################################################
# waitlock.rb: File Locking Script
#
#  Description:
#  This Ruby script implements a simple file locking mechanism. It is designed
#  to periodically check for the existence of a specified lock file and wait
#  until the lock file is removed. This script is useful for coordinating tasks
#  that should not run simultaneously, such as when multiple processes or tasks
#  need to access a shared resource or file but should not do so at the same time.
#  By checking for a lock file, the script ensures that it only proceeds once
#  the resource is no longer being used by another process.
#
#  The script takes two arguments: the name of the lock file and the check
#  interval in seconds. While the lock file exists, the script will repeatedly
#  check for its presence at the specified interval. Once the lock file is
#  removed, the script stops waiting and terminates.
#
#  Example Usage:
#  To use the script, pass the lock file name and the check interval as arguments.
#  For instance, to set the script to check for 'hoge.txt' every 1 second:
#
#      ./waitlock.rb hoge.txt 1
#
#  This command will cause the script to check for a lock file named 'hoge.txt'
#  every 1 second. The script will continue to run (and not perform further
#  actions) as long as 'hoge.txt' exists. Once 'hoge.txt' is removed, the script
#  will complete its execution.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2023-11-29
#       Refactored the script for improved readability and maintainability.
#       Removed unnecessary class structure and streamlined command-line argument processing.
#       Added clear usage instructions and feedback messages.
#  v1.1 2014-08-14
#       Minor formatting and style revisions.
#  v1.0 2010-12-01
#       Initial release. Basic functionality for file locking with check intervals.
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

# Check for the existence of a lock file at regular intervals
def wait_for_lock_release(lockfile, interval)
  while File.exist?(lockfile)
    puts "Waiting for lock file #{lockfile} to be released..."
    sleep(interval)
  end
end

# Main execution logic
def main
  if ARGV.empty? || ['-h', '--help', '-v', '--version'].include?(ARGV[0])
    usage
  end

  if ARGV.length != 2
    puts "[ERROR] Two arguments required: lockfile_name and interval_in_seconds"
    exit(1)
  end

  lockfile = ARGV[0]
  interval = ARGV[1].to_i

  wait_for_lock_release(lockfile, interval)
  puts "Lock file released, proceeding..."
end

main if __FILE__ == $PROGRAM_NAME
