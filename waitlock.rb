#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
#
# [Usage]
# $ ./waitlock.rb hoge.txt 1
#
# Lock with check interval of 1 seconds. The lock file named by "hoge.txt".
#

class Wait
end

class << Wait
  def lock
    lock = true
    while lock
      if FileTest.exist?(LOCKFILE)
        sleep INTERVAL
      else
        lock = false
      end
    end
  end
end


if __FILE__ == $0
  LOCKFILE=ARGV.shift || '.lock'
  interval=ARGV.shift || 1; INTERVAL=interval.to_i
  Wait.lock
end

