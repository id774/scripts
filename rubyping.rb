#!/usr/bin/env ruby

# syntax rubyping.rb [subnet] [from] [to]
# ex. rubyping.rb 192.168.11. 1 32

SUBNET=ARGV.shift || '192.168.11.'
from=ARGV.shift   || 1; FROM=from.to_i
to=ARGV.shift     || 32; TO=to.to_i
$be_or_not = Hash.new
$th_id = Hash.new

TO.step(FROM,-1) { |n|
  ip = SUBNET + n.to_s
  $be_or_not[ ip ] = "-----"
  $th_id[n] = Thread.start{ 
    if system("ping -c 1 -i 5 #{ip} > /dev/null")
      $be_or_not[ ip ] = 'alive'
    end
  Thread.exit
  }
}

for n in FROM .. TO
  retry if $th_id[ n ].status
end

FROM.step(TO,1) { |n|
  ip = SUBNET + n.to_s
  print ip, " --> ", $be_or_not[ ip ], "\n"
}
