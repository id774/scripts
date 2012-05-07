#!/usr/bin/env ruby

p [*0..9, *'a'..'z', *'A'..'Z', '_', '-', '!', '#', '&'].sample(8).join
