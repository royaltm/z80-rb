#!/usr/bin/env ruby
# -*- coding: BINARY -*-

# Creates the HAM256 .SCR file that displays all 256 colors 3 times for testing HAM256 routines

screen = ''

# pixels
line = ("\xFF"*8 + "\x00"*8)*2
screen = line*192

puts screen.bytesize
# attributes
attrs = (0..3).map do |clut|
  (0..63).step(9).map {|n| n + clut * 64 }.to_a.pack('c*')
end

12.times do
  attrs.each { |a| screen << a << a }
end

puts screen.bytesize

# palette
3.times do
  (0..255).each do |n|
    screen << n.ord
  end
end

puts screen.bytesize

File.open('examples/screens/test_ham256.scr', 'wb') do |file|
  file.write screen
end
