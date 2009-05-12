#!/usr/bin/env maglev-ruby
# Print the contents of the hat

require 'rabbit.rb'
puts "Do you know how many objects are in the hat?\nPerhaps there are #{$hat.size}."
puts "You can count them for yourself ..."
print $hat.contents.inspect
