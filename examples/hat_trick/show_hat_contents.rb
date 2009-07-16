#!/usr/bin/env maglev-ruby
# Print the contents of the hat

# require 'rabbit.rb'
# puts "Do you know how many objects are in the hat?\nPerhaps there are #{::HAT.size}."
# puts "You can count them for yourself ..."
# print ::HAT.contents.inspect
p Maglev::PERSISTENT_ROOT[:hat].contents
