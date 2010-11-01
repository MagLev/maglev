#!/usr/bin/env maglev-ruby
# Print the contents of the hat

hat = Maglev::PERSISTENT_ROOT[:hat]
puts "The hat contains #{hat.size} rabbits"
p Maglev::PERSISTENT_ROOT[:hat]
