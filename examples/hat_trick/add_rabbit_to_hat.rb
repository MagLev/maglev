#!/usr/bin/env maglev-ruby
# Add a rabbit to the hat. Another rabbit is added every time this is run.

require 'rabbit.rb'
$hat.put(Rabbit.new)
RubyContext.save_context
Gemstone.commitTransaction
