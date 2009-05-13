#!/usr/bin/env maglev-ruby
# Create a hat to hold rabbits. It can hold other things, too.

load 'hat.rb'
$hat = Hat.new
RubyContext.save_context
Gemstone.commitTransaction
