#!/usr/bin/env maglev-ruby
# Add a rabbit to the hat. Another rabbit is added every time this is run.

Maglev::PERSISTENT_ROOT[:hat].put(Rabbit.new)
Maglev.commit_transaction
