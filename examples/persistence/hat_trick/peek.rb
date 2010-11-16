#!/usr/bin/env maglev-ruby
# You can use this Proc to spy on objects in Maglev::PERSISTENT_ROOT

Maglev::PERSISTENT_ROOT[:peek] = Proc.new {
  |obj| 
  Maglev.abort_transaction
  Maglev::PERSISTENT_ROOT.key?(obj) ? 
    Maglev::PERSISTENT_ROOT[obj] : Maglev::PERSISTENT_ROOT
  }
Maglev.commit_transaction
