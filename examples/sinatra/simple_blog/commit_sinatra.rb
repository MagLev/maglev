Maglev.abort_transaction
Maglev.persistent do
  load 'rubygems.rb'    # Force load of rubygems
  require 'sinatra.rb'  # Use
end
Maglev.commit_transaction
puts "== Committed sinatra"
