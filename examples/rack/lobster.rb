# This runs the lobster.rb app that comes with Rack.  You can also run the
# lobster app with MagLev from the command line like this:
#
#   $ cd $MAGLEV_HOME/src/external/rack
#   $ maglev-ruby -Ilib lib/rack/lobster.rb
#
# And then hit http://localhost:9292
#
#
$:.unshift "#{ENV['MAGLEV_HOME']}/src/external/rack/lib"

require 'rack'
require 'rack/showexceptions'
require 'rack/lobster'

# From lobster.rb:
Rack::Handler::WEBrick.run \
  Rack::ShowExceptions.new(Rack::Lint.new(Rack::Lobster.new)),
  :Port => 9292
