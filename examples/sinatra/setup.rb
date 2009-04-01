# Since we don't have rubygems to load things, this is a file included by
# files involved in the multiple ways you might want to start the app.
#
puts "== setup.rb"
unless defined? SETUP_RUN
  SETUP_RUN = 1
  here = File.dirname(__FILE__)
  $:.unshift(here + '/../../src/external/Sinatra/lib')
  $:.unshift(here + '/../../src/external/Rack/lib')
  $:.unshift(here)
end



