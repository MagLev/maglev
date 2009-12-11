# -*- ruby -*-
#
# This rackup file can be run from either MRI or MagLev.  It runs the
# scgi_app.rb with Rack::Handler::SCGI. It requires the SCGI gem to be
# installed.
#
# Pass options to rackup:
#   -s scgi       Use the SCGI handler
#   -p 4567       Use port 4567
#   -E none       unset the rack Environment (:none, rather than :development)
#
#\ -s scgi -p 4567 -E none

require 'rubygems'
require 'sinatra'

require 'scgi_app'

# app = Rack::Builder.new do
#   use Rack::CommonLogger
#   run SCGIApp.new
# end

run SCGIApp.new
