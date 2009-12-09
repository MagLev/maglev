# -*- ruby -*-
# This runs the SCGI based app

require 'scgi_app'

# disable :run
# set :environment, :development

SCGIApp.run! :host => 'localhost', :port => 4567

