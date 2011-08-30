require 'objectlog_app'

# When running out of a classic top level Sinatra app, several options are
# set.  We have to set them here if we want them in a rackable app.
set :server, ["webrick"]   # Maglev currently only supports webrick
set :environment, :development
set :static, true                # Allow loading /style.css, etc.

# Setup for :views, :public and :root
set :app_file, File.expand_path(__FILE__)
# set :app_file, d
# set :views,  File.join(d, 'views')
# set :public, File.join(d, 'public')

ObjectLogApp.run! :host => 'localhost', :port => 4567
