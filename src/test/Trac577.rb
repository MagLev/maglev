$:.unshift("#{ENV['MAGLEV_HOME']}/src/external/rack/lib")
$:.unshift("#{ENV['MAGLEV_HOME']}/src/external/sinatra/lib")

require 'sinatra/base'

class MyApp < Sinatra::Base
  get '/' do
    'Hello world'
  end
end
MyApp.run! :host => 'localhost', :port => 4567
