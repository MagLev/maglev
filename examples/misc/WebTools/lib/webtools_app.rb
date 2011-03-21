require 'sinatra/base'

class WebToolsApp < Sinatra::Base
  get '/' do
    'hello'
  end
end
