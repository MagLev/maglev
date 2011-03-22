require 'sinatra/base'
require 'json'

class WebToolsApp < Sinatra::Base

  before do
    @ts = Time.now
  end

  get '/' do
    erb :index
  end

  # Provide compatibility with the WebTools app.  This returns information
  # about the tools provided by WebTools
  get '/tools' do
    pages = []
    pages << {
      'file' => 'xyz.html',
      'name' => 'Version Report',
      'description' => 'Information about the Stone and Gem processes and their host(s)',
      'title' => 'the title'
    }

    content_type :json
    sinatra_time = ((Time.now - @ts) * 1_000).to_i
    { 'tools' => pages,
      '_time'  => sinatra_time }.to_json
  end
end
