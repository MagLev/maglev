require 'sinatra/base'
require 'webtools'
require 'json'

class WebToolsApp < Sinatra::Base
  enable :sessions

  before do
    @ts = Time.now
    unless session[:app]
      session[:app] = WebTools::AppModel.new
      puts "========= NEW APP"
    end
    @app = session[:app]
    puts "====== APP: #{@app.inspect}"
  end

  get '/' do
    erb :index
  end

  get '/version' do
    @data = @app.version_report
    erb :version, :layout => false
  end

  # Render the Sesssions list
  get '/sessions' do
    @data = @app.session_report
    erb :sessions, :layout => false
  end

  get '/browsecode' do
    content_type :json
    ['browsecode: Not Implemented'].to_json
  end

  get '/statistics' do
    content_type :json
    ['statistics: Not Implemented'].to_json
  end

  get '/tools' do
    content_type :json
    sinatra_time = ((Time.now - @ts) * 1_000).to_i
    { 'tools' => @app.tools, '_time'  => sinatra_time }.to_json
  end
end
