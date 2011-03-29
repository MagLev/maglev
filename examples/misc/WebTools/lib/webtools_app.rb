require 'sinatra/base'
require 'webtools'
require 'json'

class WebToolsApp < Sinatra::Base
  enable :sessions

  before do
    @ts = Time.now
    unless session[:app]
      session[:app] = WebTools::AppModel.new
    end
    @app = session[:app]
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

  get '/codebrowser' do
    content_type :json
    add_time_stats(@app.code_browser.to_json).to_json
  end

  get '/statistics' do
    content_type :json
    add_time_stats(['statistics: Not Implemented']).to_json
  end

  get '/tools' do
    content_type :json
    add_time_stats(@app.tools).to_json
  end

  def add_time_stats(data)
    # sinatra_time = ((Time.now - @ts) * 1_000).to_i
    { "_time" => ((Time.now - @ts) * 1_000).to_i,
      "data"  => data }
  end
end
