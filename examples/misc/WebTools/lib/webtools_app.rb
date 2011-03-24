require 'sinatra/base'
require 'webtools'
require 'json'

class WebToolsApp < Sinatra::Base

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

  # This renders an HTML page, no JavaScript needed.
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

  # Provide compatibility with the WebTools app.  This returns information
  # about the tools provided by WebTools
  #
  # TODO: Perhaps we can have a class per tool?
  get '/tools' do
    pages = []
    pages << {
      'file' => '/version',
      'name' => 'Version Report',
      'description' => 'Information about the Stone and Gem processes and their host(s)',
      'title' => 'the title'
    }
    pages << {
      'file' => 'sessions',
      'name' => 'Current Sessions',
      'description' => 'Information about current sessions and other processes',
      'title' => ''
    }
    pages << {
      'file' => 'browsecode',
      'name' => 'Code Browser',
      'description' => 'Browse Classes, Constants, Methods and source',
      'title' => ''
    }
    pages << {
      'file' => 'statistics',
      'name' => 'Statistics',
      'description' => 'Load and view statmonitor files',
      'title' => ''
    }

    content_type :json
    sinatra_time = ((Time.now - @ts) * 1_000).to_i
    { 'tools' => pages,
      '_time'  => sinatra_time }.to_json
  end
end
