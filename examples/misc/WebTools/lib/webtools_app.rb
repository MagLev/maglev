require 'sinatra/base'
require 'webtools'
require 'json'

class WebToolsApp < Sinatra::Base

  before do
    @ts = Time.now
  end

  get '/' do
    erb :index
  end

  get '/version' do
    stone_rpt = WebTools::API.stone_version_report
    gem_rpt = WebTools::API.gem_version_report
    @data = { }
    (stone_rpt.keys + gem_rpt.keys).each do |k|
      @data[k] = [stone_rpt[k], gem_rpt[k]]
    end
    erb :version
  end

  get '/sessions' do
    content_type :json
    ['sessions: Not Implemented'].to_json
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
