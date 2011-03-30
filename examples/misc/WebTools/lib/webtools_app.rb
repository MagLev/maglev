require 'sinatra/base'
require 'webtools'
require 'json'

class WebToolsApp < Sinatra::Base
  enable :sessions

  before do
puts "========= request: #{request.request_method} #{request.url}"    
puts "== before: code_browser: #{session[:code_browser].inspect}"
    @ts = Time.now
    @stack = nil
puts "== before: set app"
    @app = (session[:app] ||= WebTools::AppModel.new)
puts "== before: set browser"
    @browser = (session[:code_browser] ||= WebTools::CodeBrowser.new)
puts "== before: done"
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

  get '/codebrowser/modulelist' do
    content_type :json
    wrap_in_metadata(WebTools::CodeBrowser.class_and_module_list).to_json
  end

  get '/codebrowser/module/:name' do
    begin
      mod_name = params[:name]
      puts "== GET /codebrowser/#{mod_name}"
      content_type :json
      d = @browser.select_module(mod_name)
      wrap_in_metadata(d).to_json
    rescue => e
      @stack = e.backtrace
      wrap_in_metadata("Error: #{e}").to_json
    end
  end

  get '/constant' do
    content_type :json
    wrap_in_metadata({:value => "foo" }).to_json;
  end

  get '/statistics' do
    content_type :json
    wrap_in_metadata(['statistics: Not Implemented']).to_json
  end

  get '/tools' do
    content_type :json
    wrap_in_metadata(@app.tools).to_json
  end

  # Returns a Hash that contains the data under the "data" key.
  # Adds other keys (_time, _stack) if appropriate
  def wrap_in_metadata(data)
    raise "Expecting Hash" unless Hash === data
    data['_time'] = ((Time.now - @ts) * 1_000).to_i
    data['_stack'] = @stack
    data
  end
end
