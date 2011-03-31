require 'sinatra/base'
require 'webtools'
require 'json'

# TODO: Decide on my URLs.  Should I be REST-ful:
#
#    GET /module/FooModule/constant/XYZ
#    GET /module/Object/instanceMethod/to_s
#
# Or use query params etc:
#
#    GET /constant?moduleName=FooModule&constName=XYZ
#    GET /method?moduleName=Object&methName=to_s&isInstanceMethod=false
#
class WebToolsApp < Sinatra::Base
  enable :sessions
  set :show_exceptions, true
  set :raise_errors, false

  before do
    @ts = Time.now
    @stack = nil
    @app = (session[:app] ||= WebTools::AppModel.new)
    @browser = (session[:code_browser] ||= WebTools::CodeBrowser.new)
  end

  get '/' do
    erb :index
  end

  get '/modulelist' do
    content_type :json
    wrap_in_metadata(WebTools::CodeBrowser.class_and_module_list).to_json
  end

  get '/module/:name' do
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
    wrap_in_metadata(@browser.select_constant(params[:moduleName], params[:constName])).to_json;
  end

  get '/method' do
    content_type :json
    is_instance = params[:isInstanceMethod] == 'true' ? true : false
    wrap_in_metadata(@browser.select_method(params[:moduleName], params[:methName], is_instance)).to_json;
  end

  get '/version' do
    @data = @app.version_report
    erb :version, :layout => false
  end

  get '/sessions' do
    @data = @app.session_report
    erb :sessions, :layout => false
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

  error do
    excep = request.env['sinatra.error']
    content_type :json
    { '_stack' => excep.backtrace.join("<br>") }
  end
end
