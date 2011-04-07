require 'sinatra/base'
require 'webtools'
require 'json'

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
    prepare_data(WebTools::CodeBrowser.class_and_module_list)
  end

  get '/module/:name' do
    content_type :json
    prepare_data(@browser.select_module(params[:name]))
  end

  get '/module/:module_name/constant/:const_name' do
    content_type :json
    prepare_data(@browser.select_constant(params[:module_name],
                                          params[:const_name]))
  end

  get '/module/:module_name/method' do
    content_type :json
    flag = params[:is_instance_method] == 'true' ? true : false
    prepare_data(@browser.select_method(params[:module_name],
                                        params[:method_name],
                                        flag))
  end

  get '/objectspace/:object_id' do
    content_type :json
    prepare_data(@browser.object_info(params[:object_id]))
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
    prepare_data(['statistics: Not Implemented'])
  end

  get '/tools' do
    content_type :json
    prepare_data(@app.tools)
  end

  get '/transaction/abort' do
    Maglev.abort_transaction
    content_type :json
    prepare_data(WebTools::CodeBrowser.class_and_module_list)
  end

  # Returns a JSON string that contains the data under the "data" key.
  # Adds other keys (_time, _stack) if appropriate.
  def prepare_data(data)
    raise "Expecting Hash" unless Hash === data
    data['_time'] = ((Time.now - @ts) * 1_000).to_i
    data['_stack'] = @stack
    data.to_json
  end

  error do
    excep = request.env['sinatra.error']
    content_type :json
    { '_stack' => excep.backtrace.join("<br>") }.to_json
  end
end
