require 'sinatra/base'
require 'maglev/objectlog'

# A simple object log viewer.  This app tries to be re-locatable.  It looks
# at the @path class variable for its mount point, and then makes all URLs
# relative to that.  See config.ru for example of setting @path.
class ObjectLogApp < Sinatra::Base

  set :app_file, __FILE__  # Set the public and static views

  def initialize(*args)
    super(*args)
    @title = "Object Log Application"
    # NOTE: Main App URL is a hack...works with most examples
    @nav_bar = <<-EOS
        <ul class="menu">
          <li><a href="#{@main_app_url}">Main App</a></li>
          <li><a href="#{path_for('/')}">Main Object</a></li>
          <li><a href="#{path_for('/objectlog')}">ObjectLog</a></li>
          <li><a href="#{path_for('/clear')}">Clear Log</a></li>
        </ul>
    EOS
  end

  class << self
    attr_accessor :script_name, :main_app_url, :main_object
  end

  # self.script_name = ''
  # self.main_app_url = '/'
  # self.main_object  = ObjectLogEntry.object_log

  def path_for(string)
    ObjectLogApp.script_name + string
  end

  get '/info' do
    <<-EOS
<h3>ObjectLog Info</h3>
<ul>
  <li>:app_file  #{options.app_file.inspect} </li>
  <li>:views     #{options.views.inspect} </li>
  <li>:public    #{options.public.inspect} </li>
  <li>:root      #{options.root.inspect} </li>
  <li>main_object: #{ObjectLogApp.main_object.inspect} (#{ObjectLogApp.main_object.class})</li>
</ul>
    EOS
  end

  get '/' do
    Maglev.abort_transaction  # Get a fresh object view
    @object = ObjectLogApp.main_object
    erb :objectdetail
  end

  get '/objectlog' do
    Maglev.abort_transaction  # Get a fresh object view
    @objectlog = ObjectLogEntry.object_log
    erb :objectlog
  end

  get '/clear' do
    Maglev.abort_transaction
    ObjectLogEntry.object_log.clear
    ObjectLogEntry.trace("Cleared log at #{Time.now}").add_to_log
    Maglev.commit_transaction
    redirect path_for('/')
  end

  get '/entry/:id' do
    index = params[:id].to_i
    @object = ObjectLogEntry.object_log[index]
    raise "Can't find Object Log Entry for index: #{index}" unless @object
    erb :objectdetail
  end

  # Allows introspection of an arbitrary object
  get '/object/:id' do
    oop = params[:id].to_i
    @object = ObjectSpace._id2ref(oop)
    "Can't find object with oop #{oop}" unless @object
    erb :objectdetail
  end
end
