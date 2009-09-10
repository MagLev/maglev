require 'rubygems'
require 'sinatra/base'
require 'maglev/objectlog'
require 'txn_wrapper'

# A simple object log viewer.  This app tries to be re-locatable.  It looks
# at the @@path class variable for its mount point, and then makes all URLs
# relative to that.  See config.ru for example of setting @@path.
class ObjectLogApp < Sinatra::Base

  # When running out of a classic top level Sinatra app, several options
  # are set.  We have to set them here if we want them in a rackable app.
  set :server, ["webrick"]   # Maglev currently only supports webrick
  set :environment, :development
  set :static, true                # Allow loading /style.css, etc.
  set :app_file, __FILE__          # Affects :views, :public and :root

  def initialize(*args)
    super(*args)
    @title = "Object Log Application"
    @nav_bar = <<-EOS
        <ul class="menu">
          <li><a href="/">Main App</a></li>
          <li><a href="#{path_for('/clear')}">Clear Log</a></li>
        </ul>
    EOS
  end

  @@script_name = ''

  def self.script_name=(str)
    @@script_name = str
  end

  def path_for(string)
    @@script_name + string
  end

  get '/info' do
    erb :info
  end

  get '/' do
    STDERR.puts
    Maglev.abort_transaction  # Get a fresh object view
    @objectlog = ObjectLogEntry.object_log
    erb :objectlog
  end

  get '/clear' do
    ObjectLogEntry.object_log.clear
    ObjectLogEntry.trace("Cleared log at #{Time.now}").add_to_log
    redirect path_for('/')
  end

  get '/entry/:id' do
    index = params[:id].to_i
    @object = ObjectLogEntry.object_log[index]
    raise "Can't find Object Log Entry for index: #{index}" unless @object
    erb :objectdetail
  end

  get '/object/:id' do
    oop = params[:id].to_i
    @object = ObjectSpace._id2ref(oop)
    "Can't find object with oop #{oop}" unless @object
    erb :objectdetail
  end
end
