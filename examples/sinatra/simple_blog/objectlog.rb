require 'rubygems'
require 'sinatra'

require 'maglev/objectlog'

# Workaround for issue automatically starting app.
# See comments in README file
configure(:development) do
  set :run, true
  App = {
    :title => 'Object Log Viewer',
    :nav_bar => <<-EOS
        <ul class="menu">
          <li><a href="/objectlog">Object Log</a></li>
        </ul>
    EOS
  }
end

error do
  e = request.env['sinatra.error']
  "There was an error: #{e}"
end

get '/' do
  redirect '/objectlog'
end

get '/objectlog' do
  @objectlog = ObjectLogEntry.object_log
  erb :objectlog
end

get '/entry/:id' do
  index = params[:id].to_i
  @object = ObjectLogEntry.object_log[index]
  stop [ 404, "Can't find Object Log Entry for index: #{index}" ] unless @object
  erb :objectdetail
end

get '/object/:id' do
  oop = params[:id].to_i
  @object = Object._object_for_oop(oop)
  stop [ 404, "Can't find object with oop #{oop}" ] unless @object
  erb :objectdetail
end
