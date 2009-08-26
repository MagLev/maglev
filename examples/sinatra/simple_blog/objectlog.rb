require 'rubygems'
require 'sinatra'
# require 'txn_wrapper'

require 'maglev/objectlog'

# use MagLevTransactionWrapper

# Workaround for issue automatically starting app.
# See comments in README file
configure(:development) do
  set :run, true
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
  erb :index
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

__END__

@@ layout
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
  <head>
    <title>Object Log Viewer</title>
    <meta http-equiv="Content-Type" content="text/html;  charset=iso-8859-1" />
    <link rel="stylesheet" type="text/css" href="/style.css" media="all" />
  </head>
  <body>
    <div id="container">
      <div id="header">
        <h1>Object Log Viewer</h1>
      </div>
      <div id="navigation">
        <ul class="menu">
          <li><a href="/objectlog">Home</a></li>
          <li>... not much in the navbar...</li>
        </ul>
      </div>
      <div id="content">
        <div class="left"><%= yield %></div>
        <div class="right">
          <h4>Recent Posts</h4>
          <p>TBD</p>
          <h4>About</h4>
          <p>This is a simple viewer for the MagLev object log.</p>
          <h4>Credit</h4>
          <p>The layout is <a href="http://css4free.com/wp-content/uploads/templates/zineprint/">zineprint</a>, Designed by
             <a href="http://rizzle.us.to">Rizzle Studios</a>.
        </div>
      </div>
      <div id="footer">
        Object Log Viewer running on <a href="http://maglev.gemstone.com">MagLev</a>
      </div>
    </div>
  </body>
</html>

@@ index
<table>
  <tr><th>#</th><th>Tag</th><th>Label</th><th>Object</th><th>Timestamp</th></tr>
<% @objectlog.each_with_index do |entry,i| %>
  <tr>
    <td><%= i %></td>
    <td><%= entry.tagged? ? "X" : "" %></td>
    <td><a href="/entry/<%= i %>"> <%= entry.label %></a></td>
    <td><%= entry.object %></td>
    <td><%= entry.timestamp %></td>
  </tr>
<% end %>
</table>

@@ objectdetail
<h3>Object Detail </h3>
<table>
  <tr><th>Attribute</th><th>Value</th></tr>
  <tr><td>Class</td><td><a href="/object/<%= @object.class.object_id %>"><%= @object.class %></a></td></tr>
  <tr><td>Oop</td><td> <%= @object.object_id %></td></tr>
  <% @object.instance_variables.each do |var|
       ivar = @object.instance_variable_get(var)
  %>
    <tr>
      <td><%= var %></td>
      <td><a href="/object/<%= ivar.object_id %>"><%= ivar.inspect %></td>
    </tr>
  <% end %>
</table>

