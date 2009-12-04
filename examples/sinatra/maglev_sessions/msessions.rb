require 'rubygems'
require 'sinatra'
require 'maglev_session'

# This example shows how to use a MagLev backed HTTP sesssion store.  The
# sessions are managed by the code in maglev_session.rb, and loaded as rack
# middleware.
use MaglevSession

set :run, true
set :server, ['webrick']

get '/' do
  session["counter"] ||= 0
  session["counter"] += 1

  # Ok...we're cheating here...
  @sessions = Maglev::PERSISTENT_ROOT[MaglevSession::PERSISTENT_KEY]

  erb :index
end

__END__

@@layout
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
  <head>
    <title>Simple MagLev Backed Sessions</title>
    <meta http-equiv="Content-Type" content="text/html;  charset=iso-8859-1" />
  </head>
  <body>
    <h2>Simple MagLev Backed Sessions</h2>
    <%= yield %>
  </body>
</html>


@@index
  <h3>The Current Session</h3>
  <p>Sinatra <%= Sinatra::VERSION %> says Hello at <%= Time.now %></p>
  <p>The current session count is: <%= session["counter"] %>.</p>
  <h3>Other Sessions</h3>
  <ul>
  <% if @sessions %>
    <% @sessions.each_pair do |id, val| %>
      <li>Session <%= id.to_s %> <%= val.inspect %></li>
    <% end %>
  <% else %>
    <p>There do not appear to be any sessions...</p>
  <% end %>
  </ul>
