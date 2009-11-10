require 'rubygems'
require 'sinatra'

class Demo < Sinatra::Base
  set :server, ['webrick']
  use_in_file_templates!

  get '/' do
    erb :index
  end

  post '/nearest' do
    @x = params[:x].to_f
    @y = params[:y].to_f
    @k = params[:k].to_i
    target = KDTree::Point2D.new(@x, @y, :user_target)
    @results = Maglev::PERSISTENT_ROOT[:kdtree_demo_data].nearest_k(target, @k)
    erb :index
  end
end

__END__

@@layout
<html>
  <head><title>Maglev KDTree Demo</title></head>
  <body>
  <%= yield %>
  </body>
</html>

@@index

<h2>Search for nearest neighbors</h2>
<p>
  Enter the X and Y coordinates of a target location, and enter the number
  of search results desired as K.  This will return the K nearest points in
  the data base to &lt;x, y&gt;.
</p>
<form method="post" action="/nearest">
  <ul>
    <li>X: <input type="text" name="x" title="X" value="42.78"/></li>
    <li>Y: <input type="text" name="y" title="Y" value="-97.09"/></li>
    <li>K: <input type="text" name="k" title="K" value="10" /></li>
    <li><input type="submit" value="Search"></li>
  </ul>
</form>

<% if @results %>
  <h2>Results for: [<%= @x %>, <%= @y %>] K: <%= @k %></h2>
  <table>
    <tr>
      <th>#</th>
      <th>Description</th>
      <th>Distance</th>
    </tr>
  <% @results.each_with_index do |r,i| %>
    <tr>
      <td><%= i %></td>
      <td><%= r.value %></td>
      <td><%= r.distance %></td>
    </tr>
  <% end %>
  </table>
<% end %>

