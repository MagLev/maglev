require 'sinatra'

class Demo < Sinatra::Base
  if Sinatra::VERSION =~ /^0\.*/
    use_in_file_templates!
  else
    enable :inline_templates  # Sinatra 1.0 and later
  end

  TREE = Maglev::PERSISTENT_ROOT[:kdtree_demo_data]

  get '/' do
    @lat =   48.724
    @lon = -122.488
    @k = 30
    erb :index
  end

  post '/nearest' do
    @lat = params[:lat].to_f
    @lon = params[:lon].to_f
    @k = params[:k].to_i
    @target = Collections::Point2D.new(@lon, @lat, :user_target)
    raw_results = TREE.nearest_k(@target, @k)
    @results = raw_results.map do |r|
      [r.value, r.value.spherical_miles(@target)]
     end.sort {|a,b| a[1] <=> b[1] }
    erb :index
  end
end

__END__

@@layout
<html>
  <head><title>MagLev KDTree Demo</title></head>
  <body>
  <%= yield %>
  </body>
  <h4>Credits</h4>
  <p>
   The database of zip code locations is from
   <a href="http://www.geonames.org">GeoNames.org</a>
  </p>
</html>

@@index

<h2>Search for nearest neighbors</h2>
<p>
  Find the nearest K zip codes to the location entered in the form.
</p>
<form method="post" action="/nearest">
  <ul>
    <li>Latitude:  <input type="text" name="lat" title="Lat" value="<%= @lat %>"/></li>
    <li>Longitude: <input type="text" name="lon" title="Lon" value="<%= @lon %>"/></li>
    <li>K: <input type="text" name="k" title="K" value="<%= @k %>" /></li>
    <li><input type="submit" value="Search"></li>
  </ul>
</form>

<% if @results %>
  <h2>Results for: <%= @lat %>, <%= @lon %> K: <%= @k %></h2>
  <table>
    <tr>
      <th>#</th>
      <th>Zip</th>
      <th>Description</th>
      <th>Latitude</th>
      <th>Longitude</th>
      <th>Miles</th>
    </tr>
  <% @results.each_with_index do |r,i| %>
  <%   pc = r[0] %>
    <tr>
      <td><%= i %></td>
      <td><%= pc.zip %></td>
      <td><%= pc.name %>, <%= pc.state %></td>
      <td align="right"><%= "% 3.3f" % [pc.lat] %></td>
      <td align="right"><%= "% 3.3f" % [pc.lon] %></td>
      <td align="right"><%= "% 5.2f" % r[1] %></td>
    </tr>
  <% end %>
  </table>
<% end %>

