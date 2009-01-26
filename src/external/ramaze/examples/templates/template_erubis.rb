require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  view_root __DIR__(:template)
  engine :Erubis

  def index
    %{ #{A('Home', :href => :/)} | #{A(:internal)} | #{A(:external)} }
  end

  def internal *args
    @args = args
    @place = :internal
    %q{
<html>
  <head>
    <title>Template::Erubis internal</title>
  </head>
  <body>
  <h1>The internal Template for Erubis</h1>
    <%= A('Home', :href => :/) %>
    <p>
      Here you can pass some stuff if you like, parameters are just passed like this:<br />
      <%= A("/#@place/one") %><br />
      <%= A("#@place/two/three") %><br />
      <%= A("#@place/one?foo=bar") %><br />
    </p>
    <div>
      The arguments you have passed to this action are:
      <% if @args.empty? %>
        none
      <% else %>
        <% @args.each do |arg| %>
          <span><%= arg %></span>
        <% end %>
      <% end %>
    </div>
    <div>
      <%= request.params.inspect %>
    </div>
  </body>
</html>
    }
  end

  def external *args
    @args = args
    @place = :external
  end
end

Ramaze.start
