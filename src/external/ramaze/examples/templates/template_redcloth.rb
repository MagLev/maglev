require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  view_root __DIR__(:template)
  engine :RedCloth
  layout :layout

  def index
    @place = :home
    %{ #{A 'Home', :href => :/} | #{A(:internal)} | #{A(:external)} }
  end

  def internal(*args)
    @place = :internal
    @args = args
    <<__REDCLOTH__
h1. The <%= @place %> Template for RedCloth

"Home":<%= Rs(:/) %>

Here you can pass some stuff if you like, parameters are just passed like this:<br />
"<%= @place %>/one":<%= Rs(@place, :one) %><br />
"<%= @place %>/two/three":<%= Rs(@place, :two, :three) %><br />
"<%= @place %>/one?foo=bar":<%= Rs(@place, :one, :foo => :bar) %>

The arguments you have passed to this action are:<br />
<% if @args.empty? %>
  none
<% else %>
  <% @args.each do |arg| %>
    <span><%= arg %></span>
  <% end %>
<% end %>

<%= request.params.inspect %>
__REDCLOTH__
  end

  def external(*args)
    @place = :external
    @args = args
  end

  def layout
    <<__HTML__
<html>
  <head>
    <title>Template::RedCloth <%= @place %></title>
  </head>
  <body>
#{@content}
  </body>
</html>
__HTML__
  end
end

Ramaze.start
