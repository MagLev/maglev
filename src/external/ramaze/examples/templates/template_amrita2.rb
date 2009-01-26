require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  view_root __DIR__(:template)
  engine :Amrita2

  def index
    %{ #{A 'Home', :href => :/} | #{A(:internal)} | #{A(:external)} }
  end

  def internal(*args)
    @data = binding
    @place = :internal
    <<__AMRITA2__
<html>
  <head>
    <title>Template::Amrita2 external</title>
  </head>
  <body>
  <h1 am:src="title" />
    <%= link_home %>
    <p>
      Here you can pass some stuff if you like, parameters are just passed like this:<br />
      <%= link_one %><br />
      <%= link_two %><br />
      <%= link_three %>
    </p>
    <<div<
      <<:args<
        <span><%= $_ %></span>
    <%= inspect_parameters %>
  </body>
</html>
__AMRITA2__
  end

  def external(*args)
    @data = binding
    @place = :external
  end

  private

  def title
    "The #{@place} Template for Amrita2"
  end

  def link_home
    A('Home', :href => '/')
  end

  def link_one
    A("/#{@place}/one", :href => Rs(@place, :one))
  end

  def link_two
    A("/#{@place}/one/two/three", :href => Rs(@place, :one, :two, :three))
  end

  def link_three
    A("/#{@place}?foo=bar", :href => Rs(@place, :one, :foo => :bar))
  end

  def inspect_parameters
    request.params.inspect
  end

  def args
    @params.map{|arg| "<span>#{arg}</span>"}.join(' ')
  end
end

Ramaze.start
