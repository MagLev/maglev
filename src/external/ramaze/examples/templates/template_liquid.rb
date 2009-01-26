require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  view_root __DIR__(:template)
  engine :Liquid

  def index
    %{ #{A 'Home', :href => :/} | #{A(:internal)} | #{A(:external)} }
  end

  def liquid_hash(place, *args)
    {
      'header'     => "The #{place} Template for Liquid",
      'link_home'  => A('Home', :href => :/),
      'link_one'   => A("#{place}/one"),
      'link_two'   => A("#{place}/one/two/three"),
      'link_three' => A("#{place}?foo=Bar"),
      'args'       => args,
      'args_empty' => args.empty?,
      'params'     => request.params.inspect
    }
  end


  def internal *args
    @hash = liquid_hash(:internal, *args)
    %q{
<html>
  <head>
    <title>Template::Liquid internal</title>
  </head>
  <body>
  <h1>{{header}}</h1>
    {{link_home}}
    <p>
      Here you can pass some stuff if you like, parameters are just passed like this:<br />
      {{link_one}}<br />
      {{link_two}}<br />
      {{link_three}}
    </p>
    <div>
      The arguments you have passed to this action are:
      {% if args_empty %}
        none
      {% else %}
        {% for arg in args %}
          <span>{{arg}}</span>
        {% endfor %}
      {% endif %}
    </div>
    <div>
      {{params}}
    </div>
  </body>
</html>
    }
  end

  def external *args
    @hash = liquid_hash(:external, *args)
  end
end

Ramaze.start
