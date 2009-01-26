require 'ramaze'

class MainController < Ramaze::Controller
  engine :Liquid

  def index
    @hash = {
      :hello => "Hello, World!",
      :counter => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    }
    '<html><body>{% for i in counter %}<span>{{hello}}</span>{% endfor %}</body></html>'
  end
end
