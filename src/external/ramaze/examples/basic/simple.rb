require 'rubygems'
require 'ramaze'

# A very simple little application, you can simply run it and
# point your browser to http://localhost:7000
# you can change the port by setting
# Global.port = 80
# this most likely requires root-privileges though.

# This example shows following (requests to the mentioned base-url) :
# - simple text-output from the controller    [ / ]
# - showing you what your request looked like [ /simple ]
# - joining two strings                       [ /join/string1/string2 ]
# - join arbitary strings                     [ /join_all/string1/string2/string3 ... ]
# - sum two numbers                           [ /sum/1/3 ]
# - show if you made a POST or GET request    [ /post_or_get ]
# - How to map your controllers to urls       [ /other ]
# - Also try out the error-page, just pass something odd ;)

class SimpleController < Ramaze::Controller
  map '/'

  def index
    "simple"
  end

  def simple
    request.inspect
  end

  def join(first, second)
    [first, second].join
  end

  def join_all *strings
    strings.join
  end

  def sum first, second
    first.to_i + second.to_i
  end

  def post_or_get
    request.request_method
  end
end

class OtherController < Ramaze::Controller
  map '/other'

  def index
    "Hello, World from #{self.class.name}"
  end
end

Ramaze.start
