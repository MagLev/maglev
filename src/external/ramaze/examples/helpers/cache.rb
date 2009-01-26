require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  map :/

  helper :cache
  cache :index

  def index
%[
<html>
  <head><title>examples/caching</title></head>
  <body>
    <p>
      This action just shows you a random number: #{rand * 100}.<br />
      If you <a href="/">refresh</a> the page it won't change since you see a cached version.<br />
      But if you <a href="/invalidate">invalidate</a> it, the page will be regenerated.
    </p>
  </body>
</html>
]
  end

  def invalidate
    action_cache.delete '/index'
    redirect :/
  end
end

Ramaze.start