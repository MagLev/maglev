# Hack to workaround Sinatra start up problem.
#
# To workaround a startup problem, the sinatra_app.rb file does:
#
#   set :run, true
#
# As a consequence, after the unit tests run, then webrick will fire up.
# So, in sinatra_app.rb, we test for DO_NOT_RUN, and if it is defined, then
# we don't set the :run to true....
DO_NOT_RUN = true

require 'sinatra_app'
require 'minitest/unit'
require 'rack/test'

set :environment, :test

class HelloWorldTest < MiniTest::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_it_says_hello_world
    get '/'
    assert last_response.ok?, "response not ok"
    assert last_response.body =~ /Sinatra #{Sinatra::VERSION} says Hello/, "response content wrong: #{last_response.body}"
  end
end

MiniTest::Unit.new.run(ARGV)
