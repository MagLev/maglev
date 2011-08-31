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

  def test_it_does_params
    get '/names/fred'
    assert last_response.ok?
    assert last_response.body =~ /The name is: fred/
  end

  def test_wildcards_in_url
    get '/say/hello/to/fred'
    assert last_response.ok?
    assert last_response.body =~ /Say 'hello' to fred/
  end

  def test_redirect
    get '/goto_home'
    assert last_response.redirect?
    assert_equal last_response.location, "http://example.org/"
  end

  def test_session
    get '/session_count', { }, 'rack.session' => { 'count' => 0 }
    assert last_response.ok?
    assert_equal 'count: 1', last_response.body

    get '/session_count', { }, 'rack.session' => { 'count' => 1 }
    assert last_response.ok?
    assert_equal 'count: 2', last_response.body
  end
end

MiniTest::Unit.new.run(ARGV)
