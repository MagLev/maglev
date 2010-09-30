
require 'magtag'
require 'minitest/unit'
require 'rack/test'

set :environment, :test

class MagTagTest < MiniTest::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_home_page_redirects_if_not_logged_in
    get '/'
    
  end
end

MiniTest::Unit.new.run(ARGV)
