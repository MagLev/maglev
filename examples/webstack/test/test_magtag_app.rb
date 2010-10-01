
require 'magtag_app'
require 'minitest/unit'
require 'rack/test'

require 'testhelper'

ENV['RACK_ENV'] = 'test'

class MagTagTest < MiniTest::Unit::TestCase
  include Rack::Test::Methods

  # Needs to be done once; needed to serve static pages, like css
  MagTag.set :public, './public'
  MagTag.set :sessions, true
  def app
    # We don't use Sinatra::Application, because we derive from
    # Sinatra::Base, i.e., are not a classic style Sinatra App.
    MagTag
  end
end

class  MagTagAppTest < MagTagTest
  def test_home_page_redirects_if_not_logged_in
    get '/'
    follow_redirect!
    assert_equal 'http://example.org/login', last_request.url
  end

  def test_get_css_does_not_redirect
    get '/magtag.css'
    assert last_response.ok?, "OK? status: #{last_response.status} uri: #{last_request.url}"
  end
end

class MagTagLoginTest < MagTagTest

  def test_good_login_redirects_to_home
    fred = TestHelper.create_and_save('fred', 'fred-pw')
    refute_nil fred

    post '/login', { :username => fred.name, :password => fred.instance_variable_get(:@password) }
    follow_redirect!
    assert_equal 'http://example.org/home', last_request.url, "url: #{last_request.url}"
  end
end

class MagTagSignupTest < MagTagTest
  def test_get_signup
    get '/signup'
    assert last_response.ok?
    refute_nil last_response.body.index('signup')
  end

  def test_post_signup_adds_user_and_redirects_to_home
    name = "pbm#{rand(1000)}"
    pw   = 'pw'
    assert_nil User.find_by_name(name), "username #{name} already in Users"

    post '/signup', { 'username' => name, 'password' => pw, 'confirmpassword' => pw }
    follow_redirect!
    assert last_response.ok?, "post /signup OK?: status: #{last_response.status}"
    assert_equal 'http://example.org/home', last_request.url, "url: #{last_request.url}"
    refute_nil User.find_by_name(name), "username #{name} Not added by signup"
  end

  def test_on_bad_username_it_redisplays_form
    post '/signup', { 'username' => nil, 'password' => 'pw', 'confirmpassword' => 'pw'}
    assert last_response.ok?, "refute ok?: #{last_response.inspect}"
    assert_equal "http://example.org/signup", last_request.url
    refute_nil last_response.body.index "Bad username"
  end

  def test_on_success_it_redirects_to_user_home
    post '/signup', { 'username' => nil, 'password' => 'pw', 'confirmpassword' => 'pw'}
    assert last_response.ok?, "refute ok?: #{last_response.inspect}"
    assert_equal "http://example.org/signup", last_request.url
    refute_nil last_response.body.index "Bad username"
  end

  def test_password_and_confirmpassword_must_match
    post '/signup', { 'username' => 'random_user', 'password' => 'pw', 'confirmpassword' => 'pw1'}
    assert last_response.ok?, "refute ok?: #{last_response.inspect}"
    assert_equal "http://example.org/signup", last_request.url
    refute_nil last_response.body.index "Passwords don't match"
  end

  def test_username_must_be_new
    name = 'MightyUser'
    pw = 'pw'
    TestHelper.ensure_user_exists name
    
    post '/signup', { 'username' => name, 'password' => 'pw', 'confirmpassword' => 'pw'}
    assert last_response.ok?, "refute ok?: #{last_response.inspect}"
    assert_equal "http://example.org/signup", last_request.url
    refute_nil last_response.body.index "User #{name} already taken"
  end
end

MiniTest::Unit.new.run(ARGV)
