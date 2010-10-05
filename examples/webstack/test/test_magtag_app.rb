
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

  # Asserts that each of +strings+ is in the last_response.body
  def assert_in_body(*strings)
    strings.each do |string|
      assert !last_response.body.index(string).nil?,
             "Expected '#{string}' to be in last_response.body"
    end
  end

  # Asserts that none of +strings+ is in the last_response.body
  def assert_not_in_body(*strings)
    strings.each do |string|
      assert last_response.body.index(string).nil?,
             "Did not expect '#{string}' to be in last_response.body"
    end
  end

  # Assert that current page is +page+, where page must begin with '/',
  # like: '/home'
  def assert_on_page(page)
    raise "page must begin with '/': #{page}" unless page[0] == ?\/
    assert "http://example.org#{page}" == last_request.url,
           "Expected to be on page http://example.org#{page} but was on #{last_request.url}"
  end
end

class  MagTagAppNavBar < MagTagTest
  def test_navbar_toggles_login_logout_depending_on_state
    # We are not logged in
    get '/login'
    assert_in_body     'login'   # without '/', since it is current page (inactive link)
    assert_not_in_body '/login'
    assert_not_in_body '/logout'

    user = login_new_user
    get '/home'
    assert_in_body     '/logout'
    assert_not_in_body '/login'
  end
end

class  MagTagAppTest < MagTagTest
  def test_home_page_redirects_if_not_logged_in
    get '/'
    follow_redirect!
    assert_on_page '/login'
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
    assert_on_page '/home'
  end
end

class MagTagSignupTest < MagTagTest
  def test_get_signup
    get '/signup'
    assert last_response.ok?
    assert_in_body('signup')
  end

  def test_post_signup_adds_user_and_redirects_to_home
    name = "pbm#{rand(1000)}"
    pw   = 'pw'
    assert_nil User.find_by_name(name), "username #{name} already in Users"

    post '/signup', { 'username' => name, 'password' => pw, 'confirmpassword' => pw }
    follow_redirect!
    assert last_response.ok?, "post /signup OK?: status: #{last_response.status}"
    assert_on_page '/home'
    refute_nil User.find_by_name(name), "username #{name} Not added by signup"
  end

  def test_on_bad_username_it_redisplays_form
    post '/signup', { 'username' => nil, 'password' => 'pw', 'confirmpassword' => 'pw'}
    assert last_response.ok?, "assert ok?: #{last_response.inspect}"
    assert_on_page '/signup'
    assert_in_body "Bad username"
  end

  def test_on_success_it_redirects_to_user_home
    post '/signup', { 'username' => nil, 'password' => 'pw', 'confirmpassword' => 'pw'}
    assert last_response.ok?, "assert ok?: #{last_response.inspect}"
    assert_on_page '/signup'
    assert_in_body "Bad username"
  end

  def test_password_and_confirmpassword_must_match
    post '/signup', { 'username' => 'random_user', 'password' => 'pw', 'confirmpassword' => 'pw1'}
    assert last_response.ok?, "assert ok?: #{last_response.inspect}"
    assert_on_page '/signup'
    assert_in_body "Passwords don't match"
  end

  def test_username_must_be_new
    name = 'MightyUser'
    TestHelper.ensure_user_exists name

    post '/signup', { 'username' => name, 'password' => 'pw', 'confirmpassword' => 'pw' }
    assert last_response.ok?, "assert ok?: #{last_response.inspect}"
    assert_on_page '/signup'
    assert_in_body "User #{name} already taken"
  end
end

class MagTagLogoutTest < MagTagTest
  def setup
    @name = 'logout_user'
    @pw   = 'pw'
    @logged_in_user = TestHelper.ensure_user_exists @name, @pw

    post '/login', { :username => @name, :password => @pw }
    follow_redirect!
    assert_on_page '/home'
    refute_nil last_request.session.to_hash[:logged_in_user]
  end

  def test_logout_redirects_to_login_page
    get '/logout'
    assert_nil last_request.session.to_hash[:logged_in_user]
    assert last_response.redirect?, "assert redirect?: #{last_response.inspect}"
    follow_redirect!
    assert_on_page '/login'
  end

  def test_logout_silent_if_not_logged_in
    get '/logout'
    get '/logout'
    assert last_response.redirect?, "assert redirect?: #{last_response.inspect}"
    follow_redirect!
    assert_on_page '/login'
  end

  def logged_out_user_cant_access_home_page
    get '/logout'
    assert_nil last_request.session.to_hash[:logged_in_user]

    get '/home'
    assert last_response.redirect?, "assert redirect?: #{last_response.inspect}"
    follow_redirect!
    assert_on_page '/login'
  end
end

class MagTagHomeTest < MagTagTest

  def setup
    @user = login_new_user
    assert_on_page '/home'
  end

  def test_home_page_shows_no_timeline_for_new_user
    assert_in_body "No items in timeline!"
  end

  def test_home_page_shows_timeline
    community = create_mini_community @user
    get '/home'
    assert_on_page '/home'
    assert_in_body 'Hey... I\'m following people!'
  end

  def test_home_page_shows_stats
    # Should show number of tweets, number of followers and number of following
    assert_in_body "followers 0", "following 0", "tweets 0"
  end
end

class MagTagTweetTest < MagTagTest

  def setup
    @community = create_mini_community
    @user = @community[0]
    refute_nil @user
    login_user @user
    assert_on_page '/home'
  end

  def test_post_tweet_works
    tweet_text = "test tweet from #{@user.name}."
    post "/tweet", { :tweet => tweet_text }
    follow_redirect!
    assert_on_page '/home'
    assert_in_body tweet_text
    @user.followers.each do |follower|
      refute_nil follower.timeline.detect { |tweet| tweet.text == tweet_text }
    end
  end

  def test_tweet_uses_flash_for_message
    tweet_text = "test tweet from #{@user.name}."
    post "/tweet", { :tweet => tweet_text }
    follow_redirect!
    assert_on_page '/home'
    assert_in_body 'Flash Notice'
    assert_in_body 'Tweet success!'
  end
end

# Create a new user, log them in and return the user.
# the password is the user name.
#
# @return [User] the newly created and logged-in user.
def login_new_user
  name = 'test_user_' + rand(1000).to_s
  user = TestHelper.ensure_user_exists(name, name)
  login_user user
  user
end

def login_user(user)
  post '/login', { :username => user.name, :password => user.name }
  follow_redirect!
end

# Create a community where everyone follows everyone else, and has tweeted
# about it.
#
# @param [User, nil] optional_user if not nil, will be added to community.
# @param [Fixnum, nil] size the size of the community, not including optional_user (default is 3).
# @return [Array<User>] The community of users
#
def create_mini_community(optional_user = nil, size=3)
  users = []
  users << optional_user unless optional_user.nil?
  size.times { |i| n = "u#{i}" ; users << TestHelper.ensure_user_exists(n,n) }

  # Everyone follows everyone else
  users.each { |u1| users.each { |u2| u1.follow u2 unless u1 == u2} }

  # Everyone tweets about it
  10.times { |i| users.each { |u| u.tweet("#{u.name}[#{i}] Hey... I'm following people!") } }
  users
end

MiniTest::Unit.new.run(ARGV)
