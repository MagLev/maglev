require 'spec/helper'

class MockSequelUser
  def profile
    "Arthur Dent, fearful human in outer space!"
  end

  def self.authenticate(hash)
    new if hash.values_at('name', 'password') == %w[arthur 42]
  end
end

class SpecUserHelper < Ramaze::Controller
  map '/'
  helper :user
  trait :user_model => MockSequelUser

  def status
    logged_in?
  end

  def login
    user_login
  end

  def profile
    user.profile
  end
end

describe Ramaze::Helper::User do
  behaves_like 'browser'
  ramaze :adapter => :webrick

  should 'login' do
    Browser.new do
      get('/status').should == 'false'
      get('/login', 'name' => 'arthur', 'password' => '42')
      get('/status').should == 'true'
      get('/profile').should == MockSequelUser.new.profile
    end
  end
end
