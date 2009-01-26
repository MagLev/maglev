require 'rubygems'
require 'sequel'

begin
  DB = Sequel.sqlite
rescue NoMethodError
  raise LoadError, 'Install latest Sequel gem'
end

class User < Sequel::Model(:users)
  set_schema do
    primary_key :id
    varchar :username
    varchar :password
  end
end

unless User.table_exists?
  User.create_table
  User.create :username => 'admin', :password => 'passwort'
end

require 'ramaze'

class MainController < Ramaze::Controller
  helper :auth
  layout :layout

  before(:index) { login_required }

  def index
    "Hello #{session[:username]}"
  end

  private

  def login_required
    flash[:error] = 'login required to view that page' unless logged_in?
    super
  end

  def check_auth user, pass
    return false if (not user or user.empty?) and (not pass or pass.empty?)

    if User[:username => user, :password => pass].nil?
      flash[:error] = 'invalid username or password'
      false
    else
      true
    end
  end
end

Ramaze.start :adapter => :mongrel, :load_engines => :Haml