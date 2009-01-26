require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  trait :logins => {
   'jill' => 'password1',
   'jack' => 'password2',
  }.map{|k,v| ["#{k}:#{v}"].pack('m').strip }

  helper :aspect

  before_all do
    check_auth
  end

  def index
    'Secret Info'
  end

  private

  def check_auth
    response['WWW-Authenticate'] = 'Basic realm="Login Required"'

    if auth = request.env['HTTP_AUTHORIZATION']
      if class_trait[:logins].include?(auth.split.last)
        return true
      end
    end

    respond 'Unauthorized', 401
  end
end

Ramaze.start :adapter => :mongrel
