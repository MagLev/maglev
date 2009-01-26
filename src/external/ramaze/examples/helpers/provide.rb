require 'ramaze'
require 'fjson'

class User
  def to_yaml
    "Look, I'm YAML!".to_yaml
  end

  def to_json
    "Look, I'm YSON!".to_json
  end
end

class MainController < Ramaze::Controller
  helper :provide
  provides :yaml, :json

  def user
    display User.new
  end
end

Ramaze.start
