require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  helper :rest

  on_get
  def index
    'Hello, World'
  end

  on_post
  def create
    p request.params
  end

  on_delete
  def delete
    'Deleted stuff'
  end

  on_put
  def insert
    'Inserted stuff'
  end
end

Ramaze.start
