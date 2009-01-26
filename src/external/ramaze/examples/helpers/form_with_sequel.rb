require 'sequel'
require 'ramaze'

DB = Sequel.sqlite

class User < Sequel::Model
  set_schema do
    varchar :name
    text :description
    date :created
  end
end

User.create_table

class MainController < Ramaze::Controller
  helper :form

  def index
    form_for User
  end
end

Ramaze.start
