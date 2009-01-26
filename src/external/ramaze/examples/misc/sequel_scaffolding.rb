require 'rubygems'
require 'ramaze'
require 'sequel'
require 'scaffolding_extensions'

DB = Sequel.sqlite

class User < Sequel::Model(:user)
  set_schema do
    primary_key :id
    varchar :name
    text :description
  end

  create_table unless User.table_exists?
end

ScaffoldingExtensions.all_models = [User]

class UserController < Ramaze::Controller
  scaffold_all_models :only => [User]
end

class MainController < Ramaze::Controller
  def index
    %{Scaffolding extension enabled for
      <a href="http://sequel.rubyforge.org/classes/Sequel/Model.html">
      Sequel::Model
      </a> User.
      You can access the scaffolded Model at #{A('/user')}}
  end
end

Ramaze.start
