require 'spec/helper'

spec_require 'sequel'

begin
  DB = Sequel.sqlite
rescue NoMethodError
  raise LoadError, 'Install latest Sequel gem'
end

require 'ramaze/contrib/sequel/fill'

class Person < Sequel::Model(:person)
  set_schema do
    primary_key :id
    text :name
  end
end

Person.create_table!

class MainController < Ramaze::Controller
  def index
    'Hello, World!'
  end

  def insert
    person = Person.fill
    person.save
  end

  def show id
    Person[id.to_i].name
  end
end

describe 'Route' do
  behaves_like 'http'
  ramaze

  it 'should fill values from current request' do
    insert = get('/insert', 'name' => 'manveru')
    insert.status.should == 200
    person = get('/show/1')
    person.body.should == 'manveru'
  end
end
