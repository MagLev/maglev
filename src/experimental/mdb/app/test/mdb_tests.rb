# MiniTest suite for the REST MDB wrapper.
# This test run by MRI
require 'rubygems'
require 'minitest/spec'
require 'mdb'
require 'app_model'

MiniTest::Unit.autorun

DB_NAME = 'rest_database_tests'

describe MDB::RESTDatabase do
  before do
    @db = MDB::RESTDatabase.new('http://localhost:4567', DB_NAME)
  end

  it 'starts off empty' do
    @db.size.must_equal 0
    @db.list_ids.size.must_equal 0
  end
end
