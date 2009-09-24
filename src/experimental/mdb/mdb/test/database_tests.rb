# MiniTest suite for the MDB::Database
require 'rubygems'
require 'minitest/spec'
require 'mdb/database'
require 'mdb/server'

require 'helpers'

MiniTest::Unit.autorun

DB_NAME = MDB::Test.db_name 'database_tests'

describe MDB::Database do

  before do
    MDB::Test.delete_test_dbs
    MDB::Server.create(DB_NAME, ViewClass)
    @db = MDB::Server[DB_NAME]
  end

  it 'executes the requested view passed either a symbol or a string or to_sym-able' do
    @db.execute_view('view_42').must_equal 42
    @db.execute_view(:view_42).must_equal 42
  end

  it 'raises NoSuchView if there is no view of the given name' do
    proc { @db.execute_view(:not_a_view_name) }.must_raise MDB::Database::NoViewError
  end
end

