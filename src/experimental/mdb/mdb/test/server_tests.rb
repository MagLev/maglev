# MiniTest suite for the MDB::Server
require 'rubygems'
require 'minitest/spec'
require 'mdb/server'

require 'helpers'

MiniTest::Unit.autorun

describe 'MDB::Server class side' do
  before do
    MDB::Test.delete_test_dbs
  end

  it 'creates, deletes and accesses databases' do
    key = MDB::Test.db_name 'a_db'
    MDB::Server[key].must_be_nil

    MDB::Server.create(key, ViewClass)
    MDB::Server[key].wont_be_nil

    MDB::Server.delete(key)
    MDB::Server[key].must_be_nil
  end

  it 'delete raises DatabaseNotFound when deleteing a non-existent db' do
    key = MDB::Test.db_name 'bogus'
    MDB::Server[key].must_be_nil
    proc { MDB::Server.delete(key) }.must_raise MDB::Server::DatabaseNotFound
  end

  it 'create raises DatabaseExists when db already exists' do
    key = MDB::Test.db_name 'X'
    MDB::Server[key].must_be_nil
    MDB::Server.create(key, ViewClass)
    MDB::Server[key].wont_be_nil
    proc { MDB::Server.create(key, ViewClass) }.must_raise MDB::Server::DatabaseExists
  end

  it 'key? returns true iff there is a db of that name' do
    key = MDB::Test.db_name 'X'
    MDB::Server.key?(key).must_equal false
    MDB::Server.create(key, ViewClass)
    MDB::Server.key?(key).must_equal true
  end

  it 'db_names lists the databases it has' do
    MDB::Server.db_names.grep(MDB::Test::TEST_DB_PREFIX).must_be_empty
    db_names = %w(foo bar quux)
    db_names.each do |name|
      full_name = (MDB::Test.db_name name).to_sym
      MDB::Server.create(full_name, ViewClass)
      full_name.must_include MDB::Server.db_names
    end
  end

  it 'update edits existing databases with new views' do
    key = MDB::Test.db_name 'X'
    MDB::Server[key].must_be_nil
    proc { MDB::Server.update(key, ViewClass) }.must_raise MDB::Server::DatabaseNotFound
  end

  it 'update raises DatabaseNotFound if the database does not exist' do
    key = MDB::Test.db_name 'X'
    MDB::Server[key].must_be_nil
    MDB::Server.create(key, ViewClass)
    db = MDB::Server[key]
    db.execute_view(:view_42).must_equal 42  # Ensure old view is in place
    MDB::Server.update(key, ViewClass2)
    db.execute_view(:view_42).must_equal 43  # Ensure new view is in place
  end
end

