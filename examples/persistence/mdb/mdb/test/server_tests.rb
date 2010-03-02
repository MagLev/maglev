# MiniTest suite for the MDB::Server
# Fixtures are in mdb_fixtures.rb, loaded by ../Rakefile

require 'rubygems'
require 'minitest/spec'
require 'mdb/server'

require 'helpers'

MiniTest::Unit.autorun

describe 'MDB::Server class side' do
  before do
    MDB::Test.delete_test_dbs
    @server = MDB::Server.server
  end

  it 'creates, deletes and accesses databases' do
    key = MDB::Test.db_name 'a_db'
    @server[key].must_be_nil

    @server.create(key, ViewClass)
    @server[key].wont_be_nil

    @server.delete(key)
    @server[key].must_be_nil
  end

  it 'delete raises DatabaseNotFound when deleteing a non-existent db' do
    key = MDB::Test.db_name 'bogus'
    @server[key].must_be_nil
    proc { @server.delete(key) }.must_raise MDB::Server::DatabaseNotFound
  end

  it 'create raises DatabaseExists when db already exists' do
    key = MDB::Test.db_name 'X'
    @server[key].must_be_nil
    @server.create(key, ViewClass)
    @server[key].wont_be_nil
    proc { @server.create(key, ViewClass) }.must_raise MDB::Server::DatabaseExists
    @server.delete(key)
  end

  it 'create returns the newly created db' do
    key = MDB::Test.db_name 'X'
    @server[key].must_be_nil
    db = @server.create(key, ViewClass)
    db.wont_be_nil
    db.must_equal @server[key]
  end

  it 'key? returns true iff there is a db of that name' do
    key = MDB::Test.db_name 'X'
    @server.key?(key).must_equal false
    @server.create(key, ViewClass)
    @server.key?(key).must_equal true
  end

  it 'db_names lists the databases it has' do
    @server.db_names.grep(MDB::Test::TEST_DB_PREFIX).must_be_empty
    db_names = %w(foo bar quux)
    db_names.each do |name|
      full_name = (MDB::Test.db_name name).to_sym
      @server.create(full_name, ViewClass)
      @server.db_names.must_include full_name
    end
  end

  it 'update edits existing databases with new views' do
    key = MDB::Test.db_name 'X'
    @server[key].must_be_nil
    proc { @server.update(key, ViewClass) }.must_raise MDB::Server::DatabaseNotFound
  end

  it 'update raises DatabaseNotFound if the database does not exist' do
    key = MDB::Test.db_name 'X'
    @server[key].must_be_nil
    @server.create(key, ViewClass)
    db = @server[key]
    db.execute_view(:view_42).must_equal 42  # Ensure old view is in place
    @server.update(key, ViewClass2)
    db.execute_view(:view_42).must_equal 43  # Ensure new view is in place
  end
end
