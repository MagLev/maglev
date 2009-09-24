# MiniTest suite for the MDB::Server
require 'rubygems'
require 'minitest/spec'
require 'mdb/server'

require 'helpers'

MiniTest::Unit.autorun

describe "MDB::Server class side" do
  before do
    MDB::Test.delete_test_dbs
  end

  it "raises DatabaseNotFound when deleteing a non-existent db" do
    key = MDB::Test.db_name "bogus"
    MDB::Server[key].must_be_nil
    proc { MDB::Server.delete(key) }.must_raise MDB::Server::DatabaseNotFound
  end

  it "raises DatabaseExists when creating a pre-existent db" do
    key = MDB::Test.db_name "X"
    MDB::Server[key].must_be_nil
    MDB::Server.create(key, ViewClass)
    MDB::Server[key].wont_be_nil
    proc { MDB::Server.create(key, ViewClass) }.must_raise MDB::Server::DatabaseExists
  end

  it "key? returns true iff there is a db of that name" do
    key = MDB::Test.db_name "X"
    MDB::Server.key?(key).must_equal false
    MDB::Server.create(key, ViewClass)
    MDB::Server.key?(key).must_equal true
  end

  it "creates, deletes and accesses databases" do
    key = MDB::Test.db_name "a_db"
    MDB::Server[key].must_be_nil

    MDB::Server.create(key, ViewClass)
    MDB::Server[key].wont_be_nil

    MDB::Server.delete(key)
    MDB::Server[key].must_be_nil
  end

  it "lists the databases it has" do
    MDB::Server.db_names.grep(MDB::Test::TEST_DB_PREFIX).must_be_empty
    db_names = %w(foo bar quux)
    db_names.each do |name|
      full_name = (MDB::Test.db_name name).to_sym
      MDB::Server.create(full_name, ViewClass)
      full_name.must_include MDB::Server.db_names
    end
  end
end

