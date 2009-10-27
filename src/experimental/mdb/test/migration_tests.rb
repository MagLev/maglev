# MiniTest suite for migrations.rb

require 'rubygems'
require 'minitest/spec'
# HACK:  There is an extra reference, somewhere, to Maglev::Migration,
# which gets persisted.  So, we have to make this persistable, until we
# fix the bug
Maglev.persistent { require 'migration' }

require 'maglev/repository'

MiniTest::Unit.autorun

# Exception.install_debug_block do |e|
#   puts "--- (#{e.class}): #{e}"
#   case e
#   when NameError
#     nil.pause
#   end
# end


describe Maglev::Migration do
  it "can find fully qualified classes and modules" do
    [['Object',         [Object]],
     ['::Object',       [Object]],
     ['IO',             [Object, IO]],
     ['::IO',           [Object, IO]],
     ['IO::SEEK_END',   [Object, IO, IO::SEEK_END]],
     ['::IO::SEEK_END', [Object, IO, IO::SEEK_END]]
    ].each do |name,klass|
      Maglev::Migration.get_path(name).must_equal klass
    end
  end

  it "raises NameError when it can't find a class" do
    ['Objct', '::NOT_THERE_I_HOPE','IO::SEEK_END::Foo'].each do |name|
      proc { Maglev::Migration.get_path(name) }.must_raise NameError
    end
  end

  it 'removes classes and module from their parent namespaces' do
    name = :MigrationTestClassFoo
    klass = Class.new
    proc { Object.const_get(name) }.must_raise NameError
    Object.const_set(name, klass)
    Object.const_get(name.to_s).must_equal klass
    removed_klass = Maglev::Migration.remove_from_parent(name)
    removed_klass.must_equal klass  # TODO: is there a must_be?
    Object.const_defined?(name).must_equal false
    proc { Object.const_get(name) }.must_raise NameError
  end

  it 'raises NameError if asked to remove a bogus class' do
    bogus = 'Object::Foo::Quux::NotAClass::ForSure'
    proc { Maglev::Migration.remove_from_parent(bogus) }.must_raise NameError
  end

  describe 'immediate migration' do
    it 'must create persistable classes on the first migration' do
      clear_fixtures
      proc { Data.new(i) }.must_raise NameError
      ruby_source = contents_of 'version_one.rb'
      Maglev::Migration.migrate(:Data, ruby_source, false)

      data_123 = Data.new(123)
      data_123.x.must_equal 123
      data = fresh_root
      data << data_123
      Maglev.commit_transaction  # test it doesn't throw
    end

    it 'must upgrade classes on the second migration' do
      # Clear repository of all old versions of Data, then load the V1 of
      # data via a migration.
      clear_fixtures
      ruby_source = contents_of 'version_one.rb'
      Maglev::Migration.migrate(:Data, ruby_source, false)

      data = fresh_root

      # Create some V1 data and run a few checks
      old_class = Data
      2.times { |i| data << Data.new(i) }  # Put v1 data in db
      data.each_with_index do |d,i|
        d.must_be_kind_of old_class
        d.x.must_equal i
      end

      # Run the data migration to V2
      ruby_source = contents_of 'version_two.rb'
      Maglev::Migration.migrate(:Data, ruby_source, true)

      new_class = Data
      old_class.wont_equal new_class

      # Ensure all old data has been migrated to V2
      old_instances = Maglev::Repository.instance.list_instances([old_class])[0]
      old_instances.size.must_equal 0

      # Ensure all migrated instances look good
      data.each_with_index do |d,i|
        d.must_be_kind_of new_class
        d.x.must_equal i
        d.y.must_equal i * 3
        d.migrated.must_equal true
      end
    end

    it 'must migrate all instance variables by default' do
      clear_fixtures
      ruby_source = contents_of 'default_one.rb'
      Maglev::Migration.migrate(:DefaultV1, ruby_source, false)

      data = fresh_root
      d = DefaultV1.new; d.a = 10; data << d
      d = DefaultV1.new; d.b = 20; data << d
      d = DefaultV1.new; d.a = 10; d.b = 20; data << d

      DefaultV1::CONST_ONE.must_equal 1
      DefaultV1::CONST_TWO.must_equal 2
      (defined? DefaultV1::CONST_THREE).must_be_nil

      ruby_source = contents_of 'default_two.rb'
      Maglev::Migration.migrate(:DefaultV1, ruby_source, true)

      [[10,nil,nil], [nil,20,nil], [10,20,nil]].each_with_index do |x,i|
        data[i].a.must_equal x[0]
        data[i].b.must_equal x[1]
        data[i].c.must_equal x[2]
      end

      (defined? DefaultV1::CONST_ONE).must_be_nil
      DefaultV1::CONST_TWO.must_equal 4
      DefaultV1::CONST_THREE.must_equal 3
    end
  end
  describe 'lazy migration' do
    # The first migration must be non lazy

    it 'must upgrade classes on the second migration' do
      # Clear repository of all old versions of Data, then load the V1 of
      # data via a migration.
      clear_fixtures
      ruby_source = contents_of 'version_one.rb'
      Maglev::Migration.migrate(:Data, ruby_source, false) # non-lazy

      data = fresh_root

      # Create some V1 data and run a few checks
      old_class = Data
      2.times { |i| data << Data.new(i) }  # Put v1 data in db
      data.each_with_index do |d,i|
        d.must_be_kind_of old_class
        d.x.must_equal i
      end

      # Run the data migration to V2
      ruby_source = contents_of 'version_two_lazy.rb'
      Maglev::Migration.migrate_lazily(:Data, :DataV2, ruby_source)

      new_class = DataV2
      old_class.wont_equal new_class

      # Ensure all old data has been migrated to V2
      old_instances = Maglev::Repository.instance.list_instances([old_class])[0]
      old_instances.size.must_equal 2

      # Ensure all migrated instances look good
      data.each_with_index do |d,i|
        d.x
        d.must_be_kind_of new_class
        d.x.must_equal i
        d.y.must_equal i * 3
        d.migrated.must_equal true
      end
    end
  end
end

# Return a fresh persistent root for the test
def fresh_root
  Maglev::PERSISTENT_ROOT[:migration_tests] = Array.new
end

def clear_fixtures
  Maglev.abort_transaction
  Maglev::PERSISTENT_ROOT[:migration_tests] = nil
  [:Data, :DefaultV1].each do |sym|
    Maglev.persistent { Object.send :remove_const, sym } if Object.const_defined? sym
  end
  Maglev.commit_transaction
end

def contents_of(file_name)
  IO.read File.join(File.dirname(__FILE__), file_name)
end
