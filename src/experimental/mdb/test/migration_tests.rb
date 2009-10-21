# MiniTest suite for migrations.rb
# Fixtures are in

require 'rubygems'
require 'minitest/spec'
require 'migration'

MiniTest::Unit.autorun

describe Maglev::Migration do
  it "can find full qualified classes and modules" do
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
end
