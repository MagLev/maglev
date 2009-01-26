#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

caches = {:memory => 'Hash', :yaml => 'Ramaze::YAMLStoreCache'}

begin
  require 'memcache'
  caches[:memcached] = 'Ramaze::MemcachedCache'
rescue LoadError
  puts "skipping memcached"
end

caches.each do |cache, name|
  describe "#{name} setup" do
    should 'be assignable to Global' do
      Ramaze::Global.cache = cache
      Ramaze::Global.cache.to_s.should == name
    end

    should 'do .new' do
      @cache = Ramaze::Global.cache.new
      @cache.class.name.should == name
    end
  end

  describe "#{name} modification" do
    before do
      Ramaze::Global.cache = cache
      @cache = Ramaze::Global.cache.new
    end

    after do
      @cache.clear
    end

    should 'be assignable with #[]=' do
      @cache[:foo] = :bar
      @cache[:foo].should == :bar
    end

    should 'be retrievable with #[]' do
      @cache[:yoh] = :bar
      @cache[:yoh].should == :bar
    end

    should 'delete keys' do
      @cache[:bar] = :duh
      @cache.delete(:bar)
      @cache[:bar].should == nil
    end

    should 'show values for multiple keys' do
      @cache[:baz] = :foo
      @cache[:beh] = :feh
      @cache.values_at(:baz, :beh).should == [:foo, :feh]
    end

    FileUtils.rm(@cache.file) if cache == :yaml
  end
end

describe "Cache wrapper" do
  before do
    @cache = Ramaze::Cache.new(Hash)
  end

  after do
    @cache.clear
  end

  should 'be assignable with #[]= and retrievable with #[]' do
    @cache[:foo] = :bar
    @cache[:foo].should == :bar
  end

  should 'return nil if key not found' do
    @cache[:baz].should == nil
  end

  should 'be assignable with #store and retrievable with #fetch' do
    @cache.store(:foo, :bar)
    @cache.fetch(:foo).should == :bar
  end

  should 'return default value if key not found' do
    @cache.fetch(:monkeys, :default).should == :default
  end

  should 'be assignable with #set and retrievable with #get' do
    @cache.set(:ninjas, :totally_sweet)
    @cache.get(:ninjas).should == :totally_sweet
  end

  should 'expire key after ttl' do
    @cache[:cow].should == nil
    @cache.store(:cow, :moo, :ttl => 1)
    @cache[:cow].should == :moo
    sleep(1.1)
    @cache[:cow].should == nil
  end

  should 'accept an int as the ttl parameter for #store' do
    @cache[:cow].should == nil
    @cache.store(:cow, :moo, 1)
    @cache[:cow].should == :moo
  end

  should 'delete keys' do
    @cache[:abc] = :cba
    @cache[:def] = :fed
    @cache.delete(:abc, :def)
    @cache[:abc].should == nil
    @cache[:def].should == nil
  end

  should 'show values for multiple keys' do
    @cache[:baz] = :foo
    @cache[:beh] = :feh
    @cache.values_at(:baz, :beh).should == [:foo, :feh]
  end

  should 'clear' do
    @cache[:moo] = :cow
    @cache.clear
    @cache[:moo].should == nil
  end

  should 'not allow different cache namespaces to overlap' do
    Ramaze::Cache.add :foo
    Ramaze::Cache.add :bar

    key = "foobar"
    Ramaze::Cache.foo[key] = 'foo'
    Ramaze::Cache.bar[key] = 'bar'

    Ramaze::Cache.foo[key].should.not == Ramaze::Cache.bar[key]
  end
end
