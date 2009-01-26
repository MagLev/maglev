#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TCCacheHelperController < Ramaze::Controller
  map :/

  helper :cache
  cache :cached_action

  def index
    self.class.name
  end

  def cached_value
    value_cache[:time] ||= random
  end

  def alt_cached_value
    cache[:rand] ||= random
  end

  def uncache_values
    value_cache.delete :time
    cache.delete :rand
  end

  def cached_action
    random
  end

  def uncache_actions
    action_cache.clear
  end

  private

  def random
    [Time.now.usec, rand].inspect
  end
end

class TCCacheHelperTTLController < Ramaze::Controller
  map '/ttl'

  helper :cache
  cache :index, :ttl => 1

  def cached_list
    actions_cached.inspect
  end

  def index
    rand
  end
end

class TCCacheHelperKeyController < Ramaze::Controller
  map '/key'

  helper :cache
  cache :name, :key => lambda{ request['name'] }

  def cached_list
    actions_cached.inspect
  end

  def name
    "hi #{request['name']} #{rand}"
  end
end

class TCCacheHelperOldController < Ramaze::Controller
  map '/old'

  helper :cache
  trait :actions_cached => [:index, :action]

  def index
    rand
  end

  def action with, param
    with + param + rand.to_s
  end
end



# TODO:
#   * specs fail horribly with the yaml store.
caches = {:memory => 'Hash'}

begin
  require 'memcache'
  caches[:memcached] = 'Ramaze::MemcachedCache'
rescue LoadError
  puts "skipping memcached"
end

caches.each do |cache, name|
  Ramaze::Global.cache = cache

  describe "CacheHelper on #{name}" do
    behaves_like 'http'
    ramaze

    def req(path='/', *args) get(path, *args).body end

    it "testrun" do
      req.should == 'TCCacheHelperController'
    end

    it "cached value" do
      3.times do
        lambda{ req('/cached_value') }.should.not.change{ req('/cached_value') }
      end

      3.times do
        lambda{ req('/uncache_values') }.should.change{ req('/cached_value') }
      end

      lambda{ req('/uncache_values') }.should.change{ req('/alt_cached_value') }
    end

    it "cached action" do
      3.times do
        lambda{ req('/cached_action') }.should.not.change{ req('/cached_action') }
      end

      3.times do
        lambda{ req('/uncache_actions') }.should.change{ req('/cached_action') }
      end
    end

    it "should support options" do
      req('/ttl/cached_list').should == {:index=>{:ttl=>1}}.inspect
      req('/key/cached_list').should =~ /^\{:name=>\{:key=>/
    end

    it "should expire cache after time-to-live" do
      orig_value = req('/ttl')
      req('/ttl').should == orig_value
      sleep 1
      req('/ttl').should.not == orig_value
    end

    it "should cache using key lambda if provided" do
      req('/key/name', :name=>'Aman').should == req('/key/name', :name=>'Aman')
      req('/key/name', :name=>'Bob').should =~ /^hi Bob/
    end

    it "should remain backwards compatible" do
      lambda{ req('/old') }.should.not.change{ req('/old') }
      lambda{ req('/old/action/two/three') }.should.not.change{ req('/old/action/one/two') }
      req('/old/action/two/three').should =~ /^twothree/
    end
  end
end
