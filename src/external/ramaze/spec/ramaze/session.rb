#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TCSessionController < Ramaze::Controller
  map '/'

  def index
    session.inspect
  end

  def set_session key, val
    session[key] = val
    index
  end

  def post_set_session
    session.merge! request.params
    index
  end

  def test_set(n)
    session[:n] = n
  end

  def test_result
    session[:n]
  end
end

describe "Session" do
  behaves_like 'http', 'browser'
  ramaze :adapter => :webrick

  { :MemoryCache => :memory,
    :YAMLStoreCache => :yaml_store,
    :MemcachedCache => :memcached,
  }.each do |cache, requirement|
    begin
      require "ramaze/cache/#{requirement}"
    rescue LoadError => ex
      puts ex
      next
    end

    describe cache do

      Ramaze::Global.cache = cache
      Thread.main[:session_cache] = nil

      b = Browser.new

      it "Should give me an empty session" do
        b.eget.should == {}
      end

      it "set some session-parameters" do
        b.eget('/set_session/foo/bar').should == {'foo' => 'bar'}
      end

      it "inspect session again" do
        b.eget('/').should == {'foo' => 'bar'}
      end

      it "change the session" do
        b.eget('/set_session/foo/foobar')['foo'].should == 'foobar'
      end

      it "inspect the changed session" do
        b.eget('/')['foo'].should == 'foobar'
      end

      it "now a little bit with POST" do
        b.epost('/post_set_session', 'x' => 'y')['x'].should == 'y'
      end

      it "snooping a bit around" do
        b.cookie.split('=').size.should == 3
      end

      it 'should not hit IP_COUNT_LIMIT for the same session/ip' do
        class Ramaze::Session
          remove_const :IP_COUNT_LIMIT
          const_set(:IP_COUNT_LIMIT, 2)
        end
        (0..Ramaze::Session::IP_COUNT_LIMIT * 2).each do |n|
          b.get("/test_set/#{n}")
          b.get("/test_result").should == n.to_s
        end
      end
    end
  end
end
