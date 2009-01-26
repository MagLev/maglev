#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TCParamsController < Ramaze::Controller
  map '/'

  def index
    "index"
  end

  def no_params
    "no params"
  end

  def single_param param
    "single param (#{param})"
  end

  def double_param param1, param2
    "double param (#{param1}, #{param2})"
  end

  def all_params *params
    "all params (#{params.join(', ')})"
  end

  def at_least_one param, *params
    "at least one (#{param}, #{params.join(', ')})"
  end

  def one_default param = 'default'
    "one_default (#{param})"
  end

  def cat1__cat11
    'cat1: cat11'
  end

  def cat1__cat11__cat111
    'cat1: cat11: cat111'
  end
end

class TCParamsController2 < Ramaze::Controller
  map '/jo'

  def add(one, two = nil, three = nil)
    "#{one}:#{two}:#{three}"
  end

  def keys
    request.params.keys.to_s.dump
  end
end

class TCParamsController3 < Ramaze::Controller
  map '/ma'

  def index(*args)
    request.params['foo'].to_s.dump
  end
end

describe "Simple Parameters" do
  behaves_like 'http'
  ramaze

  def handle(*url)
    Ramaze::Controller.handle(*url)
  end

  it "Should respond to no parameters given" do
    handle('/no_params').should == "no params"
  end

  it "Should respond to only / with the index" do
    handle('/').should == "index"
  end

  it "call /bar though index doesn't take params" do
    lambda{ handle('/bar') }.
      should.raise(Ramaze::Error::NoAction)
  end

  it "action that takes a single param" do
    handle('/single_param/foo').should == "single param (foo)"
  end

  it "action that takes two params" do
    handle('/double_param/foo/bar').should == "double param (foo, bar)"
  end

  it "action that takes two params but we give only one" do
    lambda{ handle('/double_param/foo') }.
      should.raise(Ramaze::Error::NoAction)
  end

  it "action that takes all params" do
    handle('/all_params/foo/bar/foobar').should == "all params (foo, bar, foobar)"
  end

  it "action that takes all params but needs at least one" do
    handle('/at_least_one/foo/bar/foobar').should == "at least one (foo, bar, foobar)"
  end

  it "action that takes all params but needs at least one (not given here)" do
    lambda{ handle('/at_least_one') }.
      should.raise(ArgumentError)
  end

  it "one default" do
    handle('/one_default').should == "one_default (default)"
  end

  it "one default" do
    handle('/one_default/my_default').should == "one_default (my_default)"
  end

  it "double underscore lookup" do
    handle('/cat1/cat11').should == 'cat1: cat11'
  end

  it "double double underscore lookup" do
    handle('/cat1/cat11/cat111').should == 'cat1: cat11: cat111'
  end


  it "jo/add should raise with 0 parameters" do
    lambda{ handle('/jo/add') }.
      should.raise(ArgumentError)
  end

  it "add should raise with 4 parameters" do
    lambda{ handle('/jo/add/1/2/3/4') }.
      should.raise(ArgumentError)
  end

  it "add should not raise with 1-3 parameters" do
    handle('/jo/add/1').should == '1::'
    handle('/jo/add/1/2').should == '1:2:'
    handle('/jo/add/1/2/3').should == '1:2:3'
  end

  it 'params should have no content without params' do
    get('/ma').body.should == '""'
  end

  it 'should have a parameter via QUERY_PARAMS' do
    get('/ma', 'foo' => 'bar').body.should == '"bar"'
  end

  it 'should handle valueless params' do
    raw_mock_request(:get, '/jo/keys?foo').body.should == '"foo"'
  end
end
