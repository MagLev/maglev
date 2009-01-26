#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'
require 'open-uri'

class TCErrorController < Ramaze::Controller
  map :/

  def index
    self.class.name
  end

  def erroring
    blah
  end
end

class TCErrorCustomController < Ramaze::Controller
  map "/custom"

  def error
    "The error page of custom"
  end

  def erroring
    bluh
  end
end

describe "Error" do
  behaves_like 'http'
  ramaze :error_page => true,
         :view_root => 'spec/ramaze/public'

  @handle_error = Ramaze::Dispatcher::Error::HANDLE_ERROR
  @orig_handle_error = @handle_error.dup

  before do
    Ramaze::Cache.resolved.clear
    Ramaze::Cache.patterns.clear
    @handle_error.clear
    @handle_error.merge!(@orig_handle_error)
  end

  it 'should resolve custom error pages per controller' do
    response = get("/custom/does_not_exist")
    response.status.should == 404
    response.body.should == "The error page of custom"
  end

  it 'should throw errors from rendering' do
    response = get('/erroring')
    response.status.should == 500
    regex = %r(undefined local variable or method `blah' for .*?TCErrorController)
    response.body.should =~ regex
  end

  it 'should throw errors from rendering' do
    response = get('/custom/erroring')
    response.status.should == 500
    response.body.should == "The error page of custom"
  end

  it 'should give 404 when no action is found' do
    response = get('/foobar')
    response.status.should == 404
    response.body.should =~ %r(No Action found for `/foobar' on TCErrorController)
  end

  it "should give custom status when no action is found" do
    @handle_error[Ramaze::Error::NoAction] = [707, '/error']

    response = get('/illegal1')
    response.status.should == 707
    response.body.should =~ %r(No Action found for `/illegal1' on TCErrorController)
  end

  it "should give 404 when no controller is found" do
    old_mapping = Ramaze::Global.mapping.dup
    Ramaze::Global.mapping.clear
    response = get('/illegal2')
    response.status.should == 404
    response.body.should =~ %r(No Controller found for `/error')
    Ramaze::Global.mapping = old_mapping
  end

  it "should return custom error page" do
    @handle_error[Ramaze::Error::NoAction] = [404, '/error404']
    response = get('/illegal3')
    response.status.should == 404
    response.body.should == '404 - not found'
  end
end
