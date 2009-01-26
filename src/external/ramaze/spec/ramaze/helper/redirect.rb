#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TCRedirectHelperController < Ramaze::Controller
  map :/

  def index
    self.class.name
  end

  def noop
    'noop'
  end

  def redirection
    redirect :index
  end

  def double_redirection
    redirect :redirection
  end

  def redirect_referer_action
    redirect_referer
  end

  def no_actual_redirect
    catch(:redirect){ redirection }
    'foo'
  end

  def no_actual_double_redirect
    catch(:redirect){ double_redirection }
    'bar'
  end

  def redirect_method
    redirect Rs(:noop)
  end

  def absolute_uri_redirect
    redirect 'http://localhost:7007/noop'
  end

  def loop
    respond 'no loop'
    'loop'
  end

  def respond_with_status
    respond 'not found', 404
  end

  def redirect_unmodified
    raw_redirect '/noop'
  end
end

describe "RedirectHelper" do
  behaves_like 'http', 'browser'
  ramaze(:adapter => :webrick)

  b = Browser.new

  it "testrun" do
    b.get('/').should == "TCRedirectHelperController"
  end

  it "should do redirection" do
    b.story do
      get('/redirection').should == "TCRedirectHelperController"
      get('/double_redirection').should == "TCRedirectHelperController"
    end
  end

  it 'should be possible to catch a redirect' do
    b.story do
      get('/no_actual_redirect').should == 'foo'
      get('/no_actual_double_redirect').should == 'bar'
    end
  end

  it "should redirect to referer" do
    b.story do
      get('/').should == "TCRedirectHelperController"
      get('/redirect_referer_action').should == 'TCRedirectHelperController'
      get('/noop').should == 'noop'
      get('/redirect_referer_action').should == 'noop'
    end
  end

  it "should work with R()" do
    b.get('/redirect_method').should == 'noop'
  end

  it "should work with absolute uris" do
    b.get('/absolute_uri_redirect').should == 'noop'
  end

  it 'should allow respond() that ignores return values and templates' do
    get('/loop').body.should == 'no loop'
    page = get('/respond_with_status')
    page.status.should == 404
    page.body.should == 'not found'
  end

  it 'should redirect without modifying the target' do
    get('/redirect_unmodified').headers['Location'].should == '/noop'
  end
end
