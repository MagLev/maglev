#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TCFlashHelperFirstController < Ramaze::Controller
  map :/
  helper :flash

  def index
    self.class.name
  end

  def first_here
    flash[:first] = 'hey'
  end

  def then_here
    flash[:first].to_s
  end

  def set(*hash)
    Hash[*hash].each do |key, value|
      flash[key] = value
    end
    hash.inspect
  end

  def box
    flashbox
  end

  def check_empty
    flash.empty?
  end
end

class TCFlashHelperSecondController < Ramaze::Controller
  map '/second'
  helper :flash

  def index
    self.class.name
  end

  def first_here
    flash[:first] = 'hey'
  end

  def then_here
    flash[:first].to_s
  end
end

class TCFlashHelperThirdController < Ramaze::Controller
  map '/third'
  helper :flash

  def index
  end

  def noop
    'noop'
  end

  def set par
    flash[:e] = par
  end

  def retrieve
    flash[:e]
  end
end

describe Ramaze::Helper::Flash do
  behaves_like 'browser'
  ramaze :adapter => :webrick

  should "set and forget flash twice" do
    Browser.new do
      get('/first_here')
      get('/then_here').should == 'hey'
      2.times{ get('/then_here').should.be.empty }
      get('/first_here')
      get('/then_here').should == 'hey'
      get('/then_here').should.be.empty
    end
  end

  should "work over multiple controllers" do
    Browser.new do
      get('/first_here')
      get('/second/then_here').should == 'hey'
      get('/then_here').should.be.empty
      get('/second/then_here').should.be.empty
      get('/second/first_here')
      get('/then_here').should == 'hey'
      get('/second/then_here').should.be.empty
    end
  end

  should "set and retrieve custom value" do
    Browser.new do
      get('/third/set/foo').should == 'foo'
      get('/third/retrieve').should == 'foo'
      2.times{ get('/third/retrieve').should.be.empty }
    end
  end

  should "show a flashbox" do
    Browser.new do
      error_div = "<div class='flash' id='flash_error'>stuff failed</div>"
      success_div = "<div class='flash' id='flash_success'>other things worked</div>"

      get('/set/error/stuff%20failed')
      get('/box').should == error_div

      get('/set/error/stuff%20failed/success/other%20things%20worked')
      get('/box').split("\n").sort.should == [error_div, success_div].sort

      get('/box').should.be.empty
    end
  end

  should 'check if flash is empty' do
    Browser.new do
      get('/check_empty').should == 'true'
      get('/set/one/two')
      get('/check_empty').should == 'false'
      get('/check_empty').should == 'true'
    end
  end
end
