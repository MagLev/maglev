require 'spec/helper'
require 'examples/misc/simple_auth'

describe "SimpleAuth" do
  behaves_like 'browser'
  ramaze(:adapter => :webrick)

  it 'should show start page' do
    Browser.new do
      http.basic_authentication "jill", "password1"
      get('/').should == "Secret Info"
    end

    Browser.new do
      http.basic_authentication "jack", "password2"
      get('/').should == "Secret Info"
    end
  end

  it 'should not show start page' do
    Browser.new do
      lambda{ get('/') }.should.raise
    end

    Browser.new do
      lambda do
        http.basic_authentication "hello", "world"
        lambda{ get('/') }.should.raise
      end
    end
  end
end
