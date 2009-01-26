#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

spec_require 'amrita2'

class TCTemplateAmritaController < Ramaze::Controller
  view_root __DIR__(:amrita2)
  engine :Amrita2

  def internal
    @data = { :hello => "hello world" }
    "<h1 am:src='hello' />"
  end

  def external
    @data = {
      :title => "hello world",
      :body => "Amrita2 is an HTML template library for Ruby"
    }
  end

  def sum
    @data = { :one => 1, :two => 2 }
  end
end

describe "Simply calling" do
  behaves_like 'http'
  ramaze(:mapping => {'/' => TCTemplateAmritaController})

  it "should respond to /internal" do
    get('/internal').body.strip.should == "<h1>hello world</h1>"
  end

  it "should respond to /external" do
    get('/external').body.strip.should ==
%{<html>
  <body>
    <h1>hello world</h1>
    <p>Amrita2 is an HTML template library for Ruby</p>
  </body>
</html>}
  end

  it "should respond to /sum" do
    get('/sum').body.strip.should == "1 + 2 = 3"
  end
end
