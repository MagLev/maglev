#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'
spec_require 'remarkably/engines/html'

class TCTemplateRemarkablyController < Ramaze::Controller
  map '/'
  view_root 'spec/ramaze/template/remarkably/'
  engine :Remarkably

  include Remarkably::Common

  def index
    h1 "Remarkably Index"
  end

  def links
    ul do
      li { a "Index page", :href => R(self,:index) }
      li { a "Internal template", :href => R(self,:internal) }
      li { a "External template", :href => R(self,:external) }
    end
  end

  def external
  end

  def sum num1, num2
    @num1, @num2 = num1.to_i, num2.to_i
  end
end

describe "Remarkably" do
  behaves_like 'http'
  ramaze

  def retrieve(*url)
    Ramaze::Controller.handle(*url)
  end

  it "index" do
    retrieve('/').should == '<h1>Remarkably Index</h1>'
  end

  it "links" do
    retrieve('/links').should == '<ul><li><a href="/index">Index page</a></li><li><a href="/internal">Internal template</a></li><li><a href="/external">External template</a></li></ul>'
  end

  it "sum" do
    retrieve('/sum/1/2').should == '<div>3</div>'
  end

  it "external" do
    retrieve('/external').should == "<html><head><title>Remarkably Test</title></head><body><h1>Remarkably Template</h1></body></html>"
  end

end
