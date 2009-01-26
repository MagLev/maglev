#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

spec_require 'markaby'

class TCTemplateMarkabyController < Ramaze::Controller
  map '/'
  view_root 'spec/ramaze/template/markaby/'
  engine :Markaby

  helper :markaby

  def index
    mab { h1 "Markaby Index" }
  end

  def links
    mab do
      ul do
        li { a "Index page", :href => R(self,:index) }
        li { a "Internal template", :href => R(self,:internal) }
        li { a "External template", :href => R(self,:external) }
      end
    end
  end

  def external
  end

  def sum num1, num2
    @num1, @num2 = num1.to_i, num2.to_i
  end
end

describe "Markaby" do
  behaves_like 'http'
  ramaze :error_page => true

  it "index" do
    get('/').body.should == '<h1>Markaby Index</h1>'
  end

  it "links" do
    get('/links').body.should == '<ul><li><a href="/index">Index page</a></li><li><a href="/internal">Internal template</a></li><li><a href="/external">External template</a></li></ul>'
  end

  it "sum" do
    get('/sum/1/2').body.should == '<div>3</div>'
  end

  it "external" do
    get('/external').body.should == "<html><head><meta content=\"text/html; charset=utf-8\" http-equiv=\"Content-Type\"/><title>Markaby Test</title></head><body><h1>Markaby Template</h1></body></html>"
  end

  it "should not respond to mab" do
    response = get('/mab')
    response.status.should == 404
  end
end
