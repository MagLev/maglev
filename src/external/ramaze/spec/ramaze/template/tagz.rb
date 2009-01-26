#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

spec_require 'tagz'


class TCTemplateTagzController < Ramaze::Controller
  map '/'
  view_root 'spec/ramaze/template/tagz/'
  engine :Tagz

  helper :tagz

  def index
    tagz{ h1_{ "Tagz Index" } }
  end

  def links
    tagz{
      ul_{
        li_{ a_(:href => R(self,:index)){ "Index page" } }
        li_{ a_(:href => R(self,:internal)){ "Internal template" } }
        li_{ a_(:href => R(self,:external)){ "External template" } }
      }
    }
  end

  def external
  end

  def sum num1, num2
    @num1, @num2 = num1.to_i, num2.to_i
  end
end

describe "Tagz" do
  behaves_like 'http'
  ramaze

  it "index" do
    get('/').body.should == '<h1>Tagz Index</h1>'
  end

  it "links" do
    get('/links').body.should == '<ul><li><a href="/index">Index page</a></li><li><a href="/internal">Internal template</a></li><li><a href="/external">External template</a></li></ul>'
  end

  it "sum" do
    get('/sum/1/2').body.should == '<div>3</div>'
  end

  it "external" do
    get('/external').body.should == "<html><head><title>Tagz Test</title></head><body><h1>Tagz Template</h1></body></html>"
  end

  it "should not respond to tagz" do
    response = get('/tagz')
    response.status.should == 500
  end
end
