#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'
require 'ramaze/helper/link'

class TCLink < Ramaze::Controller
  map '/'
  def index; end
end

class TCLink2 < Ramaze::Controller
  map '/2'
  def index; end
end

describe "A" do
  extend Ramaze::Helper::Link
  behaves_like 'resolve'

  ramaze

  it 'should build links' do
    A('title', :href => '/').should == %(<a href="/">title</a>)
    A('title', :href => '/foo').should == %(<a href="/foo">title</a>)
    A('title', :href => '/foo?x=y').should == %{<a href="/foo?x=y">title</a>}
    stack('/'){ A('/foo?x=y') }.should == %{<a href="/foo?x=y">/foo?x=y</a>}

    a = A('title', :href => '/foo', :class => :none)
    a.should =~ /class="none"/
    a.should =~ /href="\/foo"/
  end

  it 'should build position independend links' do
    stack('/'){ A(TCLink, :foo) }.should == %(<a href="/foo">foo</a>)
  end

  it 'should escape path' do
    stack('/'){ A('ti tle') }.should == '<a href="/ti+tle">ti tle</a>'
    a = A('', :href => "/foo?chunky=b\000acon")
    a.should == '<a href="/foo?chunky=b%00acon"></a>'
  end

  it 'should handle text key' do
    A(:href => '/', :text => 'text').should == '<a href="/">text</a>'
  end

  it 'should use last argument as first text fallback' do
    a = A('text', :href => '/', :title => 'title')
    a.should =~ /href="\/"/
    a.should =~ /title="title"/
    a.should =~ />text</
  end

  it 'should use :title as second text fallback' do
    a = A(:href => '/', :title => 'text')
    a.should =~ /title="text"/
    a.should =~ /href="\/"/
    a.should =~ />text</
  end

  it 'should use :href as third text fallback' do
    A(:href => '/').should == '<a href="/">/</a>'
  end
end

describe 'R' do
  extend Ramaze::Helper::Link

  it 'should build urls' do
    R(TCLink).should == '/'
    R(TCLink, :foo).should == '/foo'
    R(TCLink, :foo, :bar).should == '/foo/bar'
    R(TCLink, :foo, :bar => :baz).should == '/foo?bar=baz'
  end
end

describe 'Rs' do
  extend Ramaze::Helper::Link

  def resolve(url)
    Ramaze::Controller::resolve(url)
  end

  it 'should build links for current controller' do
    resolve('/2').stack{ Rs(:index).should == '/2/index' }
    resolve('/').stack{ Rs(:index).should == '/index' }
  end

  it 'should treat Rs() like R() when Controller given' do
    resolve('/2').stack{ Rs(TCLink, :index).should == '/2/index' }
  end

  it 'should treat non-controllers as strings' do
    resolve('/2').stack{ Rs(Ramaze, :index).should == '/2/Ramaze/index' }
  end

end

describe 'breadcrumbs' do
  extend Ramaze::Helper::Link

  it 'should lay out breadcrumbs' do
    breadcrumbs('/file/dir/listing/is/cool').
      should == [
      "<a href=\"/file\">file</a>",
      "<a href=\"/file/dir\">dir</a>",
      "<a href=\"/file/dir/listing\">listing</a>",
      "<a href=\"/file/dir/listing/is\">is</a>",
      "<a href=\"/file/dir/listing/is/cool\">cool</a>"
    ].join('/')
  end

  it 'should lay out breadcrumbs with href prefix' do
    breadcrumbs('/file/dir/listing/is/cool', '/', '/', '/prefix/path').
      should == [
      "<a href=\"/prefix/path/file\">file</a>",
      "<a href=\"/prefix/path/file/dir\">dir</a>",
      "<a href=\"/prefix/path/file/dir/listing\">listing</a>",
      "<a href=\"/prefix/path/file/dir/listing/is\">is</a>",
      "<a href=\"/prefix/path/file/dir/listing/is/cool\">cool</a>"
    ].join('/')
  end
end
