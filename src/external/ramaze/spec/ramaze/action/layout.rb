require 'spec/helper'

class TCActionLayout < Ramaze::Controller
  map '/'
  layout '/wrapper'

  def wrapper
    "<pre>#@content</pre>"
  end

  def index
    'Hello, World!'
  end

  def foo
    "bar"
  end
end

class TCActionOtherLayout < Ramaze::Controller
  map '/other'
  layout '/other_wrapper'
  view_root __DIR__(:view)

  def index
    "Others Hello"
  end
end

class TCActionSingleLayout < Ramaze::Controller
  map '/single'
  layout '/single_wrapper' => :index

  def index
    "Single Hello"
  end

  def without
    "Without wrapper"
  end
end

class TCActionDenyLayout < Ramaze::Controller
  map '/deny'
  layout '/single_wrapper'
  deny_layout :without

  def index
    "Single Hello"
  end

  def without
    "Without wrapper"
  end
end

class TCActionMultiLayout < Ramaze::Controller
  map '/multi'
  layout '/single_wrapper' => [:index, :second]

  def index
    "Single Hello"
  end

  def second
    "Second with layout"
  end

  def without
    "Without wrapper"
  end
end

class TCActionSubLayout < Ramaze::Controller
  map '/sub'
  layout :sub_wrapper

  def index
    "Sub Hello"
  end
end

class TCReUseActionLayout < Ramaze::Controller
  map '/reuse'
  layout '/wrapper'

  def index
    'hi'
  end

  def other_layout
    "#@title: #@content"
  end
end

class TCLayoutInstance < Ramaze::Controller
  map '/reuse_instance'
  layout '/reuse/other_layout'

  def index
    @title = 'hello'
    'world'
  end
end

class TCLayoutInstanceVars < Ramaze::Controller
  map '/instancevars'
  layout :layout

  def layout
    @title = "layout"
  end

  def test
    'test'
  end
end

class TCActionTwoLevelLayout < Ramaze::Controller
  map '/twolevel'
  layout :wrapper
  layout :wrapper2 => [:foo]

  def wrapper
    "<pre>#@content</pre>"
  end

  def wrapper2
    "<code>#@content</code>"
  end

  def index
    'Hello, World!'
  end

  def foo
    "bar"
  end
end

describe 'Action rendering' do
  behaves_like 'http'
  ramaze :view_root => __DIR__/:view

  it 'should work with layouts' do
    get('/').body.should == "<pre>Hello, World!</pre>"
    get('/foo').body.should == "<pre>bar</pre>"
    get('/bar').body.should == "<pre>Hello from bar</pre>"
  end

  it 'should work with layout from file' do
    get('/other').body.should == "<p>Others Hello</p>"
    get('/other/bar').body.should == "<p>Hello from bar</p>"
  end

  it 'should apply single layout' do
    get('/single').body.should == "<b>Single Hello</b>"
    get('/single/without').body.should == "Without wrapper"
  end

  it 'should deny a single action' do
    get('/deny').body.should == "<b>Single Hello</b>"
    get('/deny/without').body.should == "Without wrapper"
  end

  it 'should apply layout to a list of actions' do
    get('/multi').body.should == "<b>Single Hello</b>"
    get('/multi/second').body.should == "<b>Second with layout</b>"
    get('/multi/without').body.should == "Without wrapper"
  end

  it 'should apply relative layouts' do
    get('/sub').body.should == "<h1>SubWrapper</h1>"
  end

  it 'should use instance vars defined in the layout action in external templates' do
    get('/instancevars/test').body.should == 'layout: test'
  end

  it 'should allow re-using layouts from other controllers' do
    get('/reuse').body.should == "<pre>hi</pre>"
    get('/reuse_instance').body.should == "hello: world"
  end

  it 'should be able to override :all' do
    get('/twolevel/index').body.should == '<pre>Hello, World!</pre>'
    get('/twolevel/foo').body.should == '<code>bar</code>'
  end

end
