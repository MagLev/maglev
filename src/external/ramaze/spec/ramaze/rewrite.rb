require 'spec/helper'

class MainController < Ramaze::Controller
  def index
    'hello'
  end

  def test
    'test'
  end
end

describe 'Route' do
  behaves_like 'http'
  ramaze :public_root => __DIR__(:rewrite)

  Ramaze::Rewrite[%r|^/index.php(.*)$|] = "%s"

  it 'should work with the index action' do
    r = get('/index.php')
    [ r.status, r.body ].should == [ 200, 'hello' ]

    r = get('/index.php/')
    [ r.status, r.body ].should == [ 200, 'hello' ]
  end

  it 'should work with other actions' do
    r = get('/index.php/test')
    [ r.status, r.body ].should == [ 200, 'test' ]
  end

  it 'should rewrite urls for files' do
    r = get('/index.php/file.css')
    [ r.status, r.body ].should == [ 200, 'this is css' ]
  end
end
