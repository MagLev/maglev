require 'spec/helper'

class MainController < Ramaze::Controller
  def float(flt)
    "Float: #{flt}"
  end

  def string(str)
    "String: #{str}"
  end

  def price(p)
    "Price: \$#{p}"
  end

  def sum(a, b)
    a.to_i + b.to_i
  end

  def bar
    'this is bar'
  end
end

describe 'Route' do
  behaves_like 'http'
  ramaze
  @route = Ramaze::Route

  it 'should take custom lambda routers' do
    @route['string'] = lambda {|path, req| path if path =~ %r!^/string! }
    @route['string'].class.should == Proc

    @route['calc sum'] = lambda do |path, req|
      if req[:do_calc]
        lval, rval = req[:a, :b]
        rval = rval.to_i * 10
        "/sum/#{lval}/#{rval}"
      end
    end

    Ramaze::Route('foo') do |path, req|
      '/bar' if req[:bar]
    end
  end

  it 'should be possible to define simple string routes' do
    @route['/foobar'] = '/bar'
    @route['/foobar'].should == '/bar'
  end

  it 'should be possible to define routes' do
    @route[%r!^/(\d+\.\d{2})$!] = "/price/%.2f"
    @route[%r!^/(\d+\.\d{2})$!].should == "/price/%.2f"

    @route[%r!^/(\d+\.\d+)!] = "/float/%.3f"
    @route[%r!^/(\d+\.\d+)!].should == "/float/%.3f"

    @route[%r!^/(\w+)!] = "/string/%s"
    @route[%r!^/(\w+)!].should == "/string/%s"
  end

  it 'should be used - /float' do
    r = get('/123.123')
    r.status.should == 200
    r.body.should == 'Float: 123.123'
  end

  it 'should be used - /string' do
    r = get('/foo')
    r.status.should == 200
    r.body.should == 'String: foo'
  end

  it 'should use %.3f' do
    r = get('/123.123456')
    r.status.should == 200
    r.body.should == 'Float: 123.123'
  end

  it 'should resolve in the order added' do
    r = get('/12.84')
    r.status.should == 200
    r.body.should == 'Price: $12.84'
  end

  it 'should use lambda routers' do
    r = get('/string/abc')
    r.status.should == 200
    r.body.should == 'String: abc'

    r = get('/', 'do_calc=1&a=2&b=6')
    r.status.should == 200
    r.body.should == '62'
  end

  it 'should support Route() with blocks' do
    r = get('/foo', 'bar=1')
    r.status.should == 200
    r.body.should == 'this is bar'
  end

  it 'should support string route translations' do
    r = get('/foobar')
    r.status.should == 200
    r.body.should == 'this is bar'
  end

  it 'should clear routes' do
    Ramaze::Route.trait[:routes].size.should > 0
    Ramaze::Route.clear
    Ramaze::Route.trait[:routes].size.should == 0
  end

  it 'should exclude existing actions' do
    Ramaze::Route[ %r!^/(.+)$! ] = "/string/%s"
    r = get('/hello')
    r.status.should == 200
    r.body.should == 'String: hello'

    r = get('/bar')
    r.status.should == 200
    r.body.should == 'this is bar'
  end

  it 'should not recurse given a bad route' do
    Ramaze::Route[ %r!^/good/(.+)$! ] = "/bad/%s"
    r = get('/good/hi')
    r.status.should == 500
  end
end
