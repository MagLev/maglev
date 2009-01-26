require 'spec/helper'

class TCActionCache < Ramaze::Controller
  map '/'
  helper :cache

  def index
    rand
  end
  cache :index
end

class TCOtherCache < Ramaze::Controller
  map '/other'
  helper :cache

  def foo
    rand
  end
  cache :foo

  def query
    @foo = request['foo']
    '#{rand} #{@foo}'
  end
  cache :query, :key => lambda{ request['foo'] }

  def set_flash
    flash[:message] = "This is a random flash message: #{rand}"
  end

  def use_flash
    '#{rand} - Flash message: #{flash[:message]}'
  end
  cache :use_flash
end

describe 'Action rendering' do
  behaves_like 'http', 'browser'
  ramaze :adapter => :webrick

  def req(path)
    r = get(path)
    [ r.content_type, r.body ]
  end

  should 'cache /' do
    lambda{ req('/') }.should.not.change{ req('/') }
  end

  should 'cache /other/foo' do
    lambda{ req('/other/foo') }.should.not.change{ req('/other/foo') }
  end

  should 'respect custom cache key' do
    get('/other/query', {:foo => 1}).body.should == get('/other/query', {:foo => 1}).body
    get('/other/query', {:foo => 1}).body.should.not == get('/other/query', {:foo => 2}).body
  end

  should 'not cache when flash data exists and no_cache_flash is on' do
    Ramaze::Global.no_cache_flash = true
    b = Browser.new

    # If there's no flash data, caching should operate.
    b.get('/other/use_flash').should == b.get('/other/use_flash')

    # When flash data exists, should ignore cache and generate fresh response.
    prev = b.get('/other/use_flash')
    b.get('/other/set_flash')
    b.get('/other/use_flash').should.not == prev
    # On next request, flash data should be gone, so get cached response.
    b.get('/other/use_flash').should == prev
  end

  should 'always cache if no_cache_flash is off' do
    Ramaze::Global.no_cache_flash = false
    b = Browser.new

    b.get('/other/use_flash').should == b.get('/other/use_flash')

    # When flash data exists, should still cache.
    prev = b.get('/other/use_flash')
    b.get('/other/set_flash')
    b.get('/other/use_flash').should == prev
  end

end
