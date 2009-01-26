require 'spec/helper'

# Tests the file_cache facility that saves rendered content to files in
# /public and hence serves up the static files for subsequent requests.
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

  def index
    rand
  end
  cache :index
end

class TCIndexCache < Ramaze::Controller
  map '/withparams'
  helper :cache

  def index(somearg = nil)
    rand + somearg.to_f
  end
  alias :nonindex :index

  cache :index, :nonindex
end

describe 'Action rendering' do
  behaves_like 'http'

  FileUtils.mkdir_p(public_root = __DIR__(:public))

  ramaze :file_cache => true, :public_root => public_root

  def req(path)
    r = get(path)
    [ r.content_type, r.body ]
  end

  should 'cache to file' do
    lambda{ req('/') }.should.not.change{ req('/') }
    File.file?(public_root/'index').should == true
  end

  should 'create subdirs as needed' do
    lambda{ req('/other') }.should.not.change{ req('/other') }
    File.file?(public_root/'other/index').should == true
  end

  should 'cache action with params' do
    lambda{ req('/withparams/nonindex/42') }.should.not.change{ req('/withparams/nonindex/42') }
    File.file?(public_root/'withparams/nonindex/42').should == true
  end

  should 'cache index action with params' do
    lambda{ req('/withparams/42') }.should.not.change{ req('/withparams/42') }
    File.file?(public_root/'withparams/42').should == true
  end

  FileUtils.rm_rf public_root
end
