require 'spec/helper'

class TCRecordController < Ramaze::Controller
  map '/'

  def index
    'The index'
  end

  def foo
    'The foo'
  end
end

describe 'Adapter recording' do
  behaves_like 'http'

  ramaze :adapter => :webrick, :record => lambda{|request|
    request.remote_addr == '127.0.0.1'
  }

  @record = Ramaze::Record

  it 'should record' do
    get('/').body.should == 'The index'
    get('/foo').body.should == 'The foo'
    @record.size.should == 2
    @record.first.path_info.should == '/'
    @record.last.path_info.should == '/foo'
  end
end
