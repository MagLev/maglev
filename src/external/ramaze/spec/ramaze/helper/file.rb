require 'spec/helper'

class TCFileHelper < Ramaze::Controller
  map '/'

  def index
    send_file(__FILE__)
  end
end

describe 'FileHelper' do
  behaves_like 'http'
  ramaze

  it 'serving a file' do
    get('/').body.strip.should == File.read(__FILE__).strip
  end
end
