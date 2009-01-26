require 'spec/helper'

describe 'Ramaze::Request' do
  def request(env = {})
    Ramaze::Request.new(env)
  end

  should 'provide #request_uri' do
    request('REQUEST_URI' => '/?a=b').request_uri.should == '/?a=b'
    request(  'PATH_INFO' => '/'    ).request_uri.should == '/'
  end

  should 'provide #local_net?' do
    request.local_net?('192.168.0.1').to_s.should == '192.168.0.0'
    request.local_net?('252.168.0.1').should == nil
    request.local_net?('unknown').should == nil
    request('REMOTE_ADDR' => '211.3.129.47, 66.249.85.131').local_net?.should == nil
    request('REMOTE_ADDR' => '211.3.129.47').local_net?.should == nil
  end

  should 'provide #subset' do
    params = {'a' => 'b', 'c' => 'd', 'e' => 'f'}
    env = { 'rack.request.form_hash' => params }
    req = request(env)

    req.params.should == params
    req.subset(:a).should == {'a' => 'b'}
    req.subset(:a, :c).should == {'a' => 'b', 'c' => 'd'}
  end
end
