#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TCRequestController < Ramaze::Controller
  map '/'
  engine :None

  def is_post()   request.post?.to_s end
  def is_get()    request.get?.to_s end
  def is_put()    request.put?.to_s end
  def is_delete() request.delete?.to_s end

  def request_inspect
    request.params.inspect
  end

  def post_inspect
    request.params.inspect
  end

  def put_inspect(file)
    # referencing request.rack_params breaks this test
    # request.params is hacked to return {} on PUT requests
    request.params
    request.body.read
  end

  def get_inspect
    request.params.inspect
  end

  def test_get
    request['foo']
  end

  def test_post
    request.params.inspect
  end

  def test_headers
  end

  def my_ip
    request.remote_addr
  end

  def to_ivs
    request.to_ivs :foo, :bar
    instance_variables.sort.map{|iv|
      [ iv.to_s, instance_variable_get(iv) ].join(' => ')
    }.join(', ')
  end
end

options = ramaze_options rescue {}

describe "Request" do
  behaves_like 'http'
  ramaze options.merge(:public_root => 'spec/ramaze/public')

  Request = Ramaze::Request

  it 'to_ivs' do
    got = get('/to_ivs', 'foo' => 'a')
    got.body.should == '@foo => a'
    got = get('/to_ivs', 'bar' => 'b', 'foo' => 'c')
    got.body.should == '@bar => b, @foo => c'
  end

  should 'show locales' do
    r = Request.new('HTTP_ACCEPT_LANGUAGE' => 'en-ca,en-us;q=1,en;q=0.6')
    r.locales.should == %w[en-ca en-us en]
  end

  describe "POST" do
    behaves_like 'http'

    it "give me the result of request.post?" do
      post("/is_post").body.should == 'true'
    end

    it "give me the result of request.get?" do
      post("/is_get").body.should == 'false'
    end

    # this here has shown some odd errors... keep an eye on it.
    it "give me back what i gave" do
      post("/post_inspect", 'this' => 'post').body.should == {"this" => "post"}.inspect
    end

    should "handle key[nested_key]" do
      get('/test_get', 'foo' => 'bar').body.should == 'bar'
      params = {'foo' => 'null', 'bar[1]' => 'eins', 'bar[7]' => 'sieben'}

      eval(post('/test_post', params).body).should ==
        {'foo' => 'null', 'bar' => {'1' => 'eins', '7' => 'sieben'}}
    end

    should 'handle key[nested_key][nested_nested_key]' do
      eval(post('/test_post', 'foo[1][2]' => 'eins zwei').body).should ==
        {'foo' => {'1' => {'2' => 'eins zwei'}}}
    end

    should 'handle key&key[nested]' do
      eval(post('/test_post', 'a' => 'b', 'a[c]' => 'd').body).should ==
        {'a' => {'c' => 'd'}}
    end
  end

  describe "PUT" do
    behaves_like 'http'

    it "put a resource" do
      image = 'favicon.ico'
      image_path = File.join('spec', 'ramaze', 'public', image)
      address = "/put_inspect/#{image}"

      if RUBY_VERSION >= '1.9.0'
        file = File.open(image_path, 'r:ASCII'){|f| f.read}
      else
        file = File.read(image_path)
      end

      response = put(address, :input => file)
      response.body.dump.should == file.dump
    end
  end

  describe "DELETE" do
    behaves_like 'http'

    it "delete a resource" do
      delete('/is_delete').body.should == 'true'
    end
  end

  describe "GET" do
    behaves_like 'http'

    it "give me the result of request.post?" do
      get("/is_post").body.should == 'false'
    end

    it "give me the result of request.get?" do
      get("/is_get").body.should == 'true'
    end

    it "give me back what i gave" do
      params = {'one' => 'two', 'three' => 'four'}
      get("/get_inspect", params).body.should == params.inspect
    end

    it "my ip" do
      get("/my_ip").body.should == '127.0.0.1'
    end
  end

  describe "get files" do
    behaves_like 'http'

    it "binary" do
      image_path = '/favicon.ico'
      if RUBY_VERSION >= '1.9.0'
        static_image = File.open("spec/ramaze/public#{image_path}", 'r:ASCII'){|f| f.read}
      else
        static_image = File.read("spec/ramaze/public#{image_path}")
      end

      response = get(image_path)
      response.status.should == 200
      response.body.should == static_image
    end

    it 'plain test' do
      css_path = '/test_download.css'
      static_css = File.read("spec/ramaze/public#{css_path}").strip

      response = get(css_path)
      response.status.should == 200
      response.body.strip.should == static_css
    end
  end
end
