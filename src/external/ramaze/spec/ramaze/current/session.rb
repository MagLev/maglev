#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TrinitySessionController < Ramaze::Controller
  map :/

  def index
    "nothing"
  end

  def val
    session.client[:val]
  end

  def set
    session.client[:val] = 789
  end

  def del
    session.client.delete(:val)
  end
end

describe "Session" do
  behaves_like 'http'
  ramaze :sessions => false

  it 'should work without sessions' do
    class Ramaze::Session
      remove_const :IP_COUNT_LIMIT
      const_set(:IP_COUNT_LIMIT, 2)
    end
    (Ramaze::Session::IP_COUNT_LIMIT + 2).times do
      r = get('/')
      r.body.should == "nothing"
      r.headers['Content-Type'].should == 'text/html'
    end
  end
end

describe "Session cookie store" do
  behaves_like 'http'
  ramaze :sessions => true

  Ramaze::Session::SESSION_KEY.replace('sess')
  @secret = Ramaze::Session.trait[:secret] = 'abc'

  # Marshal a session hash into safe cookie data. Include an integrity hash.
  def marshal(session)
    data = [ Marshal.dump(session) ].pack('m').chop
    "#{data}--#{generate_digest(data)}"
  end

  # Unmarshal cookie data to a hash and verify its integrity.
  def unmarshal(cookie)
    if cookie
      data, digest = cookie.split('--')
      return {} unless digest == generate_digest(data)
      Marshal.load(data.unpack('m').first)
    end
  end

  # Generate the inline SHA512 message digest. Larger (128 bytes) than SHA256
  # (64 bytes) or RMD160 (40 bytes), but small relative to the 4096 byte
  # max cookie size.
  def generate_digest(data)
    Digest::SHA512.hexdigest "#{data}#{@secret}"
  end

  it 'should use session data from cookies' do
    r = get('/val', :cookie => "sess-client="+marshal({ :val => 123 }))
    r.body.should == '123'
  end

  it 'should use new values from session cookie' do
    r = get('/val', :cookie => "sess-client="+marshal({ :val => 456 }))
    r.body.should == '456'
  end

  it 'should set session values in the cookie' do
    r = get('/set')
    r.headers['Set-Cookie'].should == "sess-client=#{CGI.escape(marshal({:val => 789}))}; path=/"
  end

  it 'bad cookie should be ignored' do
    r = get('/val', :cookie => marshal({ :val => 456 }) + 'wrong')
    r.body.should.not == '456'
  end

  it 'should allow removing the last key/value pair' do
    r = get('/del', :cookie => "sess-client="+marshal({ :val => 456 }))
    r.body.should == '456'
    r.headers['Set-Cookie'].should == "sess-client=#{CGI.escape(marshal({}))}; path=/"
  end
end
