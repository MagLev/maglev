require 'spec/helper'
require 'examples/basic/linking'

describe 'Linking' do
  behaves_like 'http'
  ramaze

  it 'should provide a link to help' do
    r = get('/').body
    r.should.include('<a href="/help">Help?</a>')
  end

  it 'should provide a link to another controller' do
    r = get('/help').body
    r.should.include('<a href="/link_to/another">A Different Controller</a>')
  end

end
