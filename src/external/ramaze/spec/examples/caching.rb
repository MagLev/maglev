require 'spec/helper'
require 'examples/helpers/cache'

describe 'Caching' do
  behaves_like 'http'
  ramaze

  it '/' do
    3.times do
      lambda{ get('/') }.
        should.not.change{ get('/').body }
    end

    3.times do
      lambda{ get('/invalidate') }.
        should.change{ get('/').body }
    end
  end
end
