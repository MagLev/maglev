require 'spec/helper'
spec_require 'haml'
require 'examples/misc/css'

describe 'CSSController' do
  behaves_like 'http'
  ramaze

  def req(path) r = get(path); [r.content_type, r.body] end

  it 'should cache generated css' do
    lambda{ req('/css/style.css') }.
      should.not.change{ req('/css/style.css') }
  end
end
