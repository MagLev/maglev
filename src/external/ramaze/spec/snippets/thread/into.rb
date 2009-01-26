require 'spec/helper'

class TCThreadIntoController < Ramaze::Controller
  map :/

  def hello
    Thread.into('goodbye') do |str|
      "#{Ramaze::Action.current.name}, #{str}"
    end.value
  end
end

describe 'Thread.into' do
  behaves_like 'http'
  ramaze

  it 'should provide access to thread vars' do
    get('/hello').body.should == 'hello, goodbye'
  end
end
