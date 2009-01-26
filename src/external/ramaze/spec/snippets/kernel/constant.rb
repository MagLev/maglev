require 'lib/ramaze/spec/helper/snippets'

describe 'constant' do
  it 'should load from string' do
    constant('Fixnum').should == Fixnum
  end

  it 'should load from symbol' do
    constant(:Fixnum).should == Fixnum
  end

  it 'should handle hierarchy' do
    constant('Math::PI').should == Math::PI
  end

  it 'should be callable with explicit self' do
    Math.constant('PI').should == Math::PI
  end

  it 'should be callable with explicit self' do
    Math.constant('::Math').should == Math
  end
end
