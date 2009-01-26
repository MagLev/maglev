require 'lib/ramaze/spec/helper/snippets'

describe 'Array#put_within' do
  it 'should put a given object at a well-described position' do
    array = [:foo, :bar, :baz]
    array.put_within(:foobar, :after => :bar, :before => :baz)
    array.should == [:foo, :bar, :foobar, :baz]
  end

  it 'should raise on uncertainity' do
    array = [:foo, :bar, :baz]
    lambda{
      array.put_within(:foobar, :after => :foo, :before => :baz)
    }.should.raise(ArgumentError).
      message.should == "Too many elements within constrain"
  end
end

describe 'Array#put_after' do
  it 'should put a given object at a well-described position' do
    array = [:foo, :bar, :baz]
    array.put_after(:bar, :foobar)
    array.should == [:foo, :bar, :foobar, :baz]
  end
end

describe 'Array#put_within' do
  it 'should put a given object at a well-described position' do
    array = [:foo, :bar, :baz]
    array.put_before(:bar, :foobar)
    array.should == [:foo, :foobar, :bar, :baz]
  end
end
