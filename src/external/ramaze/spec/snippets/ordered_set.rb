require 'lib/ramaze/spec/helper/snippets'

describe 'OrderedSet' do
  os = OrderedSet.new(1,2,3,1)

  it 'should create sets' do
    OrderedSet.new.should == []
    OrderedSet.new(1).should == [1]
    OrderedSet.new(1,2).should == [1,2]
    OrderedSet.new([1,2,3]).should == [1,2,3]
  end

  it 'should compare sets' do
    os.should == OrderedSet.new(1,2,3,1)
    os.should == OrderedSet.new([1,2,3,1])
  end

  it 'should not contain duplicates' do
    os.should == [1,2,3]
  end

  it 'should not duplicate entries' do
    os << 4
    os.should == [1,2,3,4]

    os << 4
    os.should == [1,2,3,4]
  end

  it 'should append with push and prepend with unshift' do
    os.push 1
    os.should == [2,3,4,1]

    os.unshift 1
    os.should == [1,2,3,4]

    os.push [1,2]
    os.should == [1,2,3,4,[1,2]]

    os.unshift [1,2]
    os.should == [[1,2],1,2,3,4]
  end

  it 'should support Array#[]=' do
    os = OrderedSet.new(1)
    os.should == [1]

    os[0] = 1
    os.should == [1]

    os[1,0] = [3,4,5,1]
    os.should == [3,4,5,1]

    os[0,0] = [1,2]
    os.should == [1,2,3,4,5]

    os[5..5] = [7,8,1,2]
    os.should == [3,4,5,7,8,1,2]

    os[1..2] = 3
    os.should == [3,7,8,1,2]
  end
end
