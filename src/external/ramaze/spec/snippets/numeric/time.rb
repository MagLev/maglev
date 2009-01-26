require 'lib/ramaze/spec/helper/snippets'

describe "Numeric time extensions" do
  it 'should provide times in the past' do
    3.days.ago.to_i.should == (Time.now - 3*24*60*60).to_i
    2.months.ago.to_i.should == (Time.now - 2*30*24*60*60).to_i
  end

  it 'should provide times in the future' do
    2.weeks.from_now.to_i.should == (Time.now + 2*7*24*60*60).to_i
  end
end