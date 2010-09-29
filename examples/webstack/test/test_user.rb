require "minitest/spec"
require "magtag/user"

MiniTest::Unit.autorun

describe User do
  before do
    @user1 = User.new 'User1'
    @user2 =User.new 'User2'
  end

  it 'should have a name' do
    assert_equal @user1.name, 'User1'
  end

  describe 'should maintain a list of followers and following' do
    it 'should initialize followers and following to empty' do
      assert_empty @user1.followers
      assert_empty @user1.following
    end

    it 'should allow users to follow each other' do
      @user1.follow @user2
      assert_includes @user1.following, @user2
      assert_includes @user2.followers, @user1
    end
  end

  describe 'Tweeting' do
    it 'should allow tweets' do
      @user1.tweet "A Tweet"
    end
    
    it 'should reject long tweets' do
      long_tweet = 
      assert_raises(ArgumentError) { @user1.tweet '*' * 141 }
    end
  end
end


describe 'User Tagging Support' do
  it 'should have a unique description'
end

