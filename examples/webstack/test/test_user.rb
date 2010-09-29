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

  describe 'Reading Tweets' do
    before do
      @user3 = User.new 'User3'
      @user1.follow @user2
      @user1.follow @user3
    end

    it 'should remember its own tweets' do
      10.times do |i|
        @user2.tweet "#{@user2.name} tweet #{i}"
        @user3.tweet "#{@user3.name} tweet #{i}"
      end
      assert_equal 10, @user2.tweets.size
      assert_equal 10, @user3.tweets.size
    end
    
    it 'should start with an empty timeline' do
      assert_empty @user1.timeline
    end

    it 'should keep a timeline of last 20 tweets from friends' do
      10.times do |i|
        @user2.tweet "#{@user2.name} tweet #{i}"
        @user3.tweet "#{@user3.name} tweet #{i}"
      end
      assert_equal 20, @user1.timeline.size
    end

    it 'should limit timeline to 20 tweets' do
      20.times do |i|
        @user2.tweet "#{@user2.name} tweet #{i}"
        @user3.tweet "#{@user3.name} tweet #{i}"
      end
      assert_equal 20, @user1.timeline.size
    end
  end
end


describe 'User Tagging Support' do
  it 'should have a unique description'
end
