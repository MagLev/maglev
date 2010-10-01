require "magtag/user"
require "minitest/spec"

MiniTest::Unit.autorun

describe User do
  before do
    @user1 = User.new 'User1', 'pw1'
    @user2 = User.new 'User2', 'pw2'
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
      @user3 = User.new 'User3', 'pw3'
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


describe 'User Persistence' do
  describe 'User.signup' do
    it 'succeeds with new username and matching passwords' do
      refute_nil User.signup('new_user_1', 'pw', 'pw')
      refute_nil User.signup('new_user_2', 'pw')
    end

    it 'fails if password and confirmpassword are not same' do
      assert_raises(User::UserException) { User.signup('fred', 'pw', 'pw1') }
    end

    it 'fails on empty or nil password' do
      assert_raises(User::UserException) { User.signup('fred', '') }
      assert_raises(User::UserException) { User.signup('fred', nil) }
    end

    it 'fails on empty or nil username' do
      assert_raises(User::UserException) { User.signup('', 'pw') }
      assert_raises(User::UserException) { User.signup(nil, 'pw') }
    end
  end

  describe '#login' do
    it 'accepts a good password' do
      name = 'fred'
      pw   = 'password'
      user = User.new name, pw
      assert user.login pw
    end
  end

  describe 'User.find_by_name' do
    before do
      @user1 = User.new 'User1', 'pw1'
    end

    it 'returns nil if user not found' do
      assert_nil User.find_by_name('NOT A USER NAME')
    end

    it 'finds saved users' do
      assert_nil User.find_by_name(@user1.name)
      @user1.save
      assert_same @user1, User.find_by_name(@user1.name)
    end
  end

  describe 'Instance level persistence methods' do
    it '#save saves a user' do
      name = 'test_save_user'
      ensure_no_user_named name

      user = User.new name, 'pw'
      user.save
      refute_nil User.find_by_name name
    end

    it '#delete deletes a user' do
      name = 'test_delete_user'
      user = ensure_user_exists name
      user.delete
      assert_nil User.find_by_name name
    end
  end
end


def ensure_user_exists(name)
  user = User.find_by_name(name)
  unless user
    user = User.new name, name
    user.save
  end
  user
end

def ensure_no_user_named(name)
  user = User.find_by_name(name)
  user.delete if user
end
