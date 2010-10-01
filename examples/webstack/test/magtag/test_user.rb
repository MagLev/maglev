require "minitest/spec"

require "magtag/user"
require "magtag/tweet"

require 'testhelper'

MiniTest::Unit.autorun

describe User do
  before do
    @user1 = User.new 'User1', 'pw1'
    @user2 = User.new 'User2', 'pw2'
  end

  describe 'Accessors' do
    it 'should have a name' do
      assert_equal @user1.name, 'User1'
    end

    it 'should have no followers, follow no-one and have no tweets' do
      assert_equal 0, @user1.num_followers
      assert_equal 0, @user1.num_following
      assert_equal 0, @user1.num_tweets
    end
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

  describe '#tweet' do
    it 'returns a new Tweet' do
      t = @user1.tweet "A Tweet"
      refute_nil t
      assert_kind_of Tweet, t
    end

    it 'should reject long tweets' do
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
      refute_nil @user1.timeline.detect { |t| t.text == "#{@user3.name} tweet 19" }
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
      name, pw = 'fred', 'password'
      TestHelper.ensure_no_user_named(name)

      user = User.new name, pw
      assert user.login pw
    end
  end

  describe 'User.find_by_name' do
    before do
      TestHelper.ensure_no_user_named('User1')
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
      TestHelper.ensure_no_user_named name

      user = User.new name, 'pw'
      user.save
      refute_nil User.find_by_name name
    end

    it '#delete deletes a user' do
      name = 'test_delete_user'
      user = TestHelper.ensure_user_exists name
      user.delete
      assert_nil User.find_by_name name
    end
  end
end
