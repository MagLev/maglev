# Simple user class
# Responsibilities:
# * The class manages a repository of saved users.
# * Users manage their followers and tweets
class User

  class UserException < Exception; end

  if defined? Maglev
    unless defined? ALL_USERS
      ALL_USERS = IdentitySet.new
      ALL_USERS.create_equality_index('@name', String)
    end

    def self.find_by_name(name)
      # TODO: There has got to be a better way...
      result = ALL_USERS.search([:@name], :eql, name)
      result.size > 0 ? result.__at(1) : nil
    end
  else
    ALL_USERS = []
    def self.find_by_name(name)
      ALL_USERS.detect { |u| u.name == name }
    end
  end


  # Creates a new user with the given name and password if it does not
  # already exist in the database.
  #
  # @param [String] username the name of the user (must be unique).
  # @param [String] pw the password for the user.
  # @param [String, nil] confirm_pw the confirm password entered by user.
  # @return [User] newly created user.  The user is <em>not</em> saved.
  #
  # @raise [UserException] if +name+ or +pw+ is either empty or nil.
  # @raise [UserException] if a user named +name+ already exists in <tt>ALL_USERS</tt>.
  # @raise [UserException] if <tt>confirm_pw</tt> is non-nil, and does not match +pw+.
  #
  def self.signup(username, pw, confirm_pw=nil)
    raise UserException, "Passwords don't match" unless confirm_pw.nil? || pw == confirm_pw
    raise UserException, "User #{username} already taken" unless find_by_name(username).nil?

    new username, pw
  end

  attr_reader :followers, :following, :name, :timeline, :tweets

  # Initializes a new User with the given name and password.
  #
  # @param [String] username the name of the user.
  # @param [String] password the password for the user.
  # @return [User] the receiver
  #
  # @raise [UserException] if either +name+ or +pw+ is empty or nil.
  #
  def initialize(name, password)
    raise UserException, "Bad username '#{name}'" if name.nil? || name.empty?
    raise UserException, "Bad password" if password.nil? or password.empty?
    @name      = name
    @password  = password
    @followers = []
    @following = []
    @timeline  = []
    @tweets    = []
  end

  def num_followers
    @followers.size
  end

  def num_following
    @following.size
  end

  def num_tweets
    @tweets.size
  end

  def follow(other)
    @following << other
    other.add_follower self
  end

  def add_follower(other)
    @followers << other
  end

  def add_timeline(event)
    @timeline << event
    @timeline = @timeline[1..-1] if @timeline.size > 20
  end

  # Creates a tweet and updates followers about the tweet
  #
  # @param [String] text the tweet text
  # @return [Tweet] the new tweet.
  #
  # @raises [ArgumentError] if tweet text is too long.
  def tweet(text)
    new_tweet = Tweet.new text
    @tweets << new_tweet
    @followers.each { |f| f.add_timeline new_tweet }
    new_tweet
  end

  def login(password)
    password == @password
  end

  def save
    ALL_USERS << self
    self
  end

  def delete
    ALL_USERS.delete self
    self
  end
end
