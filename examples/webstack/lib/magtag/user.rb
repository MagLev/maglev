class User

  class UserException < Exception; end

  ALL_USERS = []

  def self.find_by_name(name)
    ALL_USERS.detect { |u| u.name == name }
  end

  def self.signup(username, pw, confirm_pw=nil)
    raise UserException, "Bad username '#{username}'" if username.nil? || username.empty?
    raise UserException, "Passwords don't match" unless confirm_pw.nil? || pw == confirm_pw
    raise UserException, "Bad password" if pw.nil? or pw.empty?
    raise UserException, "User #{username} already taken" unless find_by_name(username).nil?

    new username, pw
  end

  attr_reader :followers, :following, :name, :timeline, :tweets

  def initialize(name, password)
    @name      = name
    @password  = password
    @followers = []
    @following = []
    @timeline  = []
    @tweets    = []
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

  def tweet(msg)
    raise ArgumentError, "Tweet Too Long: #{msg}" if msg.length > 140
    @tweets << msg
    @followers.each { |f| f.add_timeline msg }
  end

  def login(password)
    password == @password
  end

  def save
    ALL_USERS << self
  end

  def delete
    ALL_USERS.delete self
  end
end
