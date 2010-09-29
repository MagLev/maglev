class User

  attr_reader :followers, :following, :name
  
  def initialize(name)
    @name = name
    @followers = []
    @following = []
  end

  def follow(other)
    @following << other
    other.add_follower self
  end

  def add_follower(other)
    @followers << other
  end

  def tweet(msg)
    raise ArgumentError, "Tweet Too Long: #{msg}" if msg.length > 140
  end
end
