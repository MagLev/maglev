class User

  attr_reader :followers, :following, :name, :timeline, :tweets
  
  def initialize(name)
    @name = name
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
end
