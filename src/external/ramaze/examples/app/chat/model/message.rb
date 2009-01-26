class Message < Struct.new(:nick, :text, :time)
  include Comparable

  def <=>(other)
    time <=> other.time
  end
end
