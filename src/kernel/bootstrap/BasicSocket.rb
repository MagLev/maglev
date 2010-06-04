
class BasicSocket  # identically Smalltalk GsSocket

  # instance creation not allowed
  def self.new(*args, &block)
    raise StandardError , 'cannot create instances of BasicSocket'
  end

  def self.allocate
    raise NotImplementedError, 'BasicSocket#allocate not supported'
  end

  def self.do_not_reverse_lookup
    false  # not sure if this is actually the default state
  end

  def self.do_not_reverse_lookup=(boolean)
    # has no effect ,  TODO - primitive for do_not_reverse_lookup=
    false
  end  

  # def self.for_fd(fd); end # not implemented yet

  # instance methods either inherited, implemented in subclasses
  # or not implemented yet

end
