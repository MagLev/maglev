class ThreadGroup
 
  # Constant ThreadGroup::Default not implementable, 
  #   cannot be persistent in the name space
  #   use  self.default instead
  # TODO, need Ruby API to create a transient constant

  # def self.default; end #  returns the equivalent of  ThreadGroup::Default
  class_primitive_nobridge 'default', 'default'

  class_primitive_nobridge '__new', 'new'

  def self.new(*args)
    inst = self.__new
    inst.initialize(*args)
  end

  def add(thread)
    if @closed
      raise ThreadError,'cannot alter enclosed thread group'
    else
      thread.__join_group(self)
    end
  end

  def enclosed?
    @closed
  end

  def enclose
    @closed = true
  end

  # TODO,  freeze does not yet prevent starting threads
  alias_method( :freeze , :enclose )

  primitive_nobridge 'list', 'list'

end
