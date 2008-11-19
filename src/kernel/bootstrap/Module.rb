
class Module

  # NOTE: Most of the implementation of Module is in delta/Module.rb due to
  # a bootstrapping issue: Most methods are stubbed, and as an aid to
  # debugging, they print out a warning so we can figure out which methods
  # really need work.  BUT, Kernel.puts requires File, which hasn't been
  # loaded yet, so we have to wait until after bootstrap/File.rb is loaded
  # before you can do a puts.  Since all of the stubs in module have a
  # puts, they are temporarily there.  As they are implemented, we should
  # pull them into here.

  def const_set(symbol, obj)
    nil  # TODO: MNI: stubbed
  end

  # Invoked as a callback when a method is added to the reciever
  def method_added(symbol)
  end

  # Invoked as a callback when a method is removed from the reciever
  def method_removed(symbol)
  end

  # Invoked as a callback when a method is undefined in the reciever
  def method_undefined(symbol)
  end
end
