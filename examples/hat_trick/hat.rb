# This class defines the Magician's Hat.  It is just a container for
# Rabbits, doves and other things Magicians don't keep up their sleeve.
class Hat
  def initialize
    @contents = []
  end

  def put(item)
    @contents << item
    nil
  end

  def contents
    @contents
  end

  def size
    @contents.size
  end
end
