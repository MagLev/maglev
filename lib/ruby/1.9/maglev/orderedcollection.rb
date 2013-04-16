OrderedCollection = __resolve_smalltalk_global(:OrderedCollection)
class OrderedCollection
  include Enumerable
  primitive 'each&', 'do:'

  # Like each, but starts from the end of the collection.
  primitive 'reverse_each&', 'reverseDo:'
  primitive 'include?', 'includes:'
  primitive 'to_a', 'asArray'
  primitive '<<', 'add:'
  primitive '_at', 'at:'
  primitive 'length', 'size'
  primitive 'size', 'size'
  primitive 'size=', 'size:'
  primitive 'delete', 'remove:'
  primitive 'last', 'last'

  def [](index)
    _at(index + 1)
  end

  def reverse_each_with_index
    idx = length - 1
    reverse_each { |o| yield(o, idx); idx -= 1 }
    self
  end

  # Clear all of the elements in receiver.  (set the size to 0)
  def clear
    self.size = 0
  end
end
