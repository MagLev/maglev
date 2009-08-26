OrderedCollection = _resolve_smalltalk_global(:OrderedCollection)
class OrderedCollection
  include Enumerable
  primitive 'each&', 'do:'

  # Like each, but starts from the end of the collection.
  primitive 'reverse_each&', 'reverseDo:'
  primitive 'include?', 'includes:'
  primitive 'to_a', 'asArray'
  primitive '<<', 'add:'
  primitive 'length', 'size'
  primitive '_at', 'at:'
  def [](index)
    _at(index + 1)
  end

  def reverse_each_with_index
    idx = length - 1
    reverse_each { |o| yield(o, idx); idx -= 1 }
    self
  end

end
