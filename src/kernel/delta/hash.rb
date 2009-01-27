class Hash
  include Enumerable

  # Override some of the methods in Enumerable with better implementations
  primitive 'include?', 'includesKey:'
  primitive 'member?', 'includesKey:'

  def sort(&block)
    to_a.sort(&block)
  end
  
end
