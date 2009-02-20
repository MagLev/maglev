# the initialzie function does not get called in MagLev (but does in MRI).
# So, @names is nil during the [] call

# Case 1:
class HeaderHash < Hash
  def initialize(hash={ })
    puts "In HeaderHash#initialize"
    @names = { }
  end

  def [](k)
    idx = @names[k.downcase]  # @names is nil....
    super idx
  end
end

hh = HeaderHash.new   # HeaderHash#initialize is not called, so...
hh['Content-Length']  # @names is nil at this point, and [] not found

# Case 2: Subclass defines initialize with more parameters than superclass
class HeaderHash2 < Hash
  def initialize(a, b, c)
    puts "In HeaderHash2#initialize"
    super()
    @names = { }
  end

  def [](k)
    idx = @names[k.downcase]  # @names is nil....
    super idx
  end
end

# Problem 2a: MagLev should throw an ArgumentError but doesn't
# MRI throws:
#   pbm.rb:35:in `initialize': wrong number of arguments (0 for 3) (ArgumentError)
#       from pbm.rb:35:in `new'
#       from pbm.rb:35
hh2 = HeaderHash2.new

# Problem 2b: MagLev throws, but shouldn't:
#  $ maglev-ruby pbm.rb
#  topaz 1> error , too many arguments,
#            during /Users/pmclain/projects/maglev/git/pbm.rb
#  ERROR 2023, Error, 'too many arguments'
#  topaz 1>
#
hh2 = HeaderHash2.new(:a, :b, :c)

