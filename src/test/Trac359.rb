# the initialize function does not get called in MagLev (but does in MRI).
# So, @names is nil during the [] call

# Case 1:
class HeaderHash < Hash
  def initialize(hash= {  }  )
    puts "In HeaderHash#initialize"
    @names = hash
  end

  def [](k)
    if @names.equal?(nil) ; raise 'error'; end
    idx = @names[k.downcase]  
    super idx
  end

  def []=(k, v)
    @names[k.downcase] = k
    super(k,v)
  end

  def self.testh(h)
    h['ABC'] = 55
    hv = h['abc'] 
    unless hv.equal?(55) ; raise 'error'; end
    hv = h['AbC']
    unless hv.equal?(55) ; raise 'error'; end
    hv = h.fetch('AbC', 99) ;
    unless hv.equal?(99) ; raise 'error'; end
    hv = h.fetch('ABC') ;
    unless hv.equal?(55) ; raise 'error'; end
  end

end

hh = HeaderHash.new   # HeaderHash#initialize is not called, so...
HeaderHash.testh(hh)


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

  def []=(k, v)
    @names[k.downcase] = k
    super(k,v)
  end 
end


# Problem 2a: MagLev should throw an ArgumentError but doesn't
# MRI throws:
#   pbm.rb:35:in `initialize': wrong number of arguments (0 for 3) (ArgumentError)
#       from pbm.rb:35:in `new'
#       from pbm.rb:35
begin
  hh2 = HeaderHash2.new
  raise Exception, 'should not be here' 
rescue ArgumentError
  "ok"
end

# Problem 2b: MagLev throws, but shouldn't:
#  $ maglev-ruby pbm.rb
#  topaz 1> error , too many arguments,
#            during /Users/pmclain/projects/maglev/git/pbm.rb
#  ERROR 2023, Error, 'too many arguments'
#  topaz 1>
#
h2cl = HeaderHash2
hhb = HeaderHash2.new(:a, :b, :c)
unless hhb.class == HeaderHash2 ; raise 'error' ; end
HeaderHash.testh(hhb)
true
