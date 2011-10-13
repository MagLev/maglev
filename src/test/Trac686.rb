# Distilled from optparse.rb
#
# Maglev is not handling passing *[] correctly.
#
class C
  def zero(id, *args, &block)
    # MRI prints:     [:long, "pre", true]
    # MagLev prints:  [:long, "pre", true, []]
    p args     # MRI:
    raise "Wrong num args #{args.size} expecting 3" unless args.size == 3
  end

  def self.test
    pat = []
    self.new.zero(:one, :long, "pre", true, *pat)
    self.new.zero(:one, :long, "pre", true, *pat) { 10 }
    true
  end
end

C.test
#################### Trac Info
# ID:         686
# Summary:    parameter passing issue *[]
# Changetime: 2010-03-30 22:54:21+00:00
###

#  The code is distilled from optparse, and is the root cause of why using shortened forms of options does not work, e.g., passing "--pre" instead of "--prerelease" to rubygems.
#  
#  {{{
#  # Distilled from optparse.rb
#  #
#  # Maglev is not handling passing *[] correctly.
#  #
#  class C
#    def zero(id, *args, &block)
#      # MRI prints:     [:long, "pre", true]
#      # MagLev prints:  [:long, "pre", true, []]
#      p args     # MRI:
#      raise "Wrong num args #{args.size} expecting 3" unless args.size == 3
#    end
#  end
#  
#  pat = []
#  C.new.zero(:one, :long, "pre", true, *pat)
#  }}}
#  
#  MRI output
#  
#  {{{
#  $ ruby src/test/TracXXX.rb 
#  [:long, "pre", true]
#  }}}
#  
#  MagLev output
#  
#  {{{
#  $ maglev-ruby src/test/TracXXX.rb 
#  [:long, "pre", true, []]
#  error , Wrong num args 4 expecting 3,
#            during /Users/pmclain/GemStone/checkouts/git/src/test/TracXXX.rb
#  ERROR 2023, Error, 'Wrong num args 4 expecting 3' (RuntimeError)
#  
#  }}}
#  
#  
#  