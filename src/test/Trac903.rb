
class Trac903
  def test
    [10,20,30].__send__ :last, *[]
  end
end

x = Trac903.new.test
unless x == 30 ; raise 'fail' ; end
true
#################### Trac Info
# ID:         903
# Summary:    Overloaded method #last on Array breaks sending or calling as proc
# Changetime: 2011-05-24 16:32:46+00:00
###

#  The following code throws an ArgumentError:
#  
#  {{{
#  [1,2,3].__send__ :last, *[]
#  }}}
#  
#  In Smalltalk land, because we're passing an empty array, this maps to #last* and throws said error. This breaks Sinatra routing code, which has something like this:
#  
#  {{{
#  :last.to_proc.call([1,2,3])
#  }}}
#  
#  I could gather from bootstrap/Array.rb that there is an implementation for #last(count) and #last, maybe merging those and making the argument optional (more closely following MRI) would help here.