# Distilled from Rake::FileList
#

# The underlying problem is in send to an array
# A send with only the selector works fine:
a = ['fff']
raise "Fail A" unless a.send(:first) == 'fff'    # passes

# But a send with *args &block fails:
args = []
block = nil
raise "Fail B" unless a.send(:first, *args, &block) == 'fff' # Fails

# For a FileList, #first is broken.  MagLev returns an array rather than
# the first element of the array.  FileList works by delegating array
# methods to an underlying array
#
class FileList
  sym = :first
  class_eval %{
    def #{sym}(*args, &block)
      result = @items.send(:#{sym}, *args, &block)
    end
  }

  def initialize
    @items = ["xyz"]
  end
end

x = FileList.new
raise "Fail C" unless x.first == 'xyz' # Fails
#################### Trac Info
# ID:         881
# Summary:    empty array instead of filename in chunky_png "rake benchmark"
# Changetime: 2011-03-25 16:19:00+00:00
###

#  chunky_png "rake benchmark:encoding" works in 1.8.7, but not in MagLev
#  
#  Here are steps to set it up.
#  {{{
#  git clone git://github.com/wvanbergen/chunky_png.git
#  cd chunky_png
#  maglev-gem install chunky_png
#  bundle install
#  rake benchmark:encoding
#  }}}
#  
#  Here is the error we get
#  {{{
#  (in /Users/monty/MagLev/github/chunky_png)
#  rake aborted!
#  Coercion error: [].to_str => String failed:
#  (NoMethodError: undefined method `to_str' for Array)
#  /Users/monty/MagLev/github/chunky_png/Rakefile:3:in `new'
#  (See full trace by running task with --trace)
#  a RubySystemExit occurred (error 2752)
#  }}}
#  