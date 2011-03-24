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
