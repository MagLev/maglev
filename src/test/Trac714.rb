# Maglev has yet another protection problem.
#
#   respond_to?: false
#   #<RuntimeError: Fail>
#   /Users/pmclain/GemStone/dev/pbm.rb:17:in `raise'
#   /Users/pmclain/GemStone/dev/pbm.rb:17:in `raise'
#   /Users/pmclain/GemStone/dev/pbm.rb:17:in `load_file'
#   /Users/pmclain/GemStone/dev/pbm.rb:28:in `m'
#   /Users/pmclain/GemStone/dev/pbm.rb:32
#   ERROR 2023, Error, 'Fail' (RuntimeError)

module Base
  protected

  def load_file
    raise 'Fail' unless respond_to?(:load_yml)
    714
  end

  def load_yml
    10
  end
end

class C
  include Base
  def m
    load_file
  end
end

x = C.new.m
unless x == 714 ; raise 'error'; end
true
