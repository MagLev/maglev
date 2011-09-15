# From spec file  core/argf/seek_spec.rb

# Maglev gets parse error 
#   SyntaxError: missing or unexpected :"," 
# For this line:
#   ARGF.seek -@file1.first.size-@file1.last.size, IO::SEEK_END

# A simpler production of the error is
#   @f.seek -@iva, IO::SEEK_CUR

# Workaround - use parenthesis
#   ARGF.seek( -@file1.first.size-@file1.last.size, IO::SEEK_END )

#   @f.seek( -@iva, IO::SEEK_CUR )

class C
  def initialize
    @x = 10
    @y = 20
  end

  def x(num)
    p num
  end
  
  def f
    self.x -@x-@y
  end
end

C.new.f
