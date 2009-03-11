
$file = '(__DELEGATE__)'
$line_number = 100

class EvalLine
  def foo2
    stack = caller()
    line = stack.grep(/#{$file}/)[0]
    unless line 
       raise "Didn't find #{$file} in caller" 
    end
    file, line, method = line.split(':')
    unless file == $file
      raise "Wrong file name: #{file} expecting"   
    end
    unless line.to_i == $line_number
      raise "Wrong line number: #{line} expecting: #{$line_number}"
    end
  end

  def baz
    setup_eval
    foo
  end

  def setup_eval
    str = "def foo() \n foo2() \n end"
    eval(str, binding, $file, $line_number)
  end
end
c = EvalLine.new
c.baz

$file = nil
$line_number = nil

true
