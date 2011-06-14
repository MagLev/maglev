
$file = '(__DELEGATE__)'
$line_number = 100

class EvalLine
  def foo2
    stack = caller()
    line = stack.grep(/#{$file}/)[0]
    unless line 
       raise "Didn't find #{$file} in caller" 
    end
    splits = line.split(':')
    file = splits[0]
    line_num = splits[1]
    method = splits[2]
    gfile = $file
    gline = $line_number
    unless file == gfile
      raise "Wrong file name: #{file} expecting"   
    end
    unless line_num.to_i == gline + 1
      raise "Wrong line number: #{line_num} expecting: #{gline}"
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
