# Test the logic of the parameter handling for String#count, String#delete, etc.

$count = 0
def test(actual, expected, msg)
  $count += 1
  unless expected == actual
    emsg = "ERROR: #{msg} Expected: #{expected.inspect} actual: #{actual.inspect}" 
    puts emsg 
    self.pause
    raise 'ERROR' 
  end
end


#     BEGIN TEST CASES

# This class is from Pickaxe page 121
class VowelFinder
  include Enumerable

  def initialize(string)
    @string = string
  end

  def each
    @string.scan(/[aeiou]/) do |vowel|
      yield vowel
    end
  end
end

vf = VowelFinder.new("the quick brown fox jumped")
test(vf.inject { |v,n| v+n }, 'euiooue', "Pickaxe p 121")

class SimpleEnum
  include Enumerable

  def initialize(items)
    @items = items
  end

  def each
    @items.each { |i| yield i }
  end
end

test(SimpleEnum.new([1,2,3]).all? { |el| el < 5 }, true,    "Gemstone A")
test(SimpleEnum.new([1,2,3]).all? { |el| el < 2 }, false,   "Gemstone B")

test(SimpleEnum.new([1,2,3]).any? { |el| el < 2 }, true,    "Gemstone C")
test(SimpleEnum.new([1,2,3]).any? { |el| el > 200 }, false, "Gemstone D")

test(SimpleEnum.new([1,2,3]).collect { |el| el < 2 },
     [true, false, false],  "Gemstone E")
test(SimpleEnum.new([1,2,3]).collect { |el| el < 0 },
     [false, false, false],  "Gemstone F")

test(SimpleEnum.new([1,2,3]).detect { |el| el > 1 }, 2,        "Gemstone G")

hash = Hash.new
expected = { 1 => 0, 2 => 1, 3 => 2}
SimpleEnum.new([1,2,3]).each_with_index { |el,i| hash[el] = i }
test(hash, expected,                                           "Gemstone H")

test(SimpleEnum.new([1,2,3]).entries { |el| el < 2 }, [1,2,3], "Gemstone I")

test(SimpleEnum.new([1,2,3]).find { |el| el > 1 }, 2,          "Gemstone J")
test(SimpleEnum.new([1,2,3]).find_all { |el| el > 1 }, [2,3],  "Gemstone K")

test(SimpleEnum.new(['cat', 'catapult', 'dogapult']).grep(/cat/),
     ['cat', 'catapult'],                                      "Gemstone L")


test(SimpleEnum.new([1,2,3]).include?(1), true, "Gemstone M")
test(SimpleEnum.new([1,2,3]).include?(12), false, "Gemstone N")

test(SimpleEnum.new([1,2,3]).inject { |acc,x| acc + x}, 6, "Gemstone P")
test(SimpleEnum.new([1,2,3]).inject(10) { |acc,x| acc + x}, 16, "Gemstone Q")

test(SimpleEnum.new([1,2,3]).map {|el| el < 2}, [true,false,false], "Gemstone R")


test(SimpleEnum.new([1,2,3]).max, 3, "Gemstone S")  # Test Case "S"

# Running this sript with exactly one of test case "S" (above) or test case
# "T" (below) uncommented runs fine, but with them both uncommented, the
# 'invalid arg' error is thrown from _newProc:
#
# TODO: Bug: Ticket #132
# If the line below is uncommented, we get an 'invalid arg' error.
#
#test(SimpleEnum.new([2,3,1]).max, 3, "Gemstone T")  # Test Case "T"

test(SimpleEnum.new([1,2,3]).member?(1), true, "Gemstone U")

test(SimpleEnum.new([1,2,3]).min, 1, "Gemstone V")
test(SimpleEnum.new([1,2,3]).partition { |el| el < 2}, [[1],[2,3]], "Gemstone W")

test(SimpleEnum.new([1,2,3]).reject { |el| el < 2}, [2,3], "Gemstone X")

test(SimpleEnum.new([1,2,3]).select { |el| el % 2 == 0}, [2], "Gemstone Y")

#test(SimpleEnum.new([2,3,1]).sort, [1,2,3], "Gemstone Z")
#test(SimpleEnum.new([2,3,1]).sort { |a,b| b <=> a }, [3,2,1], "Gemstone AA")

test(SimpleEnum.new([1,2,3]).to_a, [1,2,3], "Gemstone AB")

a = [4,5,6]
b = [7,8,9]
test(SimpleEnum.new([1,2,3]).zip(a,b), [[1,4,7],[2,5,8],[3,6,9]], "Gemstone AC")

report
