# Test the logic of the parameter handling for String#count, String#delete, etc.

Err = StandardError
$failed = []
$count = 0
def test(actual, expected, msg)
    $count += 1
    if expected == actual
      # ok
    else
      $failed << "ERROR: #{msg} Expected: #{expected} actual: #{actual}" 
      x = $failed
      x.pause
    end 
end

def report
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  puts $failed
  if $failed.empty?
    puts ok
  else
    x = $failed
    x.pause
    raise Err, $failed.join("\n") 
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
test(vf.inject("") { |v,n| v+n }, 'euiooue', "Pickaxe p 121")
report
