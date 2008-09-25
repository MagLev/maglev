# Test the logic of the parameter handling for String#count, String#delete, etc.

$failed = []
$count = 0
def test(actual, expected, msg)
    $count += 1
    $failed << "ERROR: #{msg} Expected: #{expected} actual: #{actual}" unless expected == actual
end

def report
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  puts $failed
  raise Err, $failed.join("\n") unless $failed.empty?
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
report
