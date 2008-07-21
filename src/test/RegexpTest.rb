#
# Tests the following:
#    Regular expressions

# Class definition for regular expression tests

class RegexpTest
    # Expected value: whatever in str matches exp
    def match(str, exp)
        regexp = Regexp.new(exp);
        if str =~ regexp
            puts "#{$&}"
        else
            puts "NoMatch"
        end
    end

    # Expected value: whatever in str precedes the exp match
    def before(str, exp)
        regexp = Regexp.new(exp);
        if str =~ regexp
            puts "#{$`}"
        else
            puts "NoMatch"
        end
    end

    # Expected value: whatever in str follows the exp match
    def after(str, exp)
        regexp = Regexp.new(exp);
        if str =~ regexp
            puts "#{$'}"
        else
            puts "NoMatch"
        end
    end

end


###
# Call test methods
###

puts ""
puts "****************************"
puts "Beginning method invocations"
puts "****************************"
puts ""

# expectvalue 'def'
RegexpTest.new.match("abcdefghi", "def")

# expectvalue 'NoMatch'
RegexpTest.new.match("abcdefghi", "xyz")

# expectvalue 'abc'
RegexpTest.new.match("abcabcabc", "abc")

# expectvalue 'abc'
RegexpTest.new.before("abcdefghi", "def")

# expectvalue 'testin'
RegexpTest.new.before("testingregexp", "gr")

# expectvalue 'ghi'
RegexpTest.new.after("abcdefghi", "def")

# expectvalue 'egexp'
RegexpTest.new.after("testingregexp", "gr")

# expectvalue 'NoMatch'
RegexpTest.new.match("abcdefghi", "$def")

# expectvalue 'NoMatch'
RegexpTest.new.match("abcdefghi", "$abc")

# expectvalue 'NoMatch'
RegexpTest.new.match("abcdefghi", "^abc")

# expectvalue 'ghi'
RegexpTest.new.match("abcdefghi", "ghi$")

# expectvalue 'c'
RegexpTest.new.match("abcdefghi", "[cd]")

# expectvalue ?
RegexpTest.new.match("abcdefghi", "[^cd]")

# expectvalue 'def'
RegexpTest.new.match("abcdefghi", ".ef")

# expectvalue 'def'
RegexpTest.new.match("abcdefghi", "[.ef]")

# expectvalue 'NoMatch'
RegexpTest.new.match("ABCDEFGHI", "abc")

# expectvalue 'DEF'
RegexpTest.new.match("ABCDEFGHI", "DEF")

# expectvalue 'NoMatch'
RegexpTest.new.match("AbCdEfGhI", "DEF");

# expectvalue 'A'
RegexpTest.new.match("AbCdEfGhI", "[^a..z]")

# expectvalue 'T'
RegexpTest.new.match("Testingregularexpressions", "[[:alpha:]]")

# expectvalue 'NoMatch'
RegexpTest.new.match("Testingregularexpressions", "[[:blank:]]")

# expectvalue '1'
RegexpTest.new.match("1Testing2regular3expressions", "[[:alnum:]]")

# expectvalue 'eee'
RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e+")

# expectvalue 'abc'
RegexpTest.new.after("abc", "b*")

# expectvalue 'abc'
RegexpTest.new.after("abc", "b?")

# expectvalue 'eee'
RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e{1,3}")

# expectvalue 'eee'
RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e{1,4}")

# expectvalue 'NoMatch'
RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e{4,5}")

# expectvalue 'ee'
RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e{2}")

# expectvalue 'NoMatch'
RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e{4}")

# expectvalue 'efff'
RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "ef+")

# expectvalue 'eee'
RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "eee|fff")

# expectvalue '.'
RegexpTest.new.match("abcde.fghi", "\\.")

# expectvalue 'bbb'
RegexpTest.new.match("aabbbaacccaadddaaeee", "(\\w)\\1\\1")

# expectvalue 'def'
RegexpTest.new.match("abcdefghijklmnoprqstuvwxyz", "dz*ef")

# expectvalue 'def'
RegexpTest.new.match("abcdzefghijklmnoprqstuvwxyz", "dz*ef")
