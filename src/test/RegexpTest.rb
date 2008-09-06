#
# Tests the following:
#    Regular expressions

# Class definition for regular expression tests

class RegexpTest
    # Expected value: whatever in str matches exp
    def match(str, exp)
        regexp = Regexp.new(exp);
        if str =~ regexp
            return "#{$&}"
        else
            return "NoMatch"
        end
    end

    # Expected value: whatever in str precedes the exp match
    def before(str, exp)
        regexp = Regexp.new(exp);
        if str =~ regexp
            return "#{$`}"
        else
            return "NoMatch"
        end
    end

    # Expected value: whatever in str follows the exp match
    def after(str, exp)
        regexp = Regexp.new(exp);
        if str =~ regexp
            return "#{$'}"
        else
            return "NoMatch"
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
ret = RegexpTest.new.match("abcdefghi", "def")
raise "ERROR" unless ret == 'def'

# expectvalue 'NoMatch'
ret = RegexpTest.new.match("abcdefghi", "xyz")
raise "ERROR" unless ret == 'NoMatch'

# expectvalue 'abc'
ret = RegexpTest.new.match("abcabcabc", "abc")
raise "ERROR" unless ret == 'abc'

# expectvalue 'abc'
ret = RegexpTest.new.before("abcdefghi", "def")
raise "ERROR" unless ret == 'abc'

# expectvalue 'testin'
ret = RegexpTest.new.before("testingregexp", "gr")
raise "ERROR" unless ret == 'testin'

# expectvalue 'ghi'
ret = RegexpTest.new.after("abcdefghi", "def")
raise "ERROR" unless ret == 'ghi'

# expectvalue 'egexp'
ret = RegexpTest.new.after("testingregexp", "gr")
raise "ERROR" unless ret == 'egexp'

# expectvalue 'NoMatch'
ret = RegexpTest.new.match("abcdefghi", "$def")
raise "ERROR" unless ret == 'NoMatch'

# expectvalue 'NoMatch'
ret = RegexpTest.new.match("abcdefghi", "$abc")
raise "ERROR" unless ret == 'NoMatch'

# expectvalue 'NoMatch'
ret = RegexpTest.new.match("abcdefghi", "^abc")
raise "ERROR" unless ret == 'abc'

# expectvalue 'ghi'
ret = RegexpTest.new.match("abcdefghi", "ghi$")
raise "ERROR" unless ret == 'ghi'

# expectvalue 'c'
ret = RegexpTest.new.match("abcdefghi", "[cd]")
raise "ERROR" unless ret == 'c'

# expectvalue a
ret = RegexpTest.new.match("abcdefghi", "[^cd]")
raise "ERROR" unless ret == 'a'

# expectvalue 'def'
ret = RegexpTest.new.match("abcdefghi", ".ef")
raise "ERROR" unless ret == 'def'

# expectvalue 'e'
ret = RegexpTest.new.match("abcdefghi", "[.ef]")
raise "ERROR" unless ret == 'e'

# expectvalue 'NoMatch'
ret = RegexpTest.new.match("ABCDEFGHI", "abc")
raise "ERROR" unless ret == 'NoMatch'

# expectvalue 'DEF'
ret = RegexpTest.new.match("ABCDEFGHI", "DEF")
raise "ERROR" unless ret == 'DEF'

# expectvalue 'NoMatch'
ret = RegexpTest.new.match("AbCdEfGhI", "DEF");
raise "ERROR" unless ret == 'NoMatch'

# expectvalue 'A'
ret = RegexpTest.new.match("AbCdEfGhI", "[^a..z]")
raise "ERROR" unless ret == 'A'

# expectvalue 'T'
ret = RegexpTest.new.match("Testingregularexpressions", "[[:alpha:]]")
raise "ERROR" unless ret == 'T'

# expectvalue 'NoMatch'
ret = RegexpTest.new.match("Testingregularexpressions", "[[:blank:]]")
raise "ERROR" unless ret == 'NoMatch'

# expectvalue '1'
ret = RegexpTest.new.match("1Testing2regular3expressions", "[[:alnum:]]")
raise "ERROR" unless ret == '1'

# expectvalue 'eee'
ret = RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e+")
raise "ERROR" unless ret == 'eee'

# expectvalue 'abc'
ret = RegexpTest.new.after("abc", "b*")
raise "ERROR" unless ret == 'abc'

# expectvalue 'abc'
ret = RegexpTest.new.after("abc", "b?")
raise "ERROR" unless ret == 'abc'

# expectvalue 'eee'
ret = RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e{1,3}")
raise "ERROR" unless ret == 'eee'

# expectvalue 'eee'
ret = RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e{1,4}")
raise "ERROR" unless ret == 'eee'

# expectvalue 'NoMatch'
ret = RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e{4,5}")
raise "ERROR" unless ret == 'NoMatch'

# expectvalue 'ee'
ret = RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e{2}")
raise "ERROR" unless ret == 'ee'

# expectvalue 'NoMatch'
ret = RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "e{4}")
raise "ERROR" unless ret == 'NoMatch'

# expectvalue 'efff'
ret = RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "ef+")
raise "ERROR" unless ret == 'efff'

# expectvalue 'eee'
ret = RegexpTest.new.match("aaabbbcccdddeeefffggghhhiii", "eee|fff")
raise "ERROR" unless ret == 'eee'

# expectvalue '.'
ret = RegexpTest.new.match("abcde.fghi", "\\.")
raise "ERROR" unless ret == '.'

# expectvalue 'bbb'
ret = RegexpTest.new.match("aabbbaacccaadddaaeee", "(\\w)\\1\\1")
raise "ERROR" unless ret == 'bbb'

# expectvalue 'def'
ret = RegexpTest.new.match("abcdefghijklmnoprqstuvwxyz", "dz*ef")
raise "ERROR" unless ret == 'def'

# expectvalue 'dzef'
ret = RegexpTest.new.match("abcdzefghijklmnoprqstuvwxyz", "dz*ef")
raise "ERROR" unless ret == 'dzef'
