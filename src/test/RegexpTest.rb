#
# Tests the following:
#    Regular expressions

# Class definition for regular expression tests

# Some simple tests that don't use $&, $` or $'.

require File.expand_path('simple', File.dirname(__FILE__))

test(Regexp.escape('\\[]*?{}.'), '\\\\\\[\\]\\*\\?\\{\\}\\.', 'Pickaxe escape A')

test(Regexp.escape("\n\t\f\r "), "\\n\\t\\f\\r\\ ", 'Gemstone escape A')

test( /abc/.source , 'abc' , 'simple regexp source' )
test( /\\/.source , "\\\\" , 'Trac343')

#  Test MatchData
md = /(.)(.)(\d+)(\d)/.match("THX1138.")
test(md[0],    "HX1138",                           "Pickaxe MatchData A")
test(md[1,2],  ["H", "X"],                         "Pickaxe MatchData B")
test(md[1..3], ["H", "X", "113"],                  "Pickaxe MatchData C")
test(md[-3,2], ["X", "113"],                       "Pickaxe MatchData D")

test(md.begin(0), 1,                               "Pickaxe MatchData E")
test(md.begin(2), 2,                               "Pickaxe MatchData F")

test(md.captures, %w(H X 113 8),                   "Pickaxe MatchData G")

test(md.end(0), 7,                                 "Pickaxe MatchData H")
test(md.end(2), 3,                                 "Pickaxe MatchData I")

test(md.length, 5,                                 "Pickaxe MatchData J")
test(md.size,   5,                                 "Pickaxe MatchData K")

test(md.offset(0), [1,7],                          "Pickaxe MatchData L")
test(md.offset(4), [6,7],                          "Pickaxe MatchData M")

test(md.post_match, ".",                           "Pickaxe MatchData N")

test(md.pre_match, "T",                            "Pickaxe MatchData O")
test(md.string, "THX1138.",                        "Pickaxe MatchData P")

test(md.to_a, %w(HX1138 H X 113 8),                "Pickaxe MatchData Q")
test(md.to_s, "HX1138",                            "Pickaxe MatchData R")
test(md.values_at(0,2,-2), ["HX1138", "X", "113"], "Pickaxe MatchData S")


# Test that the Regexp constants are defined and operating correctly
test(Regexp.new('dog') =~ 'DoG',                   nil, "Regexp:: A")
test(Regexp.new('dog', Regexp::IGNORECASE) =~ 'DoG', 0, "Regexp::IGNORECASE B")

test(Regexp.new('.') =~ "\n",                     nil, "Regexp::MULTILINE A")
test(Regexp.new('.', Regexp::MULTILINE) =~ "\n",    0, "Regexp::MULTILINE B")

test(Regexp.new('.D') =~ "\nD",    nil, "Multiple options A")
test(Regexp.new('.D', Regexp::IGNORECASE) =~ "\nD",
     nil,
     "Multiple options A")
test(Regexp.new('.D', Regexp::IGNORECASE|Regexp::MULTILINE) =~ "\nD",
     0,
     "Multiple options A")


# Ensure regexp.to_s quotes '/'
test(Regexp.new('\A/rails/info/path\Z').to_s,
     '(?-mix:\A\/rails\/info\/path\Z)',
     'to_s quotes /')

# Test a couple of regressions with MatchData and distinguishing
# between no match and zero length string matches.
md = /^([+-]?)(0[bdox])?(.*)/i.match '123'

test(md[0], "123", 'A md[0]')
test(md[1], "",    'A md[1]')
test(md[2], nil,   'A md[2]')
test(md[3], "123", 'A md[3]')


md = /^(.*)::(.*)$/.match 'fc00::'

test(md[0], "fc00::", 'md[0]')
test(md[1], "fc00",   'md[1]')
test(md[2], "",       'md[2]')
test(md[3], nil,      'md[3]')

# Test a regression with inspect() vs inspect(touchedSet)
#test([/xyz/].inspect, "[/xyz/]", "[/xyz/].inspect" )

# Test a regression: Regexp.new() wouldn't accept a Regexp
#
# Test one is that new does not throw an exception:
munge_re = Regexp.new(/[^a-z0-9_.-]+/)
test(munge_re.match('XXX').class, MatchData, 'Munge A')
test(munge_re.match('xxx'), nil, 'Munge B')

# test UTF8 escapes
test(/\u00E4/ =~ "ä", 0, "UTF8 escape (1)")
test(/\u00E4/u =~ "ä", 0, "UTF8 escape (2)")
test(/\u00E4/ui =~ "ä", 0, "UTF8 escape (3)")
test(/\u{E4}/ =~ "ä", 0, "UTF8 escape (4)")
test(/[\u{80}-\u{D7FF}]/ =~ "ä", 0, "UTF8 escape (5)")
test(/\u00E4/ =~ "b", nil, "UTF8 escape (6)")
test(/[\x00-\xff]/n =~ "a", 0, "ASCII escape (1)")
test(/[\x00-\xff]/ni =~ "a", 0, "ASCII escape (2)")

report

class RegexpTest
    # Expected value: whatever in str matches exp
    def match(str, exp)
        regexp = Regexp.new(exp);
        if regexp  =~ str
            return "#{$&}"
        else
            return "NoMatch"
        end
    end

    # Expected value: whatever in str precedes the exp match
    def before(str, exp)
        regexp = Regexp.new(exp);
        if regexp =~ str
            return "#{$`}"
        else
            return "NoMatch"
        end
    end

    # Expected value: whatever in str follows the exp match
    def after(str, exp)
        regexp = Regexp.new(exp);
        if regexp =~ str
            return "#{$'}"
        else
            return "NoMatch"
        end
    end

end

# # Call test methods

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


# A complex example from webrick/common.rb.  This will test that match data
# handles [] appropriately
ABS_URI = /^
  ([a-zA-Z][-+.a-zA-Z\d]*):
  (?:
    ((?:[-_.!~*'()a-zA-Z\d;?:@&=+$,]|%[a-fA-F\d]{2})(?:[-_.!~*'()a-zA-Z\d;\/?:@&=+$,\[\]]|%[a-fA-F\d]{2})*)
  |
  (?:(?:
    \/\/(?:
    (?:(?:((?:[-_.!~*'()a-zA-Z\d;:&=+$,]|%[a-fA-F\d]{2})*)@)?  (?# 3: userinfo)
                   (?:((?:(?:(?:[a-zA-Z\d](?:[-a-zA-Z\d]*[a-zA-Z\d])?)\.)*(?:[a-zA-Z](?:[-a-zA-Z\d]*[a-zA-Z\d])?)\.?|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}|\[(?:(?:[a-fA-F\d]{1,4}:)*(?:[a-fA-F\d]{1,4}|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})|(?:(?:[a-fA-F\d]{1,4}:)*[a-fA-F\d]{1,4})?::(?:(?:[a-fA-F\d]{1,4}:)*(?:[a-fA-F\d]{1,4}|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}))?)\]))(?::(\d*))?))?(?# 4: host, 5: port)
               |
                 ((?:[-_.!~*'()a-zA-Z\d$,;+@&=+]|%[a-fA-F\d]{2})+)
               )
             |
             (?!\/\/))
             (\/(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*(?:;(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*)*(?:\/(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*(?:;(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*)*)*)?
           )(?:\?((?:[-_.!~*'()a-zA-Z\d;\/?:@&=+$,\[\]]|%[a-fA-F\d]{2})*))?
        )
        (?:\#((?:[-_.!~*'()a-zA-Z\d;\/?:@&=+$,\[\]]|%[a-fA-F\d]{2})*))?                  $/x

uri = "http://localhost:9001/hello"

if ABS_URI.match(uri)
   scheme, opaque, userinfo, host, port, registry, path, query, fragment = $~[1..-1]

  raise "Didn't find scheme" unless scheme == "http"
  raise "Didn't find host"   unless host   == "localhost"
  raise "Didn't find port"   unless port   == "9001"
  raise "Didn't find path"   unless path   == "/hello"
else
  raise "Failed to match #{uri}"
end

