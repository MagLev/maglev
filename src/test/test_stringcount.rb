# Test the logic of the parameter handling for String#count, String#delete,
# etc.

require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

s = "abc-^"

test s.count("a"), 1, "1A: a"
test s.count("^a"), 4, "1B: ^a"

test s.count("a", "a"),    1, "1C:  a,  a"
test s.count("a", "^a"),   0, "1D:  a, ^a"
test s.count("^a", "a"),   0, "1E: ^a,  a"
test s.count("^a", "^a"),  4, "1F: ^a, ^a"

test s.count("ab", "^a"),  1, "1G: ab, ^a"
test s.count("^a", "ab"),  1, "1H: ^a, ab"
test s.count("a",  "^ab"), 0, "1I: a, ^ab"

# Test the tr expansion

test s.count("a-a"), 1, "2A: a-a"
test s.count("a-b"), 2, "2B: a-b"
test s.count("b-a"), 0, "2C: b-a"

test s.count("^a-a"), 4, "2D: ^a-a"
test s.count("^a-b"), 3, "2E: ^a-b"
test s.count("^b-a"), 5, "2F: ^b-a"

# Test handling of '^' and '-'
test s.count("-"),   1, "3A: -"
test s.count("a-"),  2, "3B: a-"
test s.count("-a"),  2, "3C: -a"
test s.count("-a-"), 2, "3D: -a-"

test s.count("^-"),   4, "3E: ^-"
test s.count("^a-"),  3, "3F: ^a-"
test s.count("^-a"),  3, "3G: ^-a"
test s.count("^-a-"), 3, "3H: ^-a-"

test s.count("^"),   1, "3I: ^"
test s.count("^^"),  4, "3J: ^^"
test s.count("a^"),  2, "3K: a^"
test s.count("^a^"), 3, "3M: ^a^"

test s.count("a--"),  0, "3N: a--"  # '-' is ASCII 45
test s.count("--a"),  3, "3O: --a"
test s.count("--"),   1, "3P: --"
test s.count("---"),  1, "3Q: ---"
test s.count("----"), 1, "3R: ----"

report
