
# sub was not doing \1 replacement...
r = "hello".sub /h([aeiou])llo/, '\1'
raise "Expected 'e' but got #{r}" unless r == 'e'
