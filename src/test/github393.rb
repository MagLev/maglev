# Github issue 393: Can't create top level method
#     ./bin/maglev-ruby -w -e 'class X; def x; end; end'
# works fine, but:
#     ./bin/maglev-ruby -w -e 'def x; end'
# fails with:
#     ERROR 2010 , NoMethodError: undefined method `method_added' for NilClass (NoMethodError)

result = system("maglev-ruby -e 'def x; end'")
raise "Fail" unless result
