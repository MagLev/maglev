# https://github.com/MagLev/maglev/issues/93
#
# $ LANG="it_IT.UTF-8" maglev-ruby -e 'p Float("0.0")'
# ERROR 2702 , invalid value for Float(): "0.0" (ArgumentError)


expected = "0.0\n"
x = %x{LANG="it_IT.UTF-8" maglev-ruby +d -e 'p Float("0.0")' }

raise "Fail: expected #{expected}, but was #{x}" unless expected == x

