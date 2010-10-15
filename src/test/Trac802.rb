# MagLev fails with:
# $ mruby $pbm
# #<TypeError: An attempt was made to change the invariant object ''.>
# ERROR 2031, An attempt was made to change the invariant object ''. (TypeError)

BIN='/bin'
x = "echo #{`#{BIN}/echo foo`}"
puts x

