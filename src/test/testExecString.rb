#  produces :dxstr   s-expression , ticket 98
puts %q/a percent-q string/

dateCmd='`date`'
cmd = "echo #{dateCmd}"
puts "dxstr command="
puts cmd
puts `echo #{dateCmd} `
puts "pwd="
puts `pwd`
puts "%x pwd="
puts %x/pwd/
