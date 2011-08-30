# MagLev does not escape \ properly in sub!
#
#  "X".sub!('X') { '\\\ ' }
#
# MRI:     "\\\\ "
# MagLev:  "\\ "

x = "X"
x.sub!('X') { '\ \\ \\\ \\\\ Y' }
raise "Fail Block form" unless x == "\\ \\ \\\\ \\\\ Y"


x = "X"
x.sub!('X', '\ \\ \\\ \\\\ Y')
raise "Fail Param form" unless x == "\\ \\ \\ \\ Y"
