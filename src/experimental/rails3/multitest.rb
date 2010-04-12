p $LOAD_PATH
require 'multimap'

map = Multimap.new
map['a'] = 100
map['b'] = 200
map['a'] = 300

p map

