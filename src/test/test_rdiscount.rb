# NOTE, run with
#   LD_LIBRARY_PATH=/export/iceland1/users/otisa/gitPublic/rubyCexts/rdiscount/ext:/lib:/usr/lib
# or
# setenv LD_LIBRARY_PATH '/export/iceland1/users/otisa/gitPublic/rubyCexts/rdiscount/ext:/lib:/usr/lib'

Dir.chdir('/export/iceland1/users/otisa/gitPublic/rubyCexts/rdiscount')
lp = $LOAD_PATH 
lp.insert(0, 'lib/' )
load 'lib/markdown.rb'
require 'rubygems'

lp << 'test/'

load 'test/rdiscount_test.rb'
load 'test/benchmark.rb'
load 'test/markdown_test.rb'

