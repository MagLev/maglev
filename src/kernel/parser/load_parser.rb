# file parser.rb
#   used for loading parser separately from bootstrap
# some files brought in with load to facilitate compiler re-load
require 'kernel/parser/RpStClasses.rb'
require 'kernel/parser/RubyNode.rb'
require 'kernel/parser/rp_classes.rb'
require 'kernel/parser/ffi.rb'
require 'kernel/parser/strscan.rb'
load 'kernel/parser/strscan2.rb'
require 'kernel/parser/racc_def_parser.rb'
load 'kernel/parser/extras.rb'
load 'kernel/parser/lexer.rb'
load 'kernel/parser/ruby_parser.rb'
require 'kernel/parser/racc_init_parser.rb'
load 'kernel/parser/racc_parser.rb'
