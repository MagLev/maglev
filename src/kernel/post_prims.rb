# This file is loaded after the primitives in kernel.rb.  The primitives
# are loaded with RubyGlobalScope allowDeffered: false, while this file is
# run when allowDeffered is true.  This means, e.g., that instance
# variables referenced for the first time by the files required here will
# be dynamic instance variables.  Also constant assignment and resolution
#  will be fully dynamic

require 'kernel/post_prims/Object.rb'
require 'kernel/post_prims/StructTms.rb'
require 'kernel/post_prims/bigdecimal.rb'
require 'kernel/post_prims/ffi.rb'
require 'kernel/post_prims/openssl.rb'

require 'kernel/workarounds/load_all.rb'
