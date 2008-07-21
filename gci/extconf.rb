require 'mkmf'
extension_name = 'gci'
find_library("stdc++", nil)
find_library("gcirpc", nil, "../gemstone/lib32")
find_header("gci.hf", ["../gemstone/include"])
dir_config(extension_name)
create_makefile(extension_name)
