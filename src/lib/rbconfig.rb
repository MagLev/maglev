# In other rubies this file gets created by mkconfig.rb when ruby is built. It's 
# usually used to describe various settings and other information used in the build
# For more informations refer to pickaxe p.183
# 
# Various files in the MSpec libs require this Module. At this point it's hard coded
# rather than generated to fulfill the mspec requirement.
#
module Config
    CONFIG = {}
    CONFIG['bindir'] = ""
    CONFIG['ruby_install_name'] = ""
    CONFIG['EXEEXT'] = ""
end

RbConfig = Config
