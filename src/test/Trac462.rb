require File.expand_path('simple', File.dirname(__FILE__))

test("test".gsub(/e(.)/, '\1e'), 'tset', 'Trac462 A')

report
#################### Trac Info
# ID:         462
# Summary:    Backslash interpolation of captured groups in String.sub,gsub, etc..
# Changetime: 2009-04-24 21:27:45+00:00
###

#  Backslash interpolation in substitution strings isn't working.
#  {{{
#  "test".gsub(/e(.)/,'\1e')
#  }}}
#  Should produce: 
#  {{{
#  "tset"
#  }}}
#  but it produces:
#  {{{
#  "t\\1et"
#  }}}
#  
#  This is easy to work around, but may cause mysterious breakage in third party apps.
#  