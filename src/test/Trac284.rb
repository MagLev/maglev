uri = "http://localhost:9001/hello"
ABS_URI = /^
        ([a-zA-Z][-+.a-zA-Z\d]*):                     (?# 1: scheme)
        (?:
           ((?:[-_.!~*'()a-zA-Z\d;?:@&=+$,]|%[a-fA-F\d]{2})(?:[-_.!~*'()a-zA-Z\d;\/?:@&=+$,\[\]]|%[a-fA-F\d]{2})*)              (?# 2: opaque)
        |
           (?:(?:
             \/\/(?:
                 (?:(?:((?:[-_.!~*'()a-zA-Z\d;:&=+$,]|%[a-fA-F\d]{2})*)@)?  (?# 3: userinfo)
                   (?:((?:(?:(?:[a-zA-Z\d](?:[-a-zA-Z\d]*[a-zA-Z\d])?)\.)*(?:[a-zA-Z](?:[-a-zA-Z\d]*[a-zA-Z\d])?)\.?|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}|\[(?:(?:[a-fA-F\d]{1,4}:)*(?:[a-fA-F\d]{1,4}|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})|(?:(?:[a-fA-F\d]{1,4}:)*[a-fA-F\d]{1,4})?::(?:(?:[a-fA-F\d]{1,4}:)*(?:[a-fA-F\d]{1,4}|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}))?)\]))(?::(\d*))?))?(?# 4: host, 5: port)
               |
                 ((?:[-_.!~*'()a-zA-Z\d$,;+@&=+]|%[a-fA-F\d]{2})+)           (?# 6: registry)
               )
             |
             (?!\/\/))                              (?# XXX: '\/\/' is the mark for hostport)
             (\/(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*(?:;(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*)*(?:\/(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*(?:;(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*)*)*)?              (?# 7: path)
           )(?:\?((?:[-_.!~*'()a-zA-Z\d;\/?:@&=+$,\[\]]|%[a-fA-F\d]{2})*))?           (?# 8: query)
        )
        (?:\#((?:[-_.!~*'()a-zA-Z\d;\/?:@&=+$,\[\]]|%[a-fA-F\d]{2})*))?            (?# 9: fragment)
      $/x

fail "Expecting Match "unless ABS_URI.match(uri)

#################### Trac Info
# ID:         284
# Summary:    Parse error on complex regexp
# Changetime: 2008-12-16 00:23:24+00:00
###

#  MRI parses the regexp fine, but MagLev has a parse error.  There will be an appropriate src/test/TracXXX.rb file a few minutes after this defect report is submitted:
#  
#  Notice that this is a commented regexp ( /.../x), but I don't think the problem is with the comments (the "(?# 1: schema)" stuff), as I took those out of one version, and still had the parse error.
#  
#  This is from webrick/common.rb
#  
#  {{{
#  uri = "http://localhost:9001/hello"
#  ABS_URI = /^
#          ([a-zA-Z][-+.a-zA-Z\d]*):                     (?# 1: scheme)
#          (?:
#             ((?:[-_.!~*'()a-zA-Z\d;?:@&=+$,]|%[a-fA-F\d]{2})(?:[-_.!~*'()a-zA-Z\d;\/?:@&=+$,\[\]]|%[a-fA-F\d]{2})*)              (?# 2: opaque)
#          |
#             (?:(?:
#               \/\/(?:
#                   (?:(?:((?:[-_.!~*'()a-zA-Z\d;:&=+$,]|%[a-fA-F\d]{2})*)@)?  (?# 3: userinfo)
#                     (?:((?:(?:(?:[a-zA-Z\d](?:[-a-zA-Z\d]*[a-zA-Z\d])?)\.)*(?:[a-zA-Z](?:[-a-zA-Z\d]*[a-zA-Z\d])?)\.?|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}|\[(?:(?:[a-fA-F\d]{1,4}:)*(?:[a-fA-F\d]{1,4}|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})|(?:(?:[a-fA-F\d]{1,4}:)*[a-fA-F\d]{1,4})?::(?:(?:[a-fA-F\d]{1,4}:)*(?:[a-fA-F\d]{1,4}|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}))?)\]))(?::(\d*))?))?(?# 4: host, 5: port)
#                 |
#                   ((?:[-_.!~*'()a-zA-Z\d$,;+@&=+]|%[a-fA-F\d]{2})+)           (?# 6: registry)
#                 )
#               |
#               (?!\/\/))                              (?# XXX: '\/\/' is the mark for hostport)
#               (\/(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*(?:;(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*)*(?:\/(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*(?:;(?:[-_.!~*'()a-zA-Z\d:@&=+$,]|%[a-fA-F\d]{2})*)*)*)?              (?# 7: path)
#             )(?:\?((?:[-_.!~*'()a-zA-Z\d;\/?:@&=+$,\[\]]|%[a-fA-F\d]{2})*))?           (?# 8: query)
#          )
#          (?:\#((?:[-_.!~*'()a-zA-Z\d;\/?:@&=+$,\[\]]|%[a-fA-F\d]{2})*))?            (?# 9: fragment)
#        $/x
#  
#  fail "Expecting Match "unless ABS_URI.match(uri)
#  
#  }}}
#  