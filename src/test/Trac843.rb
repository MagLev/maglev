# MagLev is double escaping somewhere.
#
#   $ ruby Trac843.rb
#   /x(?-mix:a\/b)y/
#   0
#
#   $ maglev-ruby Trac843.rb
#   /x(?-mix:a\\/b)y/
#   nil

a = %r{a/b}
as = a.to_s
b = %r{x#{a}y}
c = b.inspect
d = '/x(?-mix:a\/b)y/' #MRI result
raise "Fail #{c} should equal #{d}" unless c == d
true




# # the first few lines, taken from http://j.mp/eEEz2I
# data_mime_type = <<MIME_TYPES
#   # application/*
# application/activemessage 'IANA,[Shapiro]
# application/andrew-inset 'IANA,[Borenstein]
# application/applefile :base64 'IANA,[Faltstrom]
# MIME_TYPES

# # yanked from types.rb http://j.mp/gtTWnN
# module MIME
#   class Type
#     MEDIA_TYPE_RE = %r{([-\w.+]+)/([-\w.+]*)}o
#     UNREG_RE      = %r{[Xx]-}o
#     ENCODING_RE   = %r{(?:base64|7bit|8bit|quoted\-printable)}o
#     PLATFORM_RE   = %r|#{RUBY_PLATFORM}|o

#     SIGNATURES    = %w(application/pgp-keys application/pgp
#                    application/pgp-signature application/pkcs10
#                    application/pkcs7-mime application/pkcs7-signature
#                    text/vcard)

#     IANA_URL      = "http://www.iana.org/assignments/media-types/%s/%s"
#     RFC_URL       = "http://rfc-editor.org/rfc/rfc%s.txt"
#     FT_URL     = "http://datatracker.ietf.org/public/idindex.cgi?command=id_details&filename=%s"
#     LTSW_URL      = "http://www.ltsw.se/knbase/internet/%s.htp"
#     CONTACT_URL   = "http://www.iana.org/assignments/contact-people.htm#%s"
#   end
# end

# # from http://j.mp/fsBhlg
# _re = %r{
#   ^
#   ([*])?                                # 0: Unregistered?
#   (!)?                                  # 1: Obsolete?
#   (?:(\w+):)?                           # 2: Platform marker
#   #{MIME::Type::MEDIA_TYPE_RE}          # 3,4: Media type
#   (?:\s@([^\s]+))?                      # 5: Extensions
#   (?:\s:(#{MIME::Type::ENCODING_RE}))?  # 6: Encoding
#   (?:\s'(.+))?                          # 7: URL list
#   (?:\s=(.+))?                          # 8: Documentation
#   $
# }x


# # adapted from http://j.mp/gE9hCA
# item = data_mime_type.split($/)[1]
# raise "This should match so we can _re.match(item).captures" unless _re.match(item)


