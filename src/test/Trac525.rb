def foo
  if block_given?
    super() { yield }
  end
end
true

# class CGI
#   module HtmlExtension
#     # Generate an Anchor element as a string.
#     #
#     # +href+ can either be a string, giving the URL
#     # for the HREF attribute, or it can be a hash of
#     # the element's attributes.
#     #
#     # The body of the element is the string returned by the no-argument
#     # block passed in.
#     #
#     #   a("http://www.example.com") { "Example" }
#     #     # => "<A HREF=\"http://www.example.com\">Example</A>"
#     #
#     #   a("HREF" => "http://www.example.com", "TARGET" => "_top") { "Example" }
#     #     # => "<A HREF=\"http://www.example.com\" TARGET=\"_top\">Example</A>"
#     #
#     def a(href = "") # :yield:
#       attributes = if href._isString
#                      { "HREF" => href }
#                    else
#                      href
#                    end
#       if block_given?
#         super(attributes){ yield }  # <======= Problem
#       else
#         super(attributes)
#       end
#     end
#   end
# end
#################### Trac Info
# ID:         525
# Summary:    Compile problem in cgi.rb
# Changetime: 2009-05-29 23:46:35+00:00
###

#  Distilled from lib/ruby/1.8/cgi.rb:
#  
#  Compiling this:
#  {{{
#  def foo
#    if block_given?
#      super() { yield }
#    end
#  end
#  }}}
#  
#  Gives:
#  
#  {{{
#  $ maglev-ruby src/test/TracXXX.rb
#  -- RubyFile>>load  : loading /Users/pmclain/projects/maglev/git/src/test/TracXXX.rb
#  error , No method was found for the selector #'irNewBlockNode' when sent to nil with arguments contained in anArray( ).,
#            during /Users/pmclain/projects/maglev/git/src/test/TracXXX.rb
#  ERROR 2010, No method was found for the selector #'irNewBlockNode' when sent to nil with arguments contained in anArray( ).
#  topaz 1> 
#  
#  }}}
#  
#  