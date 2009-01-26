#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require "tagz"

module Ramaze
  module Template

    # Is responsible for compiling a template using the Tagz templating engine.
    # Can be found at: http://rubyforge.org/projects/codeforpeople/
    #   gem install tagz
    # be sure you get version 1.0 or higher

    class Tagz < Template
      ENGINES[self] = %w[ rb tagz ]

      class << self

        # Entry point for Action#render

        def transform action
          result, file = result_and_file(action)
          if file
            markup = "tagz{#{ file }}"
            action.instance.extend Methods
            result = eval markup, action.binding, file
          end
          result
        end

      end

      # Methods defines a host of methods useful inside the context of a view
      # including print style methods that output content rather that printing to
      # STDOUT

      module Methods
        include ::Tagz

      private
        def << s
          tagz << s; self
        end

        def concat *a
          a.each{|s| tagz << s}; self
        end

        def puts *a
          a.each{|elem| tagz << "#{ elem.to_s.chomp }#{ eol }"}
        end

        def print *a
          a.each{|elem| tagz << elem}
        end

        def p *a
          a.each{|elem| tagz << "#{ Rack::Utils.escape_html elem.inspect }#{ eol }"}
        end

        def pp *a
          a.each{|elem| tagz << "#{ Rack::Utils.escape_html PP.pp(elem, '') }#{ eol }"}
        end

        def eol
          if response.content_type =~ %r|text/plain|io
            "\n"
          else
            "<br />"
          end
        end

        def __ *a
          concat eol
        end
      end
    end
  end
end
