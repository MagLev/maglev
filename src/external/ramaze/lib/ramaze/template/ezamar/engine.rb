#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'digest/sha1'

# Ezamar is a very simple (and at no means complete) reimplementation of the
# Templating-engine found in Nitro.
#
# Since Nitros templating is based on REXML and Ezamar is not there are vast
# differences, but it tries to keep the look and feel as close as possible.

module Ezamar

  require 'ramaze/template/ezamar/element'

  # This class is responsible for initializing and compiling the template.

  class Template

    # Take a template (anything that responds to ::to_str) and options.
    # At the moment the only option used is :file, which is used to tell
    # Kernel::eval how to produce better backtraces.

    def initialize(template, options = {})
      @template, @options = template, options
      compile
    end

    # All ye who seek magic, look elsewhere, this method is ASAP (as simple as possible)
    #
    # There are some simple gsubs that build a final template which is evaluated
    #
    # The rules are following:
    # <?r rubycode ?>
    #   evaluate the code inside the tag, this is considered XHTML-valid and so is the
    #   preferred method for executing code inside your templates.
    #   The return-value is ignored
    # <% rubycode %>
    #   The same as <?r ?>, ERB-style and not valid XHTML, but should give someone who
    #   is already familiar with ERB some common ground
    # #{ rubycode }
    #   You know this from normal ruby already and it's actually nothing else.
    #   Interpolation at the position in the template, isn't any special taggy format
    #   and therefor safe to use.
    # <%= rubycode %>
    #   The result of this will be interpolated at the position in the template.
    #   Not valid XHTML either.
    #
    # TODO
    #   - provide C version or maybe use erbuis

    def compile
      temp = @template.dup
      start_heredoc = "T" << Digest::SHA1.hexdigest(temp)
      start_heredoc, end_heredoc = "\n<<#{start_heredoc}\n", "\n#{start_heredoc}\n"
      bufadd = "_out_ << "

      temp.gsub!(/<%(?!=)\s*(.*?)\s*%>/m,
            "#{end_heredoc} \\1; #{bufadd} #{start_heredoc}")
      temp.gsub!(/<\?r\s+(.*?)\s+\?>/m,
            "#{end_heredoc} \\1; #{bufadd} #{start_heredoc}")
      temp.gsub!(/<%=\s*(.*?)\s*%>/m,
            "#{end_heredoc} #{bufadd} (\\1); #{bufadd} #{start_heredoc}")

      @compiled = "_out_ = ''
      #{bufadd} #{start_heredoc} #{temp} #{end_heredoc}
      _out_"
    end

    # Takes a binding and evals it with the previously set options.

    def result(binding)
      eval(@compiled, binding, @options[:file]).strip
    end
  end
end
