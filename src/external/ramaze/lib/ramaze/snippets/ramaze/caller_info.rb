#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze

  # Gives you back the file, line and method of the caller number i
  # Example:
  #   Ramaze.caller_info(1)
  #   # => ['/usr/lib/ruby/1.8/irb/workspace.rb', '52', 'irb_binding']

  def self.caller_info(i = 1)
    file, line, meth = *parse_backtrace(caller[i])
  end

  # Parses one line of backtrace and tries to extract as much information
  # as possible.
  #
  # Example:
  #   line = "/web/repo/ramaze/lib/ramaze/dispatcher.rb:105:in `respond'"
  #   Ramaze.parse_backtrace(line)
  #   #=> ["/web/repo/ramaze/lib/ramaze/dispatcher.rb", "105", "respond"]

  def self.parse_backtrace(line = '')
    full = line.scan(/(.*?):(\d+):in `(.*?)'/).first
    return full if full and full.all?
    partial = line.scan(/(.*?):(\d+)/).first
    return partial if partial and partial.all?
    line
  end
end
