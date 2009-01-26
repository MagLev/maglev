#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  class Controller
    # The default error-page handler. you can overwrite this method
    # in your controller and create your own error-template for use.
    #
    # Error-pages can be in whatever the templating-engine of your controller
    # is set to.
    #   Ramaze::Dispatcher::Error.current
    # holds the exception thrown.

    def error
      error = Dispatcher::Error.current
      title = error.message

      unless Action.current.template
        response['Content-Type'] = 'text/plain'
        respond %(
          #{error.message}
            #{error.backtrace.join("\n            ")}

          #{PP.pp request, '', 200}
        ).ui
      end

      backtrace_size = Global.backtrace_size
      @backtrace = error.backtrace[0..20].map{|line|
        file, lineno, meth = *Ramaze.parse_backtrace(line)
        lines = Ramaze.caller_lines(file, lineno, backtrace_size)

        [ lines, lines.object_id.abs, file, lineno, meth ]
      }

      # for backwards-compat with old error.zmr
      @colors = [255] * @backtrace.size

      @title = h(title)
      @editor = ENV['EDITOR'] || 'vim'
      title
    rescue Object => ex
      Log.error(ex)
    end
  end
end
