
module Readline

  # callers of readline responsible for HISTORY.push
  #
  def readline(prompt, ignored_boolean)
    unless (prompt._isStringOrSymbol)
      prompt = prompt.call  # expecting a Proc
    end
    File.stdin.line_editor_readline(prompt)
  end

  class HistoryArray < Array
    def push(line)
      # based on usage seen in irb
      if self.__size >= 200
        self.__remove_from_to_(1, 1)  # one-based args
      end
      self.__push(line)
    end
  end
   
  HISTORY = HistoryArray.new
end
