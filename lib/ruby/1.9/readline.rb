
module Readline

  # callers of readline responsible for HISTORY.push
  #
  def readline(prompt, ignored_boolean=false)
    unless (prompt._isStringOrSymbol)
      prompt = prompt.call  # expecting a Proc
    end
    File.stdin.line_editor_readline(prompt)
  end

  class HistoryArray < Array
    def push(*lines)
      # based on usage seen in irb
      sz = self.__size
      if sz >= 200
        count = sz.__min(lines.__size)
        if count > 0
          self.__remove_from_to_(1, count )  # one-based args
        end
      end
      self.__push(*lines)
    end
  end
   
  HISTORY = HistoryArray.new
end
