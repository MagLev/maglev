#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module Helper::Thread
    def thread &block
      parent_thread = Thread.current
      Thread.new do
        begin
          block.call
        rescue Exception => e
          parent_thread.raise(e)
        end
      end
    end
  end
end
