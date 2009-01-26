#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  STATE = State.new

  # The module to be included into the Controller it basically just provides
  # #request, #response and #session, each accessing Thread.current to
  # retrieve the demanded object

  module Trinity
    extend StateAccessor

    state_accessor :request, :response, :session
  end
end
