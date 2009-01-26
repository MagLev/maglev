#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Sequel
  class Model
    class << self
      def fill request_object = Ramaze::Request.current
        create(request_object.params)
      end
    end
  end
end
