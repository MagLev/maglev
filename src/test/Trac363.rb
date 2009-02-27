# From webrick/httpstatus.rb
module HTTPStatus
  eval %-
    RC_BAD_REQUEST = 400
    class BadRequest < StandardError
      def self.code() 
        RC_BAD_REQUEST 
      end
      def code() 
        self::class::code + 1 
      end
    end
  -
end

a = HTTPStatus::BadRequest.code
b = HTTPStatus::BadRequest.new.code
unless a == 400 ; raise 'error'; end
unless b == 401 ; raise 'error'; end
true
