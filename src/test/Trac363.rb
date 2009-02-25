# From webrick/httpstatus.rb
module HTTPStatus
  eval %-
    RC_BAD_REQUEST = 400
    class BadRequest < StandardError
      def self.code() RC_BAD_REQUEST end
      def code() self::class::code  end
    end
  -
end

HTTPStatus::BadRequest.code
HTTPStatus::BadRequest.new.code

