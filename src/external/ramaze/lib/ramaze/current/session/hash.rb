module Ramaze
  class Session

    # The Session::Hash acts as the wrapper for a simple Hash
    #
    # Its purpose is to notify the underlying cache, in which the sessions
    # are stored, about updates.
    class Hash

      # Sets @hash to an empty Hash

      def initialize(session)
        @session = session
        @hash = {}
      end

      # relays all the methods to the @hash and updates the session_cache in
      # Session.current.sessions if anything changes.

      def method_missing(*args, &block)
        result = @hash.send(*args, &block)
        Cache.sessions[@session.session_id] = self
        result
      end

      # Calls #inspect on the wrapped @hash

      def inspect
        @hash.inspect
      end

      # Use client side session variables

      def client
        Request.current['session.client'] ||= unmarshal(Request.current.cookies["#{Session::SESSION_KEY}-client"]) || {}
      end

      private

      # Marshal a session hash into safe cookie data. Include an integrity hash.
      def marshal(session)
        data = [ Marshal.dump(session) ].pack('m').chop
        "#{data}--#{generate_digest(data)}"
      end

      # Unmarshal cookie data to a hash and verify its integrity.
      def unmarshal(cookie)
        return unless cookie
        data, digest = cookie.split('--')
        return nil unless digest == generate_digest(data)
        Marshal.load(data.unpack('m').first)
      end

      # Generate the inline SHA512 message digest. Larger (128 bytes) than SHA256
      # (64 bytes) or RMD160 (40 bytes), but small relative to the 4096 byte
      # max cookie size.
      def generate_digest(data)
        Digest::SHA512.hexdigest "#{data}#{Session.trait[:secret]}"
      end
    end
  end
end
