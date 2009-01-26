module Ramaze
  class Session
    # The purpose of this class is to act as a unifier of the previous
    # and current flash.
    #
    # Flash means pairs of keys and values that are held only over one
    # request/response cycle. So you can assign a key/value in the current
    # session and retrieve it in the current and following request.
    #
    # Please see the FlashHelper for details on the usage as you won't need
    # to touch this class at all.
    class Flash
      include Enumerable

      def initialize sess
        @session = sess
      end

      # iterate over the combined session
      def each(&block)
        combined.each(&block)
      end

      # the current session[:FLASH_PREVIOUS]
      def previous
        session[:FLASH_PREVIOUS] || {}
      end

      # the current session[:FLASH]
      def current
        session[:FLASH] ||= {}
      end

      # combined key/value pairs of previous and current
      # current keys overshadow the old ones.
      def combined
        previous.merge(current)
      end

      # flash[key] in your Controller
      def [](key)
        combined[key]
      end

      # flash[key] = value in your Controller
      def []=(key, value)
        prev = session[:FLASH] || {}
        prev[key] = value
        session[:FLASH] = prev
      end

      # Inspects the combined SessionFlash

      def inspect
        combined.inspect
      end

      # Delete a key

      def delete(key)
        previous.delete(key)
        current.delete(key)
      end

      # check if combined is empty

      def empty?
        combined.empty?
      end

      def merge!(hash)
        current.merge!(hash)
      end

      def merge(hash)
        current.merge(hash)
      end

      private

      # Associated session object

      def session
        @session
      end
    end
  end
end
