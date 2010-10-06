# Rack middleware to wrap each http request in a transaction.
#
# This requires the Rack gem, and probably others.
#
# The transaction is committed only upon success or redirect.

module Maglev

  # This class is a sample piece of Rack middleware that starts a new
  # Maglev transaction with each HTTP request, and then commits if the
  # response is a 2xx code.  It is intended to be used with the VM in
  # auto-transaction mode.
  class TransactionWrapper

    # Initialize receiver.  Remember the app we delegate to.
    #
    # @param [#call] app the rack app we delegate to
    # @return [#call] the app
    def initialize(app)
      @app = app
    end

    # Wrap an HTTP request in a Maglev transaction.  Does an
    # abort_transaction (to start a new transaction) before passing the
    # request down to the next Rack app.  On return, it checks the status
    # code from the underlying app, and if it is success (2xx), then it
    # commits the transaction.
    #
    # @param [Hash] env the rack environment
    # @return [Array] the standard Rack triple: [status, headers, body].
    #  The values are unmodified from the lower layer app.
    #
    # @todo We need to handle commit failures.
    def call(env)
      Maglev.abort_transaction
      r = @app.call env
    ensure
      # Don't abort if ! committable? since next request will abort anyway
      Maglev.commit_transaction if committable? r[0]
    end

    # A Commit-worthy status is success (2xx) or a redirect.
    #
    # @param [Fixnum] status the http status of the underlying response.
    # @return [Boolean] whether the status should trigger a commit or not.
    def committable?(status)
      ! status.nil? &&  (200..399).include?(status)
    end
  end
end
