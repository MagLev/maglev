# Rack middleware to wrap each http request in a transaction, and to aquire
# a lock so that there is only thread running in each transaction.
#
# The transaction is committed only upon success or redirect.
class LockingTransactionWrapper

  # Mutex state is never written to the repository, so although this looks
  # like a global, across VM Mutex, it is really only per-VM.
  #
  # TODO: Perhaps we should provide a class level accessor to initialize
  # and pass one back? (accessor would also have to be protected....).
  MUTEX = Mutex.new

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
    MUTEX.lock
    Maglev.abort_transaction
    r = @app.call env
  ensure
    # Don't abort if ! committable? since next request will abort anyway
    Maglev.commit_transaction if committable? r[0]
    MUTEX.unlock
  end

  # A Commit-worthy status is success (2xx) or a redirect.
  #
  # @param [Fixnum] status the http status of the underlying response.
  # @return [Boolean] whether the status should trigger a commit or not.
  def committable?(status)
    ! status.nil? &&  (200..399).include?(status)
  end
end
