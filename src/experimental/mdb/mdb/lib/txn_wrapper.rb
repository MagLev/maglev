# Rack middleware to wrap each http request in a transaction.
#
# The transaction is committed only upon success or redirect.

class MagLevTransactionWrapper
  def initialize(app)
    @app = app
  end

  def call(env)
    begin
      Maglev.abort_transaction
      status, headers, body = @app.call env
      [status, headers, body]
    ensure
      if committable? status
        Maglev.commit_transaction
      else
        Maglev.abort_transaction
      end
    end
  end

  # A Commit-worthy status is success (2xx) or a redirect.
  def committable?(status)
    ! status.nil? &&  (200..399).include?(status)
  end

end
