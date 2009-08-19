# Rack middleware to wrap each http request in a transaction.
#
# The transaction is committed only upon success or redirect.
#
puts "== txn_wrapper.rb"
class MagLevTransactionWrapper
  def initialize(app)
    @app = app
  end

  def call(env)
    begin
      puts "=== New HTTP request: (abort transaction to start)"
      Maglev.abort_transaction  if defined? Maglev
      status, headers, body = @app.call env
      [status, headers, body]
    ensure
      if good_status?(status)
        puts "=== Commit Transaction (status #{status})"
        Maglev.commit_transaction if defined? Maglev
      else
        puts "=== Abort Transaction (status #{status})"
        Maglev.abort_transaction  if defined? Maglev
      end
    end
  end

  # A Commit-worthy status is success (2xx) or a redirect.
  def good_status?(status)
    ! status.nil? &&  (200..399).include?(status)
  end

end
