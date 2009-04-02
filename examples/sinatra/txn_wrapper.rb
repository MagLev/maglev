# Rack middleware to wrap each http request in a transaction.
#
# This class saves "data", but does not copy the methods from session temps
# to a persistent root.  The transaction is committed only upon success (a
# non nil status code < 300.
#
# TODO: Should HTTP redirects commit or not?
#
puts "== txn_wrapper.rb"
require 'maglev.rb'
class MagLevTransactionWrapper
#  include Maglev::CodeAndData
  include Maglev::DataOnly

  def initialize(app)
    @app = app
  end

  def call(env)
    begin
      puts "=== (Begin Transaction)"
      status, headers, body = @app.call env
      [status, headers, body]
    ensure
      if good_status?(status)
        puts "=== Commit Transaction"
        commit_txn if running_maglev?
      else
        puts "=== Abort Transaction"
        abort_txn  if running_maglev?
      end
    end
  end

  # A Commit-worthy status is success (2xx).
  def good_status?(status)
    ! status.nil? &&  (200..299).include?(status)
  end
end
