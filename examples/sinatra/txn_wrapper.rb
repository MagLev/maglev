# Rack middleware to wrap each http request in a transaction.
#
# This class saves "data", but does not copy the methods from session temps
# to a persistent root.  The transaction is committed only upon success (a
# non nil status code < 300.
#
# TODO: Should HTTP redirects commit or not?
#

require 'maglev.rb'
class MagLevTransactionWrapper
  include Maglev::CodeAndData

  def initialize(app)
    @app = app
  end

  def call(env)
    begin
      #Gemstone.beginTransaction if running_maglev?
      puts "=== Begin Transaction"
      status, headers, body = @app.call env
      [status, headers, body]
    ensure
      if good_status?(status)
        puts "=== Commit Transaction"
        #Gemstone.commitTransaction if running_maglev?
        commit_txn if running_maglev?
      else
        puts "=== Abort Transaction"
        #Gemstone.abortTransaction  if running_maglev?
        abort_txn  if running_maglev?
      end
    end
  end

  def good_status?(status)
    not (status.nil? || status >= 300)
  end

  def running_maglev?
    !!defined? RUBY_ENGINE
  end
end
