# Rack middleware to wrap each http request in a transaction.
#
# This class saves "data", but does not copy the methods from session temps
# to a persistent root.  The transaction is committed only upon success (a
# non nil status code < 300.
#
# TODO: Should HTTP redirects commit or not?
#
class MagLevTransactionWrapper
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
      puts "=== End Transaction"
      if good_status?(status)
        Gemstone.commitTransaction if running_maglev?
      else
        Gemstone.abortTransaction  if running_maglev?
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
