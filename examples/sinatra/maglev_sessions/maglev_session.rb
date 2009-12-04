
require 'rack/session/abstract/id'

# MaglevSession is rack middleware that provides two services:
#
# 1: Wraps each HTTP request in a MagLev transaction
#
# 2: Implements a hash based session store that uses MagLev distributed
#    persistence.
#
# Even though we could use the session data's OOP as a session id, we
# instead use <tt>Rack::Session::Abstract::ID#generate_sid</tt> because we
# want random, hard to guess session ids going across the wire, and OOPs
# will eventually be reused.
#
# TODO:
# 1: Need to safely initialize SESSIONS.  Needs to be robust in face of
#    many VMs starting simultanesouly.  We could put it in a migration, and
#    then just fail here if we detect SESSIONS.nil?  Whatever scheme we
#    come up with should be put in a module so that we can offer a "Create
#    a hash store for this class" mixin.
#
# 2: Need to use RC* classes to avoid conflicts in multi-vm setup.

class MaglevSession < Rack::Session::Abstract::ID

  PERSISTENT_KEY = :MaglevSession  # Use symbol: ID might not be persistable.
  if Maglev::PERSISTENT_ROOT[PERSISTENT_KEY].nil?
    Maglev.abort_transaction
    Maglev::PERSISTENT_ROOT[PERSISTENT_KEY] = Hash.new  # Should be an RC* class
    Maglev.commit_transaction
  end

  def initialize(*args)
    super
    @sessions = Maglev::PERSISTENT_ROOT[PERSISTENT_KEY]
  end

  # Callback from Rack::Session::... to get the session from the store at
  # the beginning of the HTTP request.
  def get_session(env, sid)

    # Start each HTTP request with a fresh view of the repository.
    Maglev.abort_transaction

    session = @sessions[sid] if sid
    unless sid and session
      session = Hash.new
      sid = generate_sid
      @sessions[sid] = session # Rely on the commit_transaction in set_session
    end
    return [sid, session]
  rescue Exception => e
    puts "== Get Session: EXCEPTION: #{e}"
    return [nil, {}]
  end

  # Callback from Rack::Session::... to save the session data into the
  # store.  Called at the end of the HTTP request.
  def set_session(env, sid, new_session, options)
#     if options[:drop]
#       @sessions[sid] = nil
#       return false
#     end
    @sessions[sid] = new_session

    # Commit the repository, including session data.
    Maglev.commit_transaction
    return sid
  end

  def generate_sid
    # TODO: We should really just encrypt an OOP.  The advantages here are we
    # know that the OOP is globally unique, so we won't run into the danger of
    # an sid collision at http commit time (which is going to be waaaay to late
    # to do anything about, since we've already returned from the app.
    loop do
      sid = super
      break sid unless @sessions.has_key?(sid)
    end
  end

end
