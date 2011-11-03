require 'maglev/orderedcollection'
require 'maglev/dateandtime'

ObjectLogEntry = __resolve_smalltalk_global(:ObjectLogEntry)
DebuggerLogEntry = __resolve_smalltalk_global(:DebuggerLogEntry)

Maglev.persistent do
  module ObjectLog
    extend self

    [:trace, :debug, :info, :warn, :error, :fatal].each do |m|
      define_method(:"#{m}s") do |abort|
        Maglev.abort_transaction if abort
        ObjectLogEntry.object_log.to_a.select do |log_entry|
          log_entry.priority == ObjectLogEntry.send(:"#{m}_level")
        end
      end
    end

    def to_a
      ObjectLogEntry.object_log
    end

    def to_ary
      ObjectLogEntry.object_log.to_a
    end

    def delete(entry, commit = true)
      Maglev.abort_transaction if commit
      ObjectLogEntry.object_log.delete(entry)
      Maglev.commit_transaction if commit
    end
  end

  # ObjectLogEntries are objects that can be inserted into a distributed,
  # persistent queue for later retrieval.  They are typically used as an aid
  # to debugging.  The size of the object log is limited by disk.
  #
  # A typical scenario is to log suspicious objects to the log during a run,
  # and then later login from some other VM to inspect the object log.  E.g.,
  # in your buggy code do:
  #
  #     if strange_condition_holds(some_object)
  #       ObjectLogEntry.debug("This is a strange one", some_object).add_to_log
  #       Maglev.commit_transaction  # or rely on some other mechanism to commit...
  #     end
  #
  # This will log your strange objects.  All of the object log entries will
  # be held persistently by the repository, and you can view them at your
  # leisure:
  #
  #     # Get a static view of the object log entries
  #     view = ObjectLogEntry.object_log
  #
  #     p view   # display them (but you'd want to do this more intelligently,
  #              # as there may hundreds of entries, thousands of entries, millions
  #              # and billions and trillions of entries....
  #
  # Each ObjectLogEntry has
  # * priority
  # * tag
  # * timestamp
  # * label
  # * object
  # * pid
  #
  # For each priority, there are class side methods to get the priority number and
  # create an <tt>ObjectLogEntry</tt> object.  E.g., for warn, there are:
  #
  #     warn                        Returns 3
  #
  #     warn: aString               Creates an ObjectLogEntry with a level of
  #                                 warn and a message of aString
  #
  #     warn: aString object: obj   Creates an ObjectLogEntry with a level of
  #                                 warn, a message of aString and saves obj
  #                                 with the entry

  # The priorities are:
  # * trace 6
  # * debug 5
  # * info  4
  # * warn  3
  # * error 2
  # * fatal 1
  class ObjectLogEntry
    # Returns the object log: an OrderedCollection of ObjectLogEntries.  The caller
    # is expected to abort, acquire lock and commit if necessary.
    class_primitive 'object_log', 'objectLog'

    # Return the level assigned for trace entries (6)
    class_primitive 'trace_level', 'trace'
    class_primitive '_trace', 'trace:object:'
    # Create a trace priority object log entry with the given message and
    # object.
    def self.trace(msg, obj=nil)
      _trace(msg, obj)
    end

    # Return the level assigned for debug entries (5)
    class_primitive 'debug_level', 'debug'
    class_primitive '_debug', 'debug:object:'
    # Create a debug priority object log entry with the given message and
    # object.
    def self.debug(msg, obj=nil)
      _debug(msg, obj)
    end

    # Return the level assigned for info entries (4)
    class_primitive 'info_level', 'info'
    class_primitive '_info', 'info:object:'
    # Create a info priority object log entry with the given message and
    # object.
    def self.info(msg, obj=nil)
      _info(msg, obj)
    end

    # Return the level assigned for warn entries (3)
    class_primitive 'warn_level', 'warn'
    class_primitive '_warn', 'warn:object:'
    # Create a warn priority object log entry with the given message and
    # object.
    def self.warn(msg, obj=nil)
      _warn(msg, obj)
    end

    # Return the level assigned for error entries (2)
    class_primitive 'error_level', 'error'
    class_primitive '_error', 'error:object:'
    # Create a error priority object log entry with the given message and
    # object.
    def self.error(msg, obj=nil)
      _error(msg, obj)
    end

    # Return the level assigned for fatal entries (1)
    class_primitive 'fatal_level', 'fatal'
    class_primitive '_fatal', 'fatal:object:'
    # Create a fatal priority object log entry with the given message and
    # object.
    def self.fatal(msg, obj=nil)
      _fatal(msg, obj)
    end

    # Insert the receiver into the global object log.
    primitive_nobridge 'add_to_log', 'addToLog'

    # Return receiver's object attribute
    primitive_nobridge 'object', 'object'

    # Return receiver's priority attribute
    primitive_nobridge 'priority', 'priority'

    # Return receiver's pid attribute
    primitive_nobridge 'pid', 'pid'

    primitive_nobridge 'has_cc?', 'hasContinuation'

    primitive_nobridge '_timestamp', 'stamp'  # the timestamp is held as stamp
    # Return receiver's timestamp attribute
    def timestamp
      _timestamp.as_time
    end

    # Return true if the reciever has been tagged
    primitive_nobridge 'tagged?', 'hasTag'
    primitive_nobridge 'tag', 'tag'

    # Return receiver's label attribute
    primitive_nobridge 'label', 'label'

    def to_s
      inspect
    end
  end

  class DebuggerLogEntry < ObjectLogEntry
    # Creates a new continuation at this point and stores in the object log
    # Uses the Ruby variant, that converts the created continuation into a
    # persistable version
    class_primitive 'create_continuation_labeled', 'rubyCreateContinuationLabeled:'
    primitive 'continuation', 'continuation'
    primitive 'thread', 'continuation'
    primitive 'exception', 'object'
    primitive 'label_object', 'label:object:'

    # Readies the continuation for resume. nils the instance variable, removing
    # the thread from the stone
    def resume_continuation
      t = self.continuation
      t.convert_to_runnable_state
      self.continuation = nil
      t.resume_from_continuation
    end

    def debug_continuation
      t = self.continuation
      t.convert_to_runnable_state
      self.continuation = nil
      t.resume_from_continuation(:debug)
    end

    def stop_continuation
      t = self.continuation
      t.convert_to_runnable_state
      self.continuation = nil
      t.resume_from_continuation(:stop)
    end

    private
    primitive 'continuation=', 'continuation:'
  end
end
