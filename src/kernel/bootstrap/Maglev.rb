#--
# A quick guide to the Transient / Persistent semantics:
#
#  |-------------+----------------------------+----------------------------|
#  |             | Maglev.persistent          | Maglev.transient           |
#  |-------------+----------------------------+----------------------------|
#  | Persistent  | * Constant -> both         | * Constant -> transient    |
#  | receiver    | * method -> both           | * method -> session        |
#  |-------------+----------------------------+----------------------------|
#  | Transient   | * Constant -> transient    | * Constant -> transient    |
#  | receiver    | * method -> session        | * method -> session        |
#  |-------------+----------------------------+----------------------------|
#  | New Class   | * Class marked persistent  | * Class marked transient   |
#  |             | * Name: according to rules | * Name: according to rules |
#  |             | of parent namespace        | of parent namespace        |
#  |-------------+----------------------------+----------------------------|
#++

# The Maglev module contains MagLev specific APIs.  Most MagLev specific
# features are rooted under this module, but a few features are placed
# elsewhere (e.g., in +Module+).
#
# Note that eigenclasses (singleton classes) are always marked persistable
# and are always marked to allow instances to be persisted.  Eigenclasses
# are persisted when their corresponding Ruby object is persisted (by
# reachability).
module Maglev
  # MaglevException                  # defined in GlobalErrors.rb
  # NotPersistableException          # defined in GlobalErrors.rb
  # OutsideOfTransactionException    # defined in GlobalErrors.rb
  # CommitFailedException            # defined in GlobalErrors.rb


  #-- ##################################################
  #   PERSISTENCE API
  #++ ##################################################


  # A Hash that is the root for persistent objects.  In a fresh repository,
  # this is initialized with an empty Ruby Hash.  After the first commit,
  # the contents of this Hash will be refreshed from the repository at each
  # <tt>Maglev.abort_transaction</tt>, <tt>Maglev.commit_transaction</tt>
  # and at VM startup.
  unless defined?( PERSISTENT_ROOT )
    # if PERSISTENT_ROOT is defined, we are just reloading bootstrap methods
    # otherwise it was deleted by RubyContext(C)>>reset .
    PERSISTENT_ROOT = Hash.new
  end

  def root
    PERSISTENT_ROOT
  end
  module_function :root

  # Executes the block with the current thread in transient mode, which
  # affects the following operations:
  #
  # 1. All newly defined modules and classes will be marked as transient,
  #    and their names will be registered under the transient slot of the
  #    appropriate namespace (i.e., the constants that refer to the new
  #    classes will not be written to the repository during a
  #    <tt>Maglev.commit_transaction</tt>).
  #
  # 2. All assignments to constants will happen in the transient namespace,
  #    even for persistent modules and classes.
  #
  # 3. All method definitions in re-opened classes and modules will be
  #    placed into the transient dictionaries and not be available for
  #    persistence (but see <tt>Module#maglev_persistable=</tt>).
  #
  # 4. All methods defined via <tt>Object#extend</tt> or
  #    <tt>Module#include</tt> will be done transiently, even if the target
  #    class or module is marked persistable.
  #
  # If the thread is already in transient mode, the block is executed and
  # the thread remains in transient mode (i.e., a no-op).
  #
  # == Example
  #
  # Suppose there already exist a persistent module named +Persistent+, and
  # a transient module named +Transient+.  We then run the following code:
  #
  #   Maglev.transient do
  #
  #     module Persistent   # a previously defined, persistent module
  #       class C           # A brand new class
  #         A_CONST = 42
  #         def foo
  #         end
  #       end
  #     end
  #
  #     Maglev::PERSISTENT_ROOT['foo'] = Persistent::C
  #
  #     Persistent::X = 42
  #     Transient::X  = 42
  #
  #   end
  #
  #   Maglev.commit_transaction
  #
  # After the code runs, the following statements hold:
  #
  # 1. Module +Persistent+ is still persisted in the repository, and module
  #    +Transient+ is still local to this VM.
  #
  # 2. <tt>Persistent::C</tt> is a constant in <tt>Persistent</tt>'s transient
  #    namespace, so will not have been written to the repository.  The
  #    current VM will still see <tt>Persistent::C</tt>.
  #
  # 3. <tt>Persistent::C</tt> is a class that is itself not persistable,
  #    nor are its instances persistable.
  #
  # 4. All methods and constants in <tt>Persistent::C</tt>,
  #    (<tt>Persistent::C::A_CONST</tt> and <tt>Persistent::C#foo</tt> will
  #    be lost (as will +C+) when the VM shuts down.
  #
  # 5. <tt>Maglev::PERSISTENT_ROOT['foo'] = Persistent::C</tt> will raise an
  #    exception at commit time (since +C+ is not persistable).
  #
  # 6. For <tt>Transient::X = 42</tt>, both the constant and its value are
  #    visible to the current VM, but not saved in the repository.
  #
  # 7. For <tt>Persistent::X = 42</tt>, both the constant +X+ and its
  #    current value (42) are visible to the current VM.  The repository
  #    will see the constant <tt>Persistent::X</tt> only if it was in the
  #    repository prior to the commit, and if it was, the repository will
  #    see the previous value.
  #
  # Calls to +transient+ may be nested inside other calls to +transient+
  # and calls to +persistent+.
  #
  # Returns the value of executing the block.
  #
  def transient(&block)
    #--
    # Newly defined modules/classes will be marked as transient
    #  and their names will be stored as transient constants in parent module.
    # All assignments to constants in existing modules are transient.
    # All method definitions added to transient method dictionaries.
    # All methods defined via  Object#extend or Module#include
    #   will be transient even if target class is persistable.
    # Constant removal that finds constant in a persistent module will raise exception.
    # Method removal that finds method in a persistent module will raise exception.
    #++
    rctx = RubyContext
    save_pm = rctx.persistence_mode
    res = nil
    begin
      rctx.persistence_mode=(false)
      res = yield
    ensure
      rctx.persistence_mode=(save_pm)
    end
    res
  end

  # Executes the block with the current thread in persistent mode, which
  # affects the following operations:
  #
  # 1. All newly defined modules and classes will be marked as persistable,
  #    and their names will be registered under the rules for the
  #    appropriate namespace (i.e., if the parent namespace is persistent,
  #    then the constant will be staged for persistence; if the parent
  #    namespace is transient, the constant reference will not be visible
  #    to the persistent store).
  #
  # 2. For new class definitions, if the <tt>persistable_instances</tt>
  #    flag is set to +true+, then the VM will also mark the class so that
  #    instances of that class may also be persisted.  If the flag is
  #    +false+, then the class, but not its instances, will be persistable
  #    (this allows you to load and commit library code for performance).
  #
  # 3. All assignments to constants in persistent classes and modules will
  #    be seen by the repository at the next
  #    <tt>Maglev.commit_transaction</tt>.  Any assignment to constants in
  #    transient classes and modules will be visible in the current VM, but
  #    will not be saved to the repository.
  #
  # 4. All method definitions in re-opened persistent classes and modules
  #    will be placed into both the transient and persistent method
  #    dictionaries and will be available for persistence.  It does not
  #    matter how the method is defined (e.g., <tt>define_method</tt>,
  #    <tt>module_eval</tt>, etc.).
  #
  #    All method definitions in re-opened transient classes and modules
  #    will be available only to the current VM and will not be persisted
  #    (i.e., defining a method on a transient class during persistent mode
  #    does not change the class to be persistent).
  #
  # 5. All methods defined via <tt>Object#extend</tt> or
  #    <tt>Module#include</tt> will be done persistently.
  #
  # 6. When the persistent_mode transitions from true to false,
  #    the current transient $LOAD_PATH (i.e. $: )  is copied
  #    to persistent state.  See also discussion of $LOADED_FEATURES below.
  #    At session startup, the transient $LOAD_PATH is initialized with
  #    a copy of the persistent $LOAD_PATH .
  #
  # If the current thread is already in persistent mode, the block is
  # executed and the thread remains in persistent mode (i.e., a no-op).
  #
  # == Example
  #
  # Suppose there already exist a persistent module named +Persistent+, and
  # a transient module named +Transient+.  We then run the following code:
  #
  #   Maglev.persistent do
  #
  #     module Persistent   # a previously defined, persistent module
  #       class C           # A brand new class
  #         A_CONST = 42
  #         def foo
  #         end
  #       end
  #     end
  #
  #     Maglev::PERSISTENT_ROOT['foo'] = Persistent::C
  #
  #     Persistent::X = 42
  #     Transient::X  = 42
  #
  #   end
  #
  #   Maglev.commit_transaction
  #
  # After the code runs, the following statements hold:
  #
  # 1. Module +Persistent+ is still persisted in the repository, and module
  #    +Transient+ is still local to this VM.
  #
  # 2. <tt>Persistent::C</tt> is a constant in <tt>Persistent</tt>'s persistent
  #    namespace, so will have been written to the repository.
  #
  # 3. <tt>Persistent::C</tt> is a class that is itself persistable, and
  #    its instances are also persistable.
  #
  # 4. All methods and constants in <tt>Persistent::C</tt>,
  #    (<tt>Persistent::C::A_CONST</tt> and <tt>Persistent::C#foo</tt> will
  #    be saved to the repository.
  #
  # 5. <tt>Maglev::PERSISTENT_ROOT['foo'] = Persistent::C</tt> will be
  #    committed to the repository and be available to all VMs.
  #
  # 6. For <tt>Transient::X = 42</tt>, both the constant and its value are
  #    visible to the current VM, but not saved in the repository.
  #
  # 7. For <tt>Persistent::X = 42</tt>, both the constant +X+ and its
  #    current value (42) are visible to the current VM and saved in the
  #    repository.
  #
  # Calls to +persistent+ may be nested inside calls to +transient+ and
  # other calls to +persistent+.
  #
  # Returns the value from executing block.
  #
  def persistent(persistable_instances=true, &block)
    # Newly defined modules/classes will be marked as persistable,
    #   and their names stored in persistable parent's persistent name space,
    #   or stored in transient parent's transient name space .
    # All assignments/removals of constants in existing persistent modules
    #   are stored to both transient and peristent name space of module.
    # All assignments to constants in existing transient modules are transient.
    # All method definitions/removals in re-opened persistent modules
    #   stored to both transient and peristent method dictionaries.
    # All method definitions in re-opened transient modules are transient.
    # All methods defined via  Object#extend or Module#include
    #   will be persistent if target class is persistable.
    # The persistable_instances arg controls setting of corresponding
    #   flag in newly created classes.

    rctx = RubyContext
    save_pm = rctx.persistence_mode
    save_pinst = rctx.persistable_instances
    res = nil
    begin
      rctx.persistence_mode=(true)
      rctx.persistable_instances=(persistable_instances)
      res = yield
    ensure
      rctx.persistence_mode=(save_pm)
      rctx.persistable_instances=(save_pinst)
    end
    res
  end

  # Returns true if the current thread is in persistent mode.
  def persistent?
    RubyContext.persistence_mode
  end

  # Returns true if the current thread is in transient mode.
  def transient?
    not RubyContext.persistence_mode
  end

  module_function :transient, :persistent, :transient?, :persistent?


  #-- ##################################################
  #   TRANSACTION API
  #++ ##################################################


  # This exception is raised if MagLev is unable to commit the state of the
  # repository.  The details of why the commit failed are contained in the
  # exception.
  class CommitFailedException
    # +Raises NotImplementedError+.  When implemented, this method will
    # provide the details on the conflicts.
    def transaction_conflicts
      raise NotImplementedError, "CommitFailedException#transaction_conflicts"
    end

    # Raises +NotImplementedError+.  When implemented, this method will
    # disconnect (ignore) all conflicts.
    def disconnect_conflicts
      raise NotImplementedError, "CommitFailedException#disconnect_conflicts"
    end
  end

  # Attempts to update the persistent state of the repository to include
  # changes made by this transaction.
  #
  # If the commit operation succeeds, then this method returns true, and
  # the current transaction's changes, if any, become a part of the
  # persistent repository.  After the repository update, the session exits
  # the current transaction.  If the transaction mode is
  # <tt>:auto_begin</tt> (the MagLev default), then the session enters a
  # new transaction.  If the transaction mode is <tt>:manual_begin</tt>,
  # then the session remains outside of a transaction.
  #
  # If conflicts prevent the repository update, then this method raises a
  # +CommitFailedException+ which contains details of why the commit
  # failed.  Call the <tt>transaction_conflicts</tt> method to determine
  # the nature of the conflicts.  If the session is outside of a
  # transaction, then this method raises the error
  # +OutsideOfTransactionException+.
  #
  # This method also updates the session's view of GemStone.  If the commit
  # operation succeeds, then all objects in the session's view are
  # consistent with the current state of GemStone.  If the commit fails,
  # then this method retains all the changes that were made to objects
  # within the current transaction.  However, commits made by other
  # sessions are visible to the extent that changes in this transaction do
  # not conflict with them.
  #
  # Returns true if commit was read-only or succeeded.  Raises
  # +OutsideOfTransactionException+ if there was a failure.
  def commit_transaction
    # TODO: wrap #rtErrPrimOutsideTrans in OutsideOfTransactionException
    unless System.commit_transaction
      raise CommitFailedException
    end
    return true
  end
  alias commit commit_transaction

  # Rolls back all modifications made to committed GemStone objects and
  # provides the session with a new view of the most recently committed
  # GemStone state.
  #
  # These operations are performed whether or not the session was
  # previously in a transaction.  If the transaction mode is set to
  # <tt>:auto_begin</tt>, then a new transaction is started.  If the
  # transaction mode is set to <tt>:manual_begin</tt>, then a new
  # transaction is not started.
  def abort_transaction
    return System.abort_transaction
  end
  alias abort abort_transaction

  # Starts a new transaction for the session.  An abort is done before
  # the new transaction is started - giving the session a new snapshot of
  # the repository.
  #
  # If any permanent objects had been written by the session, their state
  # is aborted.  This method returns nil.  def begin_transaction
  def begin_transaction
    return System.begin_transaction
  end

  # Enter a new nested transaction.
  # If session is outside of a transaction, equivalent to beginTransaction.
  # Signals a ImproperOperation exception if the begin would exceed
  # 16 levels of nested transactions.
  def self.begin_nested_transaction
    return System.begin_nested_transaction
  end

  # Attempt to commit the transaction for the current session.
  #
  # This method is the same as 'commit_transaction' except for the
  # handling of locks.  If the commit succeeds, this method releases all
  # locks for the session and returns true.  Otherwise, it returns false
  # and does not release locks.
  #
  # This method also clears the commit release locks and commit-or-abort
  # release locks sets.  See the 'Releasing Locks' method category for
  # more information.
  #
  # Returns true if commit was read-only or succeeded , false if there
  # was a failure.
  def commit_and_release_locks
    return System.commit_and_release_locks
  end

  # Returns 0 if not in a transaction, or a SmallInteger > 0
  # indicating the transaction level. > 1 means a nested
  # transaction.
  def self.transaction_level
    return System.transaction_level
  end

  # $LOADED_FEATURES has a persistent Array and a transient Array .  In
  # ruby code, $LOADED_FEATURES returns the transient Array , and
  # assignment to $LOADED_FEATURES will change the transient Array.  At VM
  # startup, the transient copy is initialized with a copy of the
  # persistent Array A successful 'require' will update the transient
  # Array, and if the session is currently in persistent mode will also
  # update the persistent Array.
  #
  # The method clear_persistent_LOADED_FEATURES will set the persistent
  # Array to empty.
  def clear_persistent_LOADED_FEATURES
    RubyContext.clear_persistent_LOADED_FEATURES
  end

  module_function( :commit, :commit_transaction, :abort, :abort_transaction,
                   :begin_transaction,  :clear_persistent_LOADED_FEATURES,
                   :commit_and_release_locks )

  def __system
    # following ref to Maglev::System can be bound at boot compile
    return System
  end
  module_function( :__system )
end

Maglev.__freeze_constants
