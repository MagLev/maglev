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

module Maglev
  # MaglevException            # defined in GlobalErrors.rb
  # NotPersistableException    # defined in GlobalErrors.rb
  # OutsideOfTransactionException    # defined in GlobalErrors.rb
  # CommitFailedException      # defined in GlobalErrors.rb

  PERSISTENT_ROOT = Hash.new

  def transient(&block)
    # Newly defined modules/classes will be marked as transient
    #  and their names will be stored as transient constants in parent module.
    # All assignments to constants in existing modules are transient.
    # All method definitions added to transient method dictionaries.
    # All methods defined via  Object#extend or Module#include
    #   will be transient even if target class is persistable.
    # Constant removal that finds constant in a persistent module will raise exception.
    # Method removal that finds method in a persistent module will raise exception.
    rctx = RubyContext
    save_pm = rctx.persistence_mode
    begin
      rctx.persistence_mode=(false)
      yield
    ensure
      rctx.persistence_mode=(save_pm)
    end
  end

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
    begin
      rctx.persistence_mode=(true)
      rctx.persistable_instances=(persistable_instances)
      yield
    ensure
      rctx.persistence_mode=(save_pm)
      rctx.persistable_instances=(save_pinst)
    end
  end

  # Returns true iff the VM is currently in persistent mode.
  def persistent?
    RubyContext.persistence_mode
  end

  # Returns true iff the VM is currently in transient mode.
  def transient?
    not RubyContext.persistence_mode
  end

  module_function :transient, :persistent, :transient?, :persistent?
end

module Maglev
  class CommitFailedException
    def transaction_conflicts
      raise NotImplementedError
    end

    # TODO: Should we have something simple like this to handle the "Note"
    # at the end of the description for transaction_conflicts?
    #
    # Disconnect (ignore) all conflicts
    def disconnect_conflicts
      raise NotImplementedError
    end
  end

  def commit_transaction
    # TODO: wrap #rtErrPrimOutsideTrans in OutsideOfTransactionException
    unless Gemstone.commitTransaction
      raise CommitFailedException
    end
    return true
  end

  def abort_transaction
    return Gemstone.abortTransaction
  end

  module_function :commit_transaction, :abort_transaction
end
