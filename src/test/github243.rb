#https://github.com/MagLev/maglev/pull/243
#
#TransactionError should be accessible in ruby

object = nil
Maglev.abort_transaction
Maglev.transient do
  class X
  end
  Maglev::PERSISTENT_ROOT[:class] = X
end
begin
  Maglev.commit_transaction
rescue TransactionError => error
  object = error.object
end
raise "TransactionError and its method object should be accessible in ruby" unless object == X


