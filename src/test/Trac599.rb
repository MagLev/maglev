# To reproduce the bug, run this file, then run in a separate VM:
#
#   $ maglev-ruby -e 'p S.foo'
#   -----------------------------------------------------
#   GemStone: Error         Nonfatal
#   Error, 'undefined class variable @@foo'
#   Error Category: 231169 [GemStone] Number: 2023 Arg Count: 1 Context : 207938305
#   Arg 1: [207938049 sz:30 cls: 74753 String] undefined class variable @@foo
#
Maglev.persistent do
  class S
    @@foo = 22
    def self.foo
      @@foo
    end
  end
end
Maglev.commit_transaction
p S.foo  # This works
