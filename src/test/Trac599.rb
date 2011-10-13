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
#################### Trac Info
# ID:         599
# Summary:    class variables not persisted
# Changetime: 2009-09-08 18:34:17+00:00
###

#  From rubygems:
#  
#  
#  {{{
#  Maglev.persistent do
#    class S
#      @@foo = 22
#      def self.foo
#        @@foo
#      end
#    end
#  end
#  Maglev.commit_transaction
#  p S.foo  # This works
#  }}}
#  
#  The file runs and is able to print @@foo.  But subsequent VMs can't access @@foo:
#  
#  
#  {{{
#  $ maglev-ruby -e 'p S.foo'
#  -----------------------------------------------------
#  GemStone: Error         Nonfatal
#  Error, 'undefined class variable @@foo'
#  Error Category: 231169 [GemStone] Number: 2023 Arg Count: 1 Context : 207938305
#  Arg 1: [207938049 sz:30 cls: 74753 String] undefined class variable @@foo
#  }}}
#  
#  