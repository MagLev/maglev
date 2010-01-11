# The transient method dictionaries don't seem to be saved when
# Module#maglev_persistable is called.
#
# TO REPRODUCE:
#
# 1: Run this file:  $ maglev-ruby src/test/TracXXX.rb
#    Notice that we can call the hello method
#
# 2: Attempt to create a bar object and invoke hello from a different VM:
#      $ maglev-ruby -e 'Maglev::PERSISTENT_ROOT[:klass1].new.hello'
#
class Bar
  def hello
    puts "Hello from Bar"
  end
end
Bar.maglev_persistable
Maglev::PERSISTENT_ROOT[:klass1] = Bar
Maglev.commit_transaction

# In the current VM, we have the appropriate method defined:
Maglev::PERSISTENT_ROOT[:klass1].new.hello
