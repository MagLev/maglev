
class TestMutex
 M = Mutex.new
end

Maglev.commit_transaction

TestMutex::M.synchronize { puts "hello" }

x = TestMutex::M
