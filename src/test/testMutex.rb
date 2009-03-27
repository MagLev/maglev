
class TestMutex
 M = Mutex.new
end

Gemstone.commitTransaction

TestMutex::M.synchronize { puts "hello" }

x = TestMutex::M
nil.pause
