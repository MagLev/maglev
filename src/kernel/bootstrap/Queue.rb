#
#		thread.rb - thread support classes
#			$Date: 2006-12-31 07:02:22 -0800 (Sun, 31 Dec 2006) $
#			by Yukihiro Matsumoto <matz@netlab.co.jp>
#
# Copyright (C) 2001  Yukihiro Matsumoto
# Copyright (C) 2000  Network Applied Communication Laboratory, Inc.
# Copyright (C) 2000  Information-technology Promotion Agency, Japan
#

# 2009, Gemstone file bootstrap/Thread2.rb, 
#  using portions of Rubinius lib/thread.rb

# TODO Thread.abort_on_exception not implemented yet

# Thread defined in Thread.rb
# Mutex defined in Mutex.rb

# ConditionVariable objects augment class Mutex. Using condition variables,
# it is possible to suspend while in the middle of a critical section until a
# resource becomes available.
#
# Example:
#
#   require 'thread'
#
#   mutex = Mutex.new
#   resource = ConditionVariable.new
#   
#   a = Thread.new {
#     mutex.synchronize {
#       # Thread 'a' now needs the resource
#       resource.wait(mutex)
#       # 'a' can now have the resource
#     }
#   }
#   
#   b = Thread.new {
#     mutex.synchronize {
#       # Thread 'b' has finished using the resource
#       resource.signal
#     }
#   }
#
class ConditionVariable
  # ConditionVariable is identically the Smalltalk class TransientSemaphore

  class_primitive 'allocate', 'rubyBasicNew'

  primitive_nobridge '__wait', 'wait'

  primitive_nobridge '__wait_seconds', 'waitForSeconds:'

  # Releases the lock held in +mutex+ and waits; reacquires the lock on wakeup.
  #
  def wait(mutex, timeout=nil)
    mutex.unlock
    if timeout.equal?(nil)
      self.__wait
      was_signaled = true
    else 
      was_signaled = self.__wait_seconds(timeout) # returns false if timed out
    end
    mutex.lock
    if was_signaled
      return self
    else
      return false
    end
  end
  
  #
  # Wakes up the first thread in line waiting for this lock.
  #  Returns the receiver.
  primitive_nobridge 'signal', 'signal'

  #
  # Wakes up all threads waiting for this lock.
  #  Returns the receiver.
  primitive_nobridge 'broadcast', 'signalAll'

  #
  # Returns the number of Threads waiting on the receiver.
  primitive_nobridge 'size', 'size'

end

#
# This class provides a way to synchronize communication between threads.
#
# Example:
#
#   require 'thread'
#   
#   queue = Queue.new
#   
#   producer = Thread.new do
#     5.times do |i|
#       sleep rand(i) # simulate expense
#       queue << i
#       puts "#{i} produced"
#     end
#   end
#   
#   consumer = Thread.new do
#     5.times do |i|
#       value = queue.pop
#       sleep rand(i/2) # simulate expense
#       puts "consumed #{value}"
#     end
#   end
#   
#   consumer.join
#
class Queue
  #
  # Creates a new queue.
  #
  def initialize
    @que = []
    # tainted communication not supported
    # @waiting instVar not needed
    @mutex = Mutex.new
    @resource = ConditionVariable.new  # a smalltalk TransientSemaphore
  end

  #
  # Pushes +obj+ to the queue.
  #
  def push(obj)
    @mutex.synchronize do
      @que.push(obj)
      @resource.signal
    end
  end

  #
  # Alias of push
  #
  alias << push

  #
  # Alias of push
  #
  alias enq push

  #
  # Retrieves data from the queue.  If the queue is empty, the calling thread is
  # suspended until data is pushed onto the queue.  If +non_block+ is true, the
  # thread isn't suspended, and an exception is raised.
  #
  def pop(non_block=false)
    while true
      @mutex.synchronize do
        q = @que
        if q.size.equal?(0)
          raise ThreadError, "queue empty" if non_block
          @resource.wait(@mutex)
        else
          retval = q.shift
          @resource.signal
          return retval
        end
      end
    end
  end

  #
  # Alias of pop
  #
  alias shift pop

  #
  # Alias of pop
  #
  alias deq pop

  #
  # Returns +true+ if the queue is empty.
  #
  def empty?
    @que.size.equal?(0)
  end

  #
  # Removes all objects from the queue.
  #
  def clear
    @que.clear
  end

  #
  # Returns the length of the queue.
  #
  def length
    @que.length
  end

  #
  # Alias of length.
  #
  alias size length

  #
  # Returns the number of threads waiting on the queue.
  #
  def num_waiting
    @resource.size
  end
end

#
# This class represents queues of specified size capacity.  The push operation
# may be blocked if the capacity is full.
#
# See Queue for an example of how a SizedQueue works.
#
class SizedQueue < Queue
  #
  # Creates a fixed-length queue with a maximum size of +max+.
  #
  def initialize(max)
    raise ArgumentError, "queue size must be positive" unless max > 0
    @max = max
    # tainted communication not supported 
    super()
  end

  #
  # Returns the maximum size of the queue.
  #
  def max
    @max
  end

  #
  # Sets the maximum size of the queue.
  #
  def max=(max)
    old_size = 0
    @mutex.synchronize do
      old_size = @max 
      @max = max
    end
    unless old_size == max
      @resource.broadcast
    end
    max
  end

  #
  # Pushes +obj+ to the queue.  If there is no space left in the queue, waits
  # until space becomes available.
  #
  def push(obj)
    while (true)
      @mutex.synchronize do
        q = @que
        if(q.size >= @max)
          @resource.wait(@mutex)
        else
          q.push(obj)
          @resource.signal
          return self
        end
      end      
    end
  end

  #
  # Alias of push
  #
  alias << push

  #
  # Alias of push
  #
  alias enq push

  #
  # Retrieves data from the queue and runs a waiting thread, if any.
  #
  def pop(non_block=false)
    while true
      @mutex.synchronize do
        q = @que
        if q.size.equal?(0)
          raise ThreadError, "queue empty" if non_block
          @resource.wait(@mutex)
        else
          retval = q.shift
          if (q.size < @max)
            @resource.broadcast
          else
            @resource.signal
          end
          return retval
        end
      end
    end
  end

  def pop(*args)
    if args.size > 0
      pop(args[0])
    else
      pop(false)
    end
  end

  #
  # Alias of pop
  #
  alias shift pop

  #
  # Alias of pop
  #
  alias deq pop

  #
  # Returns the number of threads waiting on the queue.
  #
  # num_waiting inherited from Queue
end

# Documentation comments:
#  - How do you make RDoc inherit documentation from superclass?
