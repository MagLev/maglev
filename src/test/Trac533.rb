# Thread-ring test from
# http://timetobleed.com/fixing-threads-in-ruby-18-a-2-10x-performance-boost/
# invoke with `maglev-ruby Trac533.rb 400`
# for timing test invoke with larger param e.g. `maglev-ruby Trac533.rb 100000`

require 'thread'
THREAD_NUM = 403
number = ARGV.first.to_i
if number == 0
  number = 400
end

threads = []
for i in 1..THREAD_NUM
   threads << Thread.new(i) do |thr_num|
      while true
         Thread.stop
         if number > 0
            number -= 1
         else
            puts thr_num
            exit 0
         end
      end
   end
end

prev_thread = threads.last
while true
   for thread in threads
      Thread.pass until prev_thread.stop?
      thread.run
      prev_thread = thread
   end
end
#################### Trac Info
# ID:         533
# Summary:    maglev-ruby infinite loop in thread test
# Changetime: 2009-06-08 16:49:17+00:00
###

#  Maglev loops on this simple thread test from http://tinyurl.com/qyl3rq. I'll create a test file and check it in.
#  
#  Maglev outputs this:
#  {{{
#  $ maglev-ruby thread_ring.rb 400
#  [...]
#  [...]
#  [...]
#  [...]
#  [...]
#  [...]
#  [...]
#  }}}
#  
#  Should output this:
#  {{{
#  $ ruby thread_ring.rb 400
#  401
#  }}}
#  