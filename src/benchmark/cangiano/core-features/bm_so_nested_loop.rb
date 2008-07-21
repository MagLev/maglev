require 'benchmark'
puts '==> Start benchmark: ./core-features/bm_so_nested_loop.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  #!/usr/bin/ruby
  # -*- mode: ruby -*-
  # $Id: nestedloop-ruby.code,v 1.4 2004/11/13 07:42:22 bfulgham Exp $
  # http://www.bagley.org/~doug/shootout/
  # from Avi Bryant
  
  n = 16 # Integer(ARGV.shift || 1)
  x = 0
  n.times do
      n.times do
          n.times do
              n.times do
                  n.times do
                      n.times do
                          x += 1
                      end
                  end
              end
          end
      end
  end
  # puts x
  
  

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./core-features/bm_so_nested_loop.rb'
