#! /bin/bash

# Let us know we're running tests
touch benchmarks-started

# First, execute the scrips for each implementation to be tested
maglev-ruby ./run_benchmarks-MagLev.rb 1>benchmark_results-Maglev.out 2>&1
$JRUBY_HOME/jruby -J-Xss1024m -J-Xmn512m -J-Xms2048m -J-Xmx2048m -J-server ./run_benchmarks-JRuby.rb 1>benchmark_results-JRuby.out 2>&1
/usr/local/ruby1.9/bin/ruby ./run_benchmarks-MRI-19.rb 1>benchmark_results-MRI-19.out 2>&1 
/usr/local/ruby1.8.7/bin/ruby ./run_benchmarks-MRI-187p72.rb 1>benchmark_results-MRI-187p72.out 2>&1 
/usr/local/ruby1.8.6/bin/ruby ./run_benchmarks-MRI-186p287.rb 1>benchmark_results-MRI-186p287.out 2>&1 

# Let us know we're done with the actual tests so we can start using the machine again
touch benchmarks-completed

# Second, extract the times
strings benchmark_results-JRuby.out  | tr -d '*' | awk -f extractTimes.awk | cut -f1,2 > JRuby1.1.6.times
strings benchmark_results-Maglev.out  | tr -d '*' | awk -f extractTimes.awk | cut -f1,2 > MagLev.times
strings benchmark_results-MRI-186p287.out  | tr -d '*' | awk -f extractTimes.awk | cut -f1,2  > MRI-186p287.times
strings benchmark_results-MRI-187p72.out  | tr -d '*' | awk -f extractTimes.awk | cut -f1,2  > MRI-187p72.times
strings benchmark_results-MRI-19.out  | tr -d '*' | awk -f extractTimes.awk | cut -f1,2 > MRI-19.times

# Finally, make into spreadsheet form
join -a 1 -o 1.1,2.2 -e NOT_RUN benchmark_names MRI-186p287.times \
| join -a 1 -o 1.1,1.2,2.2 -e NOT_RUN - MRI-187p72.times \
| join -a 1 -o 1.1,1.2,1.3,2.2 -e NOT_RUN - JRuby1.1.6.times \
| join -a 1 -o 1.1,1.2,1.3,1.4,2.2 -e NOT_RUN - MRI-19.times \
| join -a 1 -o 1.1,1.2,1.3,1.4,1.5,2.2 -e NOT_RUN - MagLev.times > AllBenchmarkTimes.csv

