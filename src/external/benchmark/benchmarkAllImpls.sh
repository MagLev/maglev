#! /bin/bash

# Run new Cangiano ruby-benchmark-suite 
# with MRI 1.8.6, MRI 1.8.7, JRuby 1.1.6, MRI 1.9  and MagLev

# Assumes MagLev server is already running

# Let us know we're running tests
touch benchmarks-started

outfile="AllBenchmarkTimes-${HOSTNAME}-`date +%y%m%d`.csv"
hdr="Benchmark-${HOSTNAME}-`date +%y%m%d`,MRI-186,MRI-187,Jruby-116,MRI-1.9,Fastest,MagLev,x*Fastest,x*MRI-186"

pushd ruby-benchmark-suite
rake run_all RUBY_VM="maglev-ruby" TIMEOUT=600 REPORT="../MagLev-raw.csv"
rake run_all RUBY_VM="$JRUBY_HOME/jruby -J-Xss1024m -J-Xmn512m -J-Xms2048m -J-Xmx2048m -J-server" TIMEOUT=600 REPORT="../JRuby1.1.6-raw.csv"
rake run_all RUBY_VM="/usr/local/ruby1.8.6/bin/ruby" TIMEOUT=600 REPORT="../MRI-186p287-raw.csv"
rake run_all RUBY_VM="/usr/local/ruby1.8.7/bin/ruby" TIMEOUT=600 REPORT="../MRI-187p72-raw.csv"
rake run_all RUBY_VM="/usr/local/ruby1.9/bin/ruby" TIMEOUT=600 REPORT="../MRI-19-raw.csv"
popd

# Let us know we're done with the actual tests so we can start using the machine again
touch benchmarks-completed

# Second, extract the base info
grep bm_ MagLev-raw.csv  | cut -d, -f1,9 | tr "," ";" > MagLev-tmp.csv
grep bm_ JRuby1.1.6-raw.csv  | cut -d, -f1,9 | tr "," ";" > JRuby1.1.6-tmp.csv
grep bm_ MRI-186p287-raw.csv  | cut -d, -f1,9  | tr "," ";" > MRI-186p287-tmp.csv
grep bm_ MRI-187p72-raw.csv  | cut -d, -f1,9  | tr "," ";" > MRI-187p72-tmp.csv
grep bm_ MRI-19-raw.csv  | cut -d, -f1,9 | tr "," ";" > MRI-19-tmp.csv

# Next, extract the times
grep bm_ MagLev-raw.csv  | cut -d, -f7 | paste -d, MagLev-tmp.csv - > MagLev.csv
grep bm_ JRuby1.1.6-raw.csv  | cut -d, -f7 | paste -d, JRuby1.1.6-tmp.csv - > JRuby1.1.6.csv
grep bm_ MRI-186p287-raw.csv  | cut -d, -f7  | paste -d, MRI-186p287-tmp.csv - > MRI-186p287.csv
grep bm_ MRI-187p72-raw.csv  | cut -d, -f7  | paste -d, MRI-187p72-tmp.csv - > MRI-187p72.csv
grep bm_ MRI-19-raw.csv  | cut -d, -f7 | paste -d, MRI-19-tmp.csv - > MRI-19.csv

# Finally, make into spreadsheet form
echo $hdr > $outfile
join -t, -a 1 -o 1.1,2.2 -e NOT_RUN benchmark_names.dat MRI-186p287.csv \
| join -t, -a 1 -o 1.1,1.2,2.2 -e NOT_RUN - MRI-187p72.csv \
| join -t, -a 1 -o 1.1,1.2,1.3,2.2 -e NOT_RUN - JRuby1.1.6.csv \
| join -t, -a 1 -o 1.1,1.2,1.3,1.4,2.2 -e NOT_RUN - MRI-19.csv \
| join -t, -a 1 -o 1.1,1.2,1.3,1.4,1.5,2.2 -e NOT_RUN - benchmark_names.dat \
| join -t, -a 1 -o 1.1,1.2,1.3,1.4,1.5,1.6,2.2 -e NOT_RUN - MagLev.csv \
| join -t, -a 1 -o 1.1,1.2,1.3,1.4,1.5,1.6,1.7,2.3,2.4 -e NOT_RUN - benchmark_names.dat >> $outfile

