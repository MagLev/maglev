#!/bin/bash
# copy tests to RBS benchmark directory i
# rather than save 20MB benchmark directory in our git repo.
cp test_* $MAGLEV_HOME/benchmark/benchmarks/rdoc/
echo "Run tests as follows:"
echo "cd $MAGLEV_HOME/benchmark/benchmarks/rdoc/"
echo "maglev-ruby test_rdoc_against_itself_darkfish.rb"
echo "maglev-ruby test_rdoc_against_itself_ri.rb"
echo "maglev-ruby test_rdoc_core_darkfish.rb"
echo "test_rdoc_core_darkfish.rb will have errors, but should finish"
echo "run it using MRI for comparison"
