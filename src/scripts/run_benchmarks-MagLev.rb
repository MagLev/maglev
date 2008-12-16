# Run modified bm_*.rb files from ruby-benchmark-suite using MagLev
#
# Usage:  
# nohup maglev-ruby ./run_benchmarks-MagLev.rb 1>benchmark_results-Maglev-${HOSTNAME}-`date +%y%m%d`.out 2>&1 &
#
# These benchmarks have been modified to produce timing and status information
# e.g.
#
# ==> Start benchmark: ./core-features/bm_app_answer.rb
#              user     system      total        real
# (any text output by the unmodified benchmark would be here)
#          2.030000   0.260000   2.290000 (  2.316928)
# ==> End benchmark: ./core-features/bm_app_answer.rb
#
# We commented out benchmarks that fail, or that produce huge amounts of output.
# Uncomment them if you want to run them anyway.

# Run the benchmarks from Antonio Cangiano's ruby-benchmark-suite
require '../benchmark/cangiano/core-features/bm_app_answer.rb'
require '../benchmark/cangiano/core-features/bm_app_factorial.rb'
require '../benchmark/cangiano/core-features/bm_app_factorial2.rb'
require '../benchmark/cangiano/core-features/bm_app_fib.rb'
require '../benchmark/cangiano/core-features/bm_app_raise.rb'
require '../benchmark/cangiano/core-features/bm_app_tak.rb'
require '../benchmark/cangiano/core-features/bm_app_tarai.rb'
require '../benchmark/cangiano/core-features/bm_loop_times.rb'
require '../benchmark/cangiano/core-features/bm_loop_whileloop.rb'
require '../benchmark/cangiano/core-features/bm_loop_whileloop2.rb'
require '../benchmark/cangiano/core-features/bm_so_ackermann.rb'
require '../benchmark/cangiano/core-features/bm_so_nested_loop.rb'
require '../benchmark/cangiano/core-features/bm_so_object.rb'
require '../benchmark/cangiano/core-features/bm_so_random.rb'
require '../benchmark/cangiano/core-features/bm_startup.rb'
require '../benchmark/cangiano/core-features/bm_vm1_block.rb'
require '../benchmark/cangiano/core-features/bm_vm1_const.rb'
require '../benchmark/cangiano/core-features/bm_vm1_ensure.rb'
require '../benchmark/cangiano/core-features/bm_vm1_length.rb'
require '../benchmark/cangiano/core-features/bm_vm1_rescue.rb'
require '../benchmark/cangiano/core-features/bm_vm1_simplereturn.rb'
require '../benchmark/cangiano/core-features/bm_vm1_swap.rb'
require '../benchmark/cangiano/core-features/bm_vm2_method.rb'
require '../benchmark/cangiano/core-features/bm_vm2_poly_method.rb'
require '../benchmark/cangiano/core-features/bm_vm2_poly_method_ov.rb'
require '../benchmark/cangiano/core-features/bm_vm2_proc.rb'
require '../benchmark/cangiano/core-features/bm_vm2_send.rb'
require '../benchmark/cangiano/core-features/bm_vm2_super.rb'
require '../benchmark/cangiano/core-features/bm_vm2_unif1.rb'
require '../benchmark/cangiano/core-features/bm_vm2_zsuper.rb'
require '../benchmark/cangiano/core-library/bm_app_strconcat.rb'
require '../benchmark/cangiano/core-library/bm_so_array.rb'
require '../benchmark/cangiano/core-library/bm_so_concatenate.rb'
require '../benchmark/cangiano/core-library/bm_so_count_words.rb'
require '../benchmark/cangiano/core-library/bm_so_exception.rb'
require '../benchmark/cangiano/core-library/bm_so_lists.rb'
require '../benchmark/cangiano/core-library/bm_so_matrix.rb'
require '../benchmark/cangiano/core-library/bm_vm2_array.rb'
require '../benchmark/cangiano/core-library/bm_vm2_regexp.rb'
# require '../benchmark/cangiano/core-library/bm_vm3_thread_create_join.rb' # Unimplemented
require '../benchmark/cangiano/micro-benchmarks/bm_app_pentomino.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_binary_trees.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_fannkuch.rb'
# require '../benchmark/cangiano/micro-benchmarks/bm_fasta.rb' # Too much output
require '../benchmark/cangiano/micro-benchmarks/bm_fractal.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_knucleotide.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_lucas_lehmer.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_mandelbrot.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_mergesort.rb'
# require '../benchmark/cangiano/micro-benchmarks/bm_meteor_contest.rb' # runs mostly
require '../benchmark/cangiano/micro-benchmarks/bm_monte_carlo_pi.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_nbody.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_nsieve.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_nsieve_bits.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_partial_sums.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_quicksort.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_recursive.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_regex_dna.rb'
# require '../benchmark/cangiano/micro-benchmarks/bm_reverse_compliment.rb' # Too much output
require '../benchmark/cangiano/micro-benchmarks/bm_so_sieve.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_spectral_norm.rb'
require '../benchmark/cangiano/micro-benchmarks/bm_sum_file.rb'
# require '../benchmark/cangiano/micro-benchmarks/bm_thread_ring.rb' # Unimplemented
require '../benchmark/cangiano/micro-benchmarks/bm_word_anagrams.rb'
# require '../benchmark/cangiano/real-world/bm_hilbert_matrix.rb' # requires mathn.rb
# require '../benchmark/cangiano/standard-library/bm_app_mandelbrot.rb' # requires complex.rb
