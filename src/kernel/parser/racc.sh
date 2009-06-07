#! /bin/csh
#
#  requires that /usr/local/bin/racc  exist
#   from installation of the Ruby  gem  racc 1.4.6  .

mv ruby_parser.rb ruby_parser.rb.old
rm -f parser.racc_out ruby_parser.rb ruby_parser.rb.save

/usr/local/bin/racc -v -t -l -o parser.racc_out ruby_parser.y
if ( $status != 0) then
  echo "Error: NON-ZERO status from racc"
  exit 1
endif
 cat parser.racc_out | \
   sed -e '1,$s+Racc::+MagRp::+' | \
   sed -e '1,$s+racc/parser.rb+kernel/parser/racc_parser.rb+' | \
   sed -e '1,$s+racc_reduce_n+Racc_reduce_n+' | \
   sed -e '1,$s+racc_shift_n+Racc_shift_n+' | \
   sed -e '1,$s+racc_nt_base+Racc_nt_base+' | \
   sed -e '1,$s+racc_use_result_var+Racc_use_result_var+' | \
   sed -e '1,$s+, _values, result)+, vofs)+' > ruby_parser.rb
if ( $status != 0) then
  echo "Error: NON-ZERO status from sed "
  exit 1
endif

cp ruby_parser.rb ruby_parser.rb.save
chmod -w ruby_parser.rb.save
