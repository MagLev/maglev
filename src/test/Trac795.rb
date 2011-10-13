
def go filename
  # assume ARGV[0] is a log
  start_time = nil
  float = /[\d]+\.[\d]+/
  start_regex = /(#{float}).*starting up logger/
  end_regex = /(#{float}).*DONE WITH WHOLE FILE/

=begin rubydoctest
rubydoctest: can read
>> '3.3' =~ float
=> 0
>> '83.805[146]starting up logger' =~ start_regex
=> 0
>> $1
=> '83.805'
>> '83.806[146]hello DONE WITH WHOLE FILE' =~ end_regex
=> 0
>> $1
=> '83.806'
=end
  starty = nil
  endy = nil
  File.read(filename).each_line {|line|
    if !starty && line =~ start_regex
      puts line if $VERBOSE
      starty = $1.to_f
    else
      begin
       if line =~ end_regex # pesky encoding errors
        puts line if $VERBOSE
        endy = $1.to_f
        break # break out of enumerator early
       end
      rescue => e # encoding error...
               puts line, line.inspect
               sleep 5
      end
    end
  } unless File.directory? filename
  if endy && starty
    puts endy - starty
    endy - starty
  else
          nil
  end

end

[5].map do |n|
  n.times { go ENV['MAGLEV_HOME'] + '/src/test/peer_log.txt' }
end
#################### Trac Info
# ID:         795
# Summary:    Syntax error in benchmarks/macro-benchmarks/bm_parse_log.rb
# Changetime: 2010-09-30 20:50:40+00:00
###

#  Works in MRI, breaks in MagLev
#  
#  A test that doesn't require the benchmark framework is in: /export/backup/users/maglev/baseline/raw_benchmarks/macro-benchmarks/
#  
#  The two files you need are:
#  test_parse_log.rb
#  peer_log.txt
#  
#  I'll create a test case after I know the ticket number.
#  
#  {{{
#  $ maglev-ruby test_parse_log.rb 
#  embedded document meets end of file
#  syntax error
#  #<SyntaxError: embedded document meets end of file, near line 52>
#  ERROR 2023, Error, 'embedded document meets end of file, near line 52' (SyntaxError)
#  }}}
#  