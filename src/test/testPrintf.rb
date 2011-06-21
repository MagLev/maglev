
def test(act, exp)
  unless act == exp
    raise 'ERROR'
  end
end

test( sprintf("%d", 987) , "987" )
test( sprintf("%dab", 987) , "987ab" )
test( sprintf("%5d", 987) , "  987" )
test( sprintf("%-5d", 987) , "987  " )
test( sprintf("%05d", 987) , "00987" )
test( sprintf("%#5d", 987) , "  987" )
test( sprintf("% d", 987) , " 987" )
test( sprintf("%+5d", 987) , " +987" )
test( sprintf("%+d", 987) , "+987" )

test( sprintf("%d", -987) , "-987" )
test( sprintf("%dab", -987) , "-987ab" )
test( sprintf("%5d", -987) , " -987" )
test( sprintf("%-5d", -987) , "-987 " )
test( sprintf("%05d", -987) , "-0987" )
test( sprintf("%#5d", -987) , " -987" )
test( sprintf("% d", -987) , "-987" )
test( sprintf("%+5d", -987) , " -987" )
test( sprintf("%+d", -987) , "-987" )

test( sprintf("%.5d", 987) , "00987" )
test( sprintf("%8.5d", 987) ,  "   00987" )
test( sprintf("%+8.5d", 987) , "  +00987" )

test( sprintf("%5d %7d yyy", 33, 456), "   33     456 yyy" )

test( sprintf("%+i", -987) , "-987" )

test( sprintf("%u", 456) , '456' )
test( sprintf("%8u", 456) , '     456' )
test( sprintf("%u", -456) , '..18446744073709551160' )
test( sprintf("%8u", -456) , '..18446744073709551160' )

h = Hash.new
h[5]=7
test( sprintf("%p", h ) , '{5=>7}' )

test( sprintf("%s", h) , '57' )

test( sprintf("%10.5s", 'abcdefgh') , '     abcde')
test( sprintf("%c", 50) , '2' )

test( sprintf("%b", 0x456), '10001010110' )
test( sprintf("%#b", 0x456) , '0b10001010110' )
test( sprintf("%b", -456) ,  '..1000111000' )
test( sprintf("%#b", -456) ,  '0b..1000111000' )

test( sprintf('%o', 465) , '721' )
test( sprintf('%o', 250000000000000000003) , '33065623267056752000003' )
test( sprintf('%0o', 465) , '721' )
test( sprintf('%#o', 465) , '0721' )

if defined? Maglev
  test( sprintf('%o', -465) , '..7777777777057' )
  test( sprintf('%#o', -465) , '0..7777777777057' )
end

test( sprintf("%x", 0x4b6), '4b6')
test( sprintf("%#x", 0x4b6) , '0x4b6' )
test( sprintf("%x", -459) ,  '..fe35')
test( sprintf("%#x", -459) ,  '0x..fe35' )

test( sprintf("%X", 0x4b6), '4B6' )
test( sprintf("%#X", 0x4b6) , '0X4B6' )
test( sprintf("%X", -459) ,  '..FE35' )
test( sprintf("%#X", -458) ,  '0X..FE36' )

test( sprintf("%e", 5.5), '5.500000e+00')
test( sprintf("%E", 5.5) ,'5.500000E+00')
test( sprintf("%f", 5.5) , "5.500000" )
test( sprintf("%g", 5.5) , '5.5' )
test( sprintf("%G", 5.5) , '5.5' )

test( sprintf("%d", 5.5) , '5' )
test( sprintf("%g", 5) , '5' )
test( sprintf("%#g", 5) , '5.00000' )
test( sprintf("%s", 99), '99' )

test( sprintf("%8.4e", 3.7e45), '3.7000e+45' )
test( sprintf("%8.4e", -3.7e45), '-3.7000e+45' )
test( sprintf("%8.4E", -3.7e45), '-3.7000E+45' )
test( sprintf("%8.4g", -3.7e45), '-3.7e+45' )
test( sprintf("%8.4G", -3.7e45), '-3.7E+45' )
test( sprintf("%10.4g", 5.5), '       5.5' )


test( sprintf("%2$d w %1$d z", 33, 456), '456 w 33 z' )
test( sprintf("%2$*3$d w %1$*4$d z", 33, 456, 4, 5), ' 456 w    33 z' )

test( sprintf( '  %-*s - %s' , 8 , 'abcd', 'xyz'), '  abcd     - xyz' )
test( sprintf( "    %*1$2$s  %3$s\n" , -15, "--accessor", "support discontinued"), "    --accessor       support discontinued\n" )


# This test is platform dependent.  When the float literal -3.7e45 is
# parsed, MRI prints it and on Macs it gives the wrong answer, presumably
# because the underlying sprintf is different on OS X.  Linux and Solaris
# give the right answer.  Since the use of the parser server via MRI is
# temporary, we just warn OS X users, and eat the exception.
begin
  test( sprintf("%8.4f", -3.7e45), '-3699999999999999771793234396487274551161389056.0000' )
rescue Exception => ex
  puts "WARN: #{__FILE__}: bug with float precision on this platform: SKIPPING"
end

# Regression: test passes as long as no exception is raised
# MagLev was failing with arg error for zero and one arg variants of printf
a = []
10.times do |i|
  printf(*a)
  a << i.to_s
end

true
