
$count = 0
def test(act, exp, line, warn=false )
  unless act == exp
    warn_str = warn ? "WARNING" : ""
    printf(" line #{line} #{warn_str} expected '#{exp}' actual '#{act}' \n")
    if warn.equal?(false)
      $count += 1
    end
  end
end

test( sprintf("%d", 987) , "987" , __LINE__ )
test( sprintf("%dab", 987) , "987ab" , __LINE__ )
test( sprintf("%5d", 987) , "  987" , __LINE__ )
test( sprintf("%-5d", 987) , "987  " , __LINE__ )
test( sprintf("%05d", 987) , "00987" , __LINE__ )
test( sprintf("%08.5d", 987) , "   00987", __LINE__)
test( sprintf("%05.8d", 987) , "00000987", __LINE__)
test( sprintf("%+010.8x", 123), " +0000007b", __LINE__)
test( sprintf("%#5d", 987) , "  987" , __LINE__ )
test( sprintf("% d", 987) , " 987" , __LINE__ )
test( sprintf("%+5d", 987) , " +987" , __LINE__ )
test( sprintf("%+d", 987) , "+987" , __LINE__ )

test( sprintf("%+05x", -123) , "-007b", __LINE__ )
test( sprintf("%#8x", -123), " 0x..f85", __LINE__)
test( sprintf("% x", -123), "-7b" , __LINE__ )
test( sprintf("%x", -123), "..f85" , __LINE__ )
test( sprintf("%+x", -123), "-7b" , __LINE__ )
test( sprintf("%# x", -123), "-0x7b", __LINE__ )
test( sprintf("%8x", -123), "   ..f85", __LINE__)
test( sprintf("% 8.10x", -123), "-000000007b", __LINE__)
test( sprintf("% 10.8x", -123), " -0000007b", __LINE__)
test( sprintf("% 8x", -123), "     -7b", __LINE__ )

test( sprintf("%d", -987) , "-987" , __LINE__ )
test( sprintf("%dab", -987) , "-987ab" , __LINE__ )
test( sprintf("%5d", -987) , " -987" , __LINE__ )
test( sprintf("%-5d", -987) , "-987 " , __LINE__ )
test( sprintf("%05d", -987) , "-0987" , __LINE__ )
test( sprintf("%#5d", -987) , " -987" , __LINE__ )
test( sprintf("% d", -987) , "-987" , __LINE__ )
test( sprintf("%+5d", -987) , " -987" , __LINE__ )
test( sprintf("%+d", -987) , "-987" , __LINE__ )

test( sprintf("%.5d", 987) , "00987" , __LINE__ )
test( sprintf("%8.5d", 987) ,  "   00987" , __LINE__ )
test( sprintf("%+8.5d", 987) , "  +00987" , __LINE__ )

test( sprintf("%5d %7d yyy", 33, 456), "   33     456 yyy" , __LINE__ )

test( sprintf("%+i", -987) , "-987" , __LINE__ )

test( sprintf("%u", 456) , '456' , __LINE__ )
test( sprintf("%8u", 456) , '     456' , __LINE__ )
test( sprintf("%u", -456) , '..18446744073709551160' , __LINE__ )
test( sprintf("%8u", -456) , '..18446744073709551160' , __LINE__ )

h = Hash.new
h[5]=7
test( sprintf("%p", h ) , '{5=>7}' , __LINE__ )

test( sprintf("%s", h) , '{5=>7}' , __LINE__ )  #Hash.to_s changed in Ruby 1.9

test( sprintf("%10.5s", 'abcdefgh') , '     abcde', __LINE__ )
test( sprintf("%c", 50) , '2' , __LINE__ )

test( sprintf("%b", 0x456), '10001010110' , __LINE__ )
test( sprintf("%#b", 0x456) , '0b10001010110' , __LINE__ )
test( sprintf("%b", -456) ,  '..1000111000' , __LINE__ )
test( sprintf("%#b", -456) ,  '0b..1000111000' , __LINE__ )
test( sprintf("%# x", 123), " 0x7b", __LINE__ )
test( sprintf("%010.8x", 123) , "  0000007b" , __LINE__)

test( sprintf("%b", 0), "0", __LINE__)
test( sprintf("%b", -5), "..1011" , __LINE__)
test( sprintf("%+b", -5), "-101", __LINE__)  # getting '+..1011'

test( sprintf('%o', 465) , '721' , __LINE__ )
test( sprintf('%o', 250000000000000000003) , '33065623267056752000003' , __LINE__ )
test( sprintf('%0o', 465) , '721' , __LINE__ )
test( sprintf('%#o', 465) , '0721' , __LINE__ )

if defined? Maglev
  test( sprintf('%o', -465) , '..7777777777057' , __LINE__ )
  test( sprintf('%#o', -465) , '0..7777777777057' , __LINE__ )
end

test( sprintf("%x", 0x4b6), '4b6', __LINE__ )
test( sprintf("%#x", 0x4b6) , '0x4b6' , __LINE__ )
test( sprintf("%x", -459) ,  '..fe35', __LINE__ )
test( sprintf("%#x", -459) ,  '0x..fe35' , __LINE__ )

test( sprintf("%X", 0x4b6), '4B6' , __LINE__ )
test( sprintf("%#X", 0x4b6) , '0X4B6' , __LINE__ )
test( sprintf("%X", -459) ,  '..FE35' , __LINE__ )
test( sprintf("%#X", -458) ,  '0X..FE36' , __LINE__ )

test( sprintf("%e", 5.5), '5.500000e+00', __LINE__ )
test( sprintf("%E", 5.5) ,'5.500000E+00', __LINE__ )
test( sprintf("%f", 5.5) , "5.500000" , __LINE__ )
test( sprintf("%g", 5.5) , '5.5' , __LINE__ )
test( sprintf("%G", 5.5) , '5.5' , __LINE__ )

test( sprintf("%d", 5.5) , '5' , __LINE__ )
test( sprintf("%g", 5) , '5' , __LINE__ )
test( sprintf("%#g", 5) , '5.00000' , __LINE__ )
test( sprintf("%s", 99), '99' , __LINE__ )

test( sprintf("%+o", 10), "+12", __LINE__ )
test( sprintf("%+o", 0), "+0", __LINE__ )
test( sprintf("%+d", -5), "-5", __LINE__ )
test( sprintf("%+d", 10), "+10", __LINE__ )
test( sprintf("%+d", 0), "+0", __LINE__ )
test( sprintf("%+x", -15), "-f", __LINE__ )
test( sprintf("%+x", 100), "+64", __LINE__ )
test( sprintf("%+x", 0), "+0", __LINE__ )
test( sprintf("%+X", -15), "-F", __LINE__ )
test( sprintf("%+X", 100), "+64", __LINE__ )
test( sprintf("%+X", 0), "+0", __LINE__ )
test( sprintf("=%02X", 1), "=01", __LINE__ )
test( sprintf("%+03d", 0), "+00", __LINE__ )
test( sprintf("%+03d", 5), "+05", __LINE__ )
test( sprintf("%+03d", -5), "-05", __LINE__ )
test( sprintf("%+03d", 12), "+12", __LINE__ )
test( sprintf("%+03d", -12), "-12", __LINE__ )
test( sprintf("%+03d", 123), "+123", __LINE__ )
test( sprintf("%+03d", -123), "-123", __LINE__ )

test( sprintf("%8.4e", 3.7e45), '3.7000e+45' , __LINE__ )
test( sprintf("%8.4e", -3.7e45), '-3.7000e+45' , __LINE__ )
test( sprintf("%8.4E", -3.7e45), '-3.7000E+45' , __LINE__ )
test( sprintf("%8.4g", -3.7e45), '-3.7e+45' , __LINE__ )
test( sprintf("%8.4G", -3.7e45), '-3.7E+45' , __LINE__ )
test( sprintf("%10.4g", 5.5), '       5.5' , __LINE__ )


test( sprintf("%2$d w %1$d z", 33, 456), '456 w 33 z' , __LINE__ )
test( sprintf("%2$*3$d w %1$*4$d z", 33, 456, 4, 5), ' 456 w    33 z' , __LINE__ )

test( sprintf( '  %-*s - %s' , 8 , 'abcd', 'xyz'), '  abcd     - xyz' , __LINE__ )
test( sprintf( "    %*1$2$s  %3$s\n" , -15, "--accessor", "support discontinued"), "    --accessor       support discontinued\n" , __LINE__ )

test( sprintf( "CA %*d CB %*d CC",  6 , 22 , 8, 33 ) , "CA     22 CB       33 CC" , __LINE__ )


# This test is platform dependent.  When the float literal -3.7e45 is
# parsed, MRI prints it and on Macs it gives the wrong answer, presumably
# because the underlying sprintf is different on OS X.  Linux and Solaris
# give the right answer.  Since the use of the parser server via MRI is
# temporary, we just warn OS X users, and eat the exception.
begin
  test( sprintf("%8.4f", -3.7e45), '-3699999999999999771793234396487274551161389056.0000' , __LINE__ , :warn)
end

# Regression: test passes as long as no exception is raised
# MagLev was failing with arg error for zero and one arg variants of printf
a = []
10.times do |i|
  printf(*a)
  a << i.to_s
end

unless $count == 0 ; raise 'fail'; end
true
