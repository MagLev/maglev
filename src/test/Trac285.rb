require File.expand_path('simple', File.dirname(__FILE__))

# From Webrick/URI

class Generic
  def takes_eleven(one, two, three, four, five, six, seven, eight, nine, ten, eleven)
    11
  end
  def takes_ten(one, two, three, four, five, six, seven, eight, nine, ten)
    10
  end
  def takes_nine(one, two, three, four, five, six, seven, eight, nine)
    9
  end
  def takes_eight(one, two, three, four, five, six, seven, eight)
    8
  end
  def takes_seven(one, two, three, four, five, six, seven)
    7
  end
  def takes_six(one, two, three, four, five, six)
    6
  end
  def takes_five(one, two, three, four, five)
    5
  end
  def takes_four(one, two, three, four)
    4
  end
  def takes_three(one, two, three)
    3
  end
  def takes_two(one, two)
    2
  end
  def takes_one(one)
    1
  end
end

class HTTP < Generic
  def takes_one(*arg);    super(*arg); end
  def takes_two(*arg);    super(*arg); end
  def takes_three(*arg);  super(*arg); end
  def takes_four(*arg);   super(*arg); end
  def takes_five(*arg);   super(*arg); end
  def takes_six(*arg);    super(*arg); end
  def takes_seven(*arg);  super(*arg); end
  def takes_eight(*arg);  super(*arg); end
  def takes_nine(*arg);   super(*arg); end
  def takes_ten(*arg);    super(*arg); end
  def takes_eleven(*arg); super(*arg); end
end

eleven_args = %w(one two three four five six seven eight nine ten eleven)
ten_args    = %w(one two three four five six seven eight nine ten)
nine_args   = %w(one two three four five six seven eight nine)
eight_args  = %w(one two three four five six seven eight)
seven_args  = %w(one two three four five six seven)
six_args    = %w(one two three four five six)
five_args   = %w(one two three four five)
four_args   = %w(one two three four)
three_args  = %w(one two three)
two_args    = %w(one two)
one_args    = %w(one)


# takes_one works....
test(HTTP.new.takes_one(*one_args),        1, "test many args one")

# but the rest fail
test(HTTP.new.takes_two(*two_args),        2, "test many args two")
test(HTTP.new.takes_three(*three_args),    3, "test many args three")
test(HTTP.new.takes_four(*four_args),      4, "test many args four")
test(HTTP.new.takes_five(*five_args),      5, "test many args five")
test(HTTP.new.takes_six(*six_args),        6, "test many args six")
test(HTTP.new.takes_seven(*seven_args),    7, "test many args seven")
test(HTTP.new.takes_eight(*eight_args),    8, "test many args eight")
test(HTTP.new.takes_nine(*nine_args),      9, "test many args nine")
test(HTTP.new.takes_ten(*ten_args),       10, "test many args ten")
test(HTTP.new.takes_eleven(*eleven_args), 11, "test many args eleven")

report
