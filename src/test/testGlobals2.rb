# a test case from webrick
module URI
  module REGEXP
    module PATTERN
      ESCAPED = 'abc' 
    end
  end
  class Generic < Object
    include REGEXP
  end
end

module URI
  class MailTo < Generic
    MBP = PATTERN::ESCAPED
  end
  URI_A = 99
  URI_B = 101
end

m = URI::MailTo.new  
s = URI::MailTo::MBP
unless s = 'abc' ; raise 'ERR' ; end
a = URI::URI_A
unless a == 99 ; raise 'err' ; end
b = URI::URI_B
unless b == 101 ; raise 'err' ; end

true
