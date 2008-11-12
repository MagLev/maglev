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
end

m = URI::MailTo.new  
s = URI::MailTo::MBP
unless s = 'abc'
  raise 'ERR'
end
true
