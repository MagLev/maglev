module LA
  module SA
    module PA
    end
    def pa_test
puts "In pa_test"
    end
  end
end

src = "module Less
  module StyleSheet
    module Primary0
    end
    def pri_test
puts 'In pri_test'
    end
  end
end
"
Object.class_eval(src)

class CA
  include LA::SA
end
class CL
  include Less::StyleSheet
end
CA.new.pa_test

CL.new.pri_test
true
