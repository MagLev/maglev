class Test498 < Module

    def initialize(rec)
        puts "Receiver #{rec}"
    end

end
t = Test498.new("This is a test")
true
#################### Trac Info
# ID:         498
# Summary:    Too many args signaled when calling new with initialize having one arg and called with one
# Changetime: 2009-04-29 16:42:14+00:00
###

#  The attached stack and frames shows ProxyModule.new  called with one arg, and the initialize for it takeing one arg, but signaling too marny args.