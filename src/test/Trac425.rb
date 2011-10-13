class D
  class << self
    def once(*ids) # :nodoc:
      for id in ids
        s = <<-"EOS"
          alias_method :__#{id.to_i}__, :#{id.to_s}
          private :__#{id.to_i}__
          def #{id.to_s}(*args, &block)
            # a newmethod
            (@__#{id.to_i}__ ||= [__#{id.to_i}__(*args, &block)])[0]
          end
        EOS
      puts "MODULE_EVAL: #{s}"
      module_eval s
      end
    end
    private :once
  end
  def jd()
    10
  end
  once :jd
end

D.new.jd
true
#################### Trac Info
# ID:         425
# Summary:    Date#jd causing stack overflow
# Changetime: 2009-04-20 15:46:39+00:00
###

#  The bridge methods end up calling the base method and it happens all over again until the stack overflows:
#  
#  ERROR 2059, GemStone Smalltalk execution stack overflow.
#  
#  Stack overflow,  Stack depth 15565 
#  topaz 1> frame 100
#  100 Date >> jd*&                             (envId 1) @5 line 2
#      receiver [96506881 sz:0 tags:6  Date] aDate
#      args [96405761 sz:0  Array]   anArray
#      block nil
#  topaz 1> list 
#               def jd(*args, &block)
#                 (@__98953473__ ||= [__98953473__(*args, &block)])[0]
#   *          ^5                                                        *******
#     
#     # method starts at line 10 of file eval 
#  topaz 1> frame 101
#  101 Date >> jd                               (envId 1) @2 line 1
#      receiver [96506881 sz:0 tags:6  Date] aDate
#  topaz 1> list
#     <a Ruby bridge method>
#   * ^2                                                                 *******
#       
#  topaz 1> frame 102
#  102 Date >> jd*&                             (envId 1) @6 line 1
#      receiver [96506881 sz:0 tags:6  Date] aDate
#      rest [96401153 sz:0  Array]   anArray
#      block nil
#  topaz 1> list
#     <a Ruby bridge method>
#   * ^6                                                                 *******
#       
#  topaz 1> frame 103
#  103 Date >> jd*&                             (envId 1) @5 line 2
#      receiver [96506881 sz:0 tags:6  Date] aDate
#      args [96401153 sz:0  Array]   anArray
#      block nil
#  topaz 1> list
#               def jd(*args, &block)
#                 (@__98953473__ ||= [__98953473__(*args, &block)])[0]
#   *          ^5                                                        *******
#     
#     # method starts at line 10 of file eval 
#  topaz 1> 
#  
#  