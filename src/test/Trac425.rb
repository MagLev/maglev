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
