class Exception
    primitive 'signal', 'signal:'
    self.class.primitive 'signal', 'signal:'
    primitive 'message', 'description'
    primitive '_backtrace', 'backtrace'
    def backtrace
      result = []
      _backtrace.each do |ary|
        where, line, source = ary
        if /(.*) \(envId 1\)/ =~ where
          meth = $1
          if source
            lines = source.split("\n").grep(/# method/)
            unless lines.empty?
              if /line (\d+) .* file (.*)/=~ lines[-1]
                baseline = $1.to_i
                file = $2
                result << "#{file[0..-2]}:#{baseline+line}: in '#{meth}'"
              end
            end
          end
        end
      end
      result[1..-1]
    end
end