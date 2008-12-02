# Maps to Smalltalk class GsProcess .  See Globals.rb
class Thread

  class_primitive_nobridge '_stbacktrace', 'backtraceToLevel:'

  def self._backtrace(includeSt, limit)
    unless limit._isFixnum
      raise ArgumentError, 'limit must be a Fixnum'
    end
    unless limit > 0
      raise ArgumentError, 'limit must be > 0'
    end
    result = []
    _stbacktrace(limit).each do |ary|
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
      elsif includeSt
        if  /(.*) \(envId 0\)/ =~ where
	  meth = $1
	  result << "smalltalk:#{line}: in '#{meth}'"
        else
	  result << "smalltalk:#{line}: in #{where} " # usually in Executed Code
        end
      end
    end
    result[1..-1]
  end

end

