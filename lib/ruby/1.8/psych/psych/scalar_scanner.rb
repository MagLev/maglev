require 'strscan'

module Psych
  ###
  # Scan scalars for built in types
  class ScalarScanner
    # Taken from http://yaml.org/type/timestamp.html
    TIME = /^\d{4}-\d{1,2}-\d{1,2}([Tt]|\s+)\d{1,2}:\d\d:\d\d(\.\d*)?(\s*Z|[-+]\d{1,2}(:\d\d)?)?/

    CHAR_KIND_TABLE = begin  
      s = String.new
      s.size=(256)			# all others zero
      for n in ?a..?z ;  s[n] = 1 ; end # Alpha
      for n in ?A..?Z ;  s[n] = 1 ; end # Alpha
      s[ ?~ ] = 1                       # Alpha
      for n in ?0..?9 ;  s[n] = 2 ; end # Digit
      s
    end
    SHORT_PREFIXES = begin
      s = String.new
      s.size=(256)
      'ytonf~YTONF'.each_byte { |b| s[b] = 1 }
      s
    end
    SHORT_STR_CACHE = { 'no' => false , 'false' => false , 'off' => false ,
			'yes' => true, 'true' => true, 'on' => true,
                        '~' => nil , 'null' => nil  }
    SHORT_STR_CACHE.default=(0)
                       
    # Create a new scanner
    def initialize
      @string_cache = {}
    end

    # Tokenize +string+ returning the ruby object
    def tokenize string
      return nil if string.empty?
      return string if @string_cache.key?(string)

      first_ch = string[0]
      ch_kind = CHAR_KIND_TABLE[first_ch]
      if ch_kind == 1  #  string  =~ /^[A-Za-z~]/
        if string.length > 5
          @string_cache[string] = true
          return string
        end
        if SHORT_PREFIXES[first_ch].equal?(1)
          val = SHORT_STR_CACHE[string]
          unless val.equal?(0)
            return val
          end
        end
        @string_cache[string] = true
        return string
      elsif ch_kind == 2  # string =~ /^\d/
        case string
        when TIME  #  /^\d ... /
          date, time = *(string.split(/[ tT]/, 2))
          (yy, m, dd) = date.split('-').map { |x| x.to_i }
          md = time.match(/(\d+:\d+:\d+)(\.\d*)?\s*(Z|[-+]\d+(:\d\d)?)?/)
  
          (hh, mm, ss) = md[1].split(':').map { |x| x.to_i }
          us = (md[2] ? Rational(md[2].sub(/^\./, '0.')) : 0) * 1000000
  
          time = Time.utc(yy, m, dd, hh, mm, ss, us)
  
          return time if 'Z' == md[3]
  
          tz = md[3] ? Integer(md[3].split(':').first.sub(/([-+])0/, '\1')) : 0
          return Time.at((time - (tz * 3600)).to_i, us)
        when /^\d{4}-\d{1,2}-\d{1,2}$/
          require 'date'
          return Date.strptime(string, '%Y-%m-%d')
        end
      elsif first_ch == ?. 
        case string
        when /^\.inf$/i
          return 1 / 0.0
        when /^-\.inf$/i
          return -1 / 0.0
        when /^\.nan$/i
          return 0.0 / 0.0
        end
      elsif first_ch == ?:
        if string =~ /^:./
          if string =~ /^:(["'])(.*)\1/
            return $2.sub(/^:/, '').to_sym
          else
            return string.sub(/^:/, '').to_sym
          end
        end
      end
      case string
      when /^[-+]?[1-9][0-9_]*(:[0-5]?[0-9])+$/
	i = 0
	string.split(':').each_with_index do |n,e|
	  i += (n.to_i * 60 ** (e - 2).abs)
	end
	return i
      when /^[-+]?[0-9][0-9_]*(:[0-5]?[0-9])+\.[0-9_]*$/
	i = 0
	string.split(':').each_with_index do |n,e|
	  i += (n.to_f * 60 ** (e - 2).abs)
	end
	return i
      end
      return Integer(string.gsub(/[,_]/, '')) rescue ArgumentError
      return Float(string.gsub(/[,_]/, '')) rescue ArgumentError
      @string_cache[string] = true
      string
    end
  end
end
