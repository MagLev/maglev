require 'psych/scalar_scanner'

module Psych
  module Visitors
    ###
    # This class walks a YAML AST, converting each node to ruby
    class ToRuby < Psych::Visitors::Visitor
      def initialize
        super
        @st = {}
        @ss = ScalarScanner.new
        @domain_types = Psych.domain_types
      end

      def accept target
        result = super
        return result if @domain_types.empty? || !target.tag

        short_name = target.tag.sub(/^!/, '').split('/', 2).last
        if Psych.domain_types.key? short_name
          url, block = Psych.domain_types[short_name]
          return block.call "#{url}:#{short_name}", result
        end

        result
      end

      def visit_Psych_Nodes_Scalar o
        o_anchor = o.anchor
        @st[o_anchor] = o.value if o_anchor

        o_tag = o.tag
        if o_tag
          if klass = Psych.load_tags[o_tag]
            instance = klass.allocate
  
            if instance.respond_to?(:init_with)
              coder = Psych::Coder.new(o.tag)
              coder.scalar = o.value
              instance.init_with coder
            end
  
            return instance
          end
          return o.value if o.quoted
        else
          return o.value if o.quoted
          return @ss.tokenize(o.value) 
        end
        ch_zero = o_tag[0]
        if ch_zero == ?!  # [
	  if o_tag == '!str'
	    return o.value
          elsif o_tag.start_with?('!ruby/') 
	    if o_tag == "!ruby/object:Complex"
	      return Complex(o.value)
	    elsif o_tag ==  "!ruby/object:Rational"
	      return Rational(o.value)
	    elsif o_tag == "!ruby/regexp"
	      o.value =~ /^\/(.*)\/([mix]*)$/
	      source  = $1
	      options = 0
	      lang    = nil
	      ($2 || '').split('').each do |option|
	        case option
	        when 'x' then options |= Regexp::EXTENDED
	        when 'i' then options |= Regexp::IGNORECASE
	        when 'm' then options |= Regexp::MULTILINE
	        else lang = option
	        end
	      end
	      return Regexp.new(*[source, options, lang].compact)
	    elsif o_tag ==  "!ruby/range"
	      args = o.value.split(/([.]{2,3})/, 2).map { |s|
	        accept Nodes::Scalar.new(s)
	      }
	      args.push(args.delete_at(1) == '...')
	      return Range.new(*args)
	    elsif o_tag.start_with?( '!ruby/sym') && o_tag =~ /^!ruby\/sym(bol)?:?(.*)?$/
	      return o.value.to_sym
	    end
	  elsif o_tag == '!binary'
	    return o.value.unpack('m').first
	  elsif o_tag == "!float"
	    return Float(@ss.tokenize(o.value))
          end
	elsif ch_zero == ?t  # ] [
          if o_tag == 'tag:yaml.org,2002:str'
            return o.value
          elsif o_tag == 'tag:yaml.org,2002:binary'
            return o.value.unpack('m').first
          elsif o_tag == "tag:yaml.org,2002:float"
            return Float(@ss.tokenize(o.value))
          end
        end # ]
        @ss.tokenize o.value  # DEFAULT
      end

      def visit_Psych_Nodes_Sequence o
        o_tag = o.tag
        if o_tag # [
	  if klass = Psych.load_tags[o_tag]
	    instance = klass.allocate

	    if instance.respond_to?(:init_with)
	      coder = Psych::Coder.new(o.tag)
	      coder.seq = o.children.map { |c| accept c }
	      instance.init_with coder
	    end

	    return instance
	  end
	  if o_tag[0] == ?! # STARTS with '!' # [
	    if o_tag ==  '!omap'
	      map = Psych::Omap.new       # OMAP
	      @st[o.anchor] = map if o.anchor
	      o.children.each { |a|
		map[accept(a.children.first)] = accept a.children.last
	      }
	      return map
	    elsif o_tag == '!ruby/object:IdentitySet'  # maglev
	      set = IdentitySet.new
	      @st[o.anchor] = set if o.anchor
	      o.children.each { | elem |
		set << accept(elem)
	      }
	      return set
	    end  
	  elsif o_tag == 'tag:yaml.org,2002:omap'
	    map = Psych::Omap.new		# same as OMAP
	    @st[o.anchor] = map if o.anchor
	    o.children.each { |a|
	      map[accept(a.children.first)] = accept a.children.last
	    }
	    return map
	  end # ]
        end  # ]
        # DEFAULT, create an Array
        list = []
        o_anchor = o.anchor
        @st[o_anchor] = list if o_anchor
        o.children.each { |c| list.push accept c }
        list
      end

      def visit_Psych_Nodes_Mapping o  # [
        o_tag = o.tag  # o_tag could be nil
        if o_tag
          return revive(Psych.load_tags[o_tag], o) if Psych.load_tags[o_tag]

	  ch_zero = o_tag[0]
	  if ch_zero == ?! # STARTS with '!' # [
	    ch_one = o_tag[1]
	    if ch_one == ?s
	      if o_tag == '!str'
		members = Hash[*o.children.map { |c| accept c }]  # STRING
		string = members.delete 'str'
		return init_with(string, members.map { |k,v| [k.to_s.sub(/^@/, ''),v] }, o)
	      elsif o_tag == '!set'
		set = Psych::Set.new
		@st[o.anchor] = set if o.anchor
		o.children.each_slice(2) do |k,v|
		  set[accept(k)] = accept(v)
		end
		return set
	      end
	    elsif o_tag.start_with?('!ruby/') #   STARTS WITH '!ruby'  # [
	      if o_tag.start_with?('!ruby/object:')
		if o_tag == '!ruby/object:IdentityHash'
		    h = IdentityHash.new
		  @st[o.anchor] = set if o.anchor
		  o.children.each_slice(2) do |k,v|
		    h[accept(k)] = accept(v)
		  end
		  return h

		elsif o_tag == '!ruby/object:Complex'
		  h = Hash[*o.children.map { |c| accept c }]
		  return Complex(h['real'], h['image'])

		elsif o_tag == '!ruby/object:Rational'
		  h = Hash[*o.children.map { |c| accept c }]
		  return Rational(h['numerator'], h['denominator'])

		elsif o_tag =~ /^!ruby\/object:?(.*)?$/
		  name = $1 || 'Object'
		  obj = revive((resolve_class(name) || Object), o)
		  @st[o.anchor] = obj if o.anchor
		  return obj
		end
	      elsif o_tag == '!ruby/range'
		h = Hash[*o.children.map { |c| accept c }]
		return Range.new(h['begin'], h['end'], h['excl'])

	      elsif o_tag =~  /^!ruby\/struct:?(.*)?$/
		klass = resolve_class($1)
		if klass
		  s = klass.allocate
		  @st[o.anchor] = s if o.anchor

		  members = {}
		  struct_members = s.members.map { |x| x.to_sym }
		  o.children.each_slice(2) do |k,v|
		    member = accept(k)
		    value  = accept(v)
		    if struct_members.include?(member.to_sym)
		      s.send("#{member}=", value)
		    else
		      members[member.to_s.sub(/^@/, '')] = value
		    end
		  end
		  return init_with(s, members, o)
		else
		  members = o.children.map { |c| accept c }
		  h = Hash[*members]
		  return Struct.new(*h.map { |k,v| k.to_sym }).new(*h.map { |k,v| v })
		end
	      elsif o_tag =~ /^!ruby\/exception:?(.*)?$/
		h = Hash[*o.children.map { |c| accept c }]
		e = build_exception((resolve_class($1) || Exception),
				  h.delete('message'))
		return init_with(e, h, o)
	      end
	    end  # STARTS WITH '!ruby' # ]
	    # ]
	  elsif ch_zero == ?t  && o_tag == 'tag:yaml.org,2002:str'
	    members = Hash[*o.children.map { |c| accept c }]  # same as STRING
	    string = members.delete 'str'
	    return  init_with(string, members.map { |k,v| [k.to_s.sub(/^@/, ''),v] }, o)
	  end
        end
        # DEFAULT , create a Hash
	hash = {}
        o_anchor = o.anchor
	@st[o_anchor] = hash if o_anchor

	o.children.each_slice(2) { |k,v|
	  key = accept(k)
	  if key == '<<' && Nodes::Alias === v
	    # FIXME: remove this when "<<" syntax is deprecated
	    if $VERBOSE
	      where = caller.find { |x| x !~ /psych/ }
	      warn where
	      warn "\"<<: *#{v.anchor}\" is no longer supported, please switch to \"*#{v.anchor}\""
	    end
	    return accept(v)
	  else
	    hash[key] = accept(v)
	  end

	}
	hash
      end # ]

      def visit_Psych_Nodes_Document o
        accept o.root
      end

      def visit_Psych_Nodes_Stream o
        o.children.map { |c| accept c }
      end

      def visit_Psych_Nodes_Alias o
        @st[o.anchor]
      end

      private
      def revive klass, node
        s = klass.allocate
        h = Hash[*node.children.map { |c| accept c }]
        init_with(s, h, node)
      end

      def init_with o, h, node
        if o.respond_to?(:init_with)
          c = Psych::Coder.new(node.tag)
          c.map = h
          o.init_with c
        else
          h.each { |k,v| o.instance_variable_set(:"@#{k}", v) }
        end
        o
      end

      # Convert +klassname+ to a Class
      def resolve_class klassname
        return nil unless klassname and not klassname.empty?

        name    = klassname
        retried = false

        begin
          path2class(name)
        rescue ArgumentError => ex
          name    = "Struct::#{name}"
          unless retried
            retried = true
            retry
          end
          raise ex
        end
      end
    end
  end
end
