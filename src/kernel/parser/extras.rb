# require 'stringio'
# require 'racc/parser'
# require 'sexp'
# require 'strscan'

# Fixnum#ord deleted

class Array
  def _join
    # optimized version of join
    sep = nil  # default value of $,
    n = 0
    out = ""
    sz = self.size
    while (n < sz)
      elem = at(n)
      if elem._isArray
        raise ArgumentError, 'Array#_join, nested Array not supported'
      end
      out << elem.to_s
      n += 1
    end
    out
  end
end

class String
  def _count_eols
    count = 0
    idx = 1
    while true
      idx = self.__indexOfByte( ?\n , idx )
      if idx._equal?(0)
        return count
      end
      count += 1
      idx += 1
    end
  end

  def _contains_string(a_string)
    self.__findStringStartingAt(a_string, 1)._not_equal?(0)
  end
  def _contains_byte(bytevalue)
    self.__indexOfByte(bytevalue, 1)._not_equal?(0)
  end
end

class Symbol
  # allow String-like access to bytes of Symbols
  primitive_nobridge_env '[]' , '_rubyAt', ':'

  def __as_symbol
    self  
  end
end

# class InternalParseError < StandardError
# end

class TransientShortArray
  # TransientShortArray supports contigous in-memory instances 
  # up to 65Kbytes. Each element is a 16 bit integer.
  # Instances may not be committed . 
  #
  # The parser uses TransientShortArrays for the in-memory
  # copies of some of the racc state-tables,
  # which would exceed the small-object limit of 8K bytes if 
  # represented with normal Arrays .
  
  class_primitive_nobridge '_with_shorts', '_withAllShorts:'
    # copies an Array
    
  primitive_nobridge '[]', '_rubyParserShortAt:'
    # zero values are translated to a nil result
    
  primitive_nobridge '[]=', '_rubyShortAt:put:'
end

module MagRp # {

    # RPStringScanner deleted, code moved to strscan2.rb
    # Keyword moved to rp_classes.rb

    class Environment # {
      # first openning of Environment is in rp_classes.rp,
      #    so OFF_ constant refs can be static

      def reset
	@curridx = 0 - ENTRY_SIZE 
	self.extend(false, false)
      end

      def extend(dyn, kind_sym )  # arg is false or true
	idx = @curridx + ENTRY_SIZE
	@curridx = idx 
        ary = @arr
	ary[idx ] = nil   # the env dict , created on demand
	ary[idx + OFF_dyn] = dyn
	ary[idx + OFF_use] = nil # # the use dict , created on demand
        pos = @src_scanner.pos
        ary[idx + OFF_byte_offset] = pos
        if kind_sym._not_equal?( nil )
          if kind_sym._equal?( :def )
            if idx._equal?(ENTRY_SIZE) and @first_top_level_def_offset._equal?(-1)
              if @module_count > 0
                @first_top_level_def_offset = pos
              end
            end
          elsif kind_sym._equal?( :module )
            @module_count = @module_count + 1
          end
        end  
      end

      def in_block_params=(v)
        @in_block_params = v
      end

      def in_block_params
        @in_block_params
      end

      def unextend
        idx = @curridx
        @extend_ofs_last_unextend = @arr[idx + OFF_byte_offset]
	idx = idx - ENTRY_SIZE
	if idx < 0
	  syntax_error("You went too far unextending env")
	end
	@curridx = idx
      end

      def is_extended
        @curridx > 0
      end

      def []( k)
	# look for k starting in current and including first non-dynamic scope
	idx = @curridx
	ary = @arr
	while idx >= 0
	  h = ary[idx ]
          if h._not_equal?(nil)
	    v = h[k] 
	    if v._not_equal?(nil)
	      return v
	    end
          end
          if ary[idx + OFF_dyn]._equal?( false)
            break # exit loop , we just probed first non-dynamic scope
          end
	  idx -= ENTRY_SIZE
	end
	nil
      end

      def []=( k, v)
	# raise "no" if v == true
	if v._equal?(true)
	  internal_error("Environment:[]= , invalid true arg")
	end
	# self.current[k] = v
        idx = @curridx
        ary = @arr
	h = ary[idx ]
        if h._equal?(nil)
          h = Hash.new 
          ary[idx ] = h 
        end
	h[k] = v
      end

      # def all ; end #  not used

      # def env ; end # same as  current

      def current_at(akey)
	# @env.first
        h = @arr[@curridx ]
        if h._not_equal?( nil)
          h[akey]
        else
          nil
	end
      end

      def dynamic_keys
	# return all keys in current up to and excluding first non-dynamic
	idx = @curridx
	ary = @arr
	set = IdentitySet.new
	while idx >= 0
	  if ary[idx + OFF_dyn]._equal?( false) 
	    break # exit while loop
	  end
	  h = ary[idx ]
          if h._not_equal?(nil)
	    h.__add_keys_to(set)
          end
	  idx -= ENTRY_SIZE
	end
	set.to_a 
      end

      def dynamic?
	# @dyn[0] != false
	@arr[@curridx + OFF_dyn]._equal?( true )
      end

      def use( id)
	#@env.each_with_index do |env, i|
	#  if env[id] then
	#    @use[i][id] = true
	#  end
	#end
	idx = @curridx
	ary = @arr
	while idx >= 0
	  h_env = ary[idx ]
          if h_env._not_equal?(nil)
	    if h_env[id]
	      h_use = ary[idx + OFF_use]
              if h_use._equal?(nil)
                h_use = Hash.new
                ary[idx + OFF_use] = h_use
              end
	      h_use[id] = true
            end
	  end
	  idx -= ENTRY_SIZE
	end
      end

      def used?( id)
	idx = @curridx
	ary = @arr
	while idx >= 0
	  h_use = ary[idx + OFF_use]
          if h_use._not_equal?(nil)
	    if h_use[idx]
	      return true
            end
	  end
	end 
	false
      end

    end # }

    class StackState < Array  # {
      # deleted instvar @stack and made StackState a subclass of Array
      def initialize
	self[0] = false 
      end

      def inspect
	"StackState:" << super 
      end

      def is_in_state
	# @stack.last
	self[ self.size - 1]
      end

      def lexpop
        # raise if @stack.size == 0
        # a = @stack.pop
        # b = @stack.pop
        # @stack.push(a || b)
	sz = self.size
        if (sz >= 2)
          new_sz = sz - 1
          a = self[new_sz]
          if a
            self[new_sz - 1] = a
          end
          self.size=( new_sz )
        elsif sz._equal?(0)
          internal_error('lexpop underflow')
        else
          # no change if stack size == 1
        end
	self
      end

      def pop
	sz = self.size
	sz_minus_1 = sz - 1 
	# r = @stack.pop
	r = self[ sz_minus_1 ]
	# @stack.push false if @stack.size == 0
	if sz_minus_1._equal?(0)
	  self[ sz_minus_1 ] = false
	else
	  self.size=( sz_minus_1 )
	end
	r
      end

      def push( val)
	# @stack.push val
	sz = self.size
	self[sz] = val
	self 
      end
    end # }

# Gemstone, deleted class Sexp

  class RubyParser # {

    def line_for(name_tok)
      # for syntax error messages
      # returns a String
      num = -1
      if name_tok._is_a?(RpNameToken)
	byte_ofs = name_tok.src_offset
	num = @lexer.line_for_offset(byte_ofs)
      end
      num == -1 ? '???' : num.to_s
    end

    def line_for_offset(byte_ofs)
      # error messages and debugging
      @lexer.line_for_offset(byte_ofs)
    end

    def last_closed_def_message
      # for error messages
      ofs = @env.last_closed_def_offset
      if ofs >= 0
	num = @lexer.line_for_offset(ofs)
	#if (num > 1)
	#  num -= 1   # usually extend happens after EOL has been lexed
	#end
	msg = " last 'end' matched def/class/module near line "
	msg << (num >= 0 ? num.to_s : '???' )
      else
	msg = ''
      end
      msg
    end

    def assignable(lhs, value) # value maybe nil    # [
      lhs_cls = lhs.class
      if lhs_cls._equal?(RpNameToken)
	id = lhs.symval 
	src_ofs = lhs.src_offset 
      elsif lhs._kind_of?( RubyAbstractLiteralNode )
	syntax_error("Can't change the value of #{lhs.nameForError}")
      else
	syntax_error("invalid left side of assignment");
      end
      if InvalidAssignableLhs.include?(id)
	syntax_error("Can't change the value of #{id}")
      end
      first_ch = id[0]
      result = nil
      if first_ch <= ?Z
	if first_ch._equal?(  ?@ )
	  if id[1]._equal?( ?@ )
	    # asgn = self.in_def || self.in_single > 0
	    # s((asgn ? :cvasgn : :cvdecl), id)
	    #   maglev, :cvasgn , :cvdecl  both have same production
	    result = RubyClassVarDeclNode.s(id, value)  # s(:cvasgn ) , s(:cvdecl) 
	  else
	    result = RubyInstAsgnNode.s(id, value) # s(:iasgn )
	  end
	elsif first_ch._equal?( ?$ )
	  result = RubyGlobalAsgnNode.s(id, value) # s(:gasgn )
	elsif RpStringScanner.ch_is_uc_alpha(first_ch)   # A-Z
          if @in_def || @in_single > 0
	    line = self.line_for(lhs)
            syntax_error("dynamic constant assignment of #{id} near line #{line}")
          end
	  c2node = RubyColon2Node.simple(id, src_ofs )
	  result = RubyConstDeclNode.s(c2node, value) # s(:cdecl )
	  result.src_offset=( src_ofs )
	end
      end
      if result._equal?(nil) 
	cenv = @env
	type = cenv[id]
	if type._equal?( :lvar) 
	  result = RubyLocalAsgnNode.s(id, value) # s(:lasgn )
	  result.src_offset=( src_ofs )
	  return result
	elsif type._equal?( nil ) || type._equal?(:dvar)
	   if cenv.current_at(id)._equal?(  :dvar)  then
	     # ok
	   elsif type._equal?( :dvar) then
	     @env.use(id)
	   end
	   result = RubyLocalAsgnNode.s(id, value) # s(:lasgn )
	else
	   internal_error("in assignable:  unknown type: #{@env[id]}")
	end
	if type._equal?( nil)
	  cenv[id] = :lvar
	end
      else
	@env[id] = :lvar
      end
      result.src_offset=( src_ofs )
      return result
    end      # ]

    def check_assignable(lhs) 
      # has side effect of updating env 
      if lhs._isSymbol
	id = lhs
	if InvalidAssignableLhs.include?(id)
	  syntax_error("Can't change the value of #{id}")
	end
      else
	internal_error("check_assignable arg not a symbol")
      end

      first_ch = id[0]
      ok = nil
      if first_ch <= ?Z
	if first_ch._equal?(  ?@ )
	  if id[1]._equal?( ?@ )
	    ok = true # for s(:cvasgn ) , s(:cvdecl)
	  else
	    ok = true # for s(:iasgn )
	  end
	elsif first_ch._equal?( ?$ )
	  ok = true # for  s(:gasgn )
	elsif RpStringScanner.ch_is_uc_alpha(first_ch)
	  ok = true # s(:cdecl )
	end
      end
      if ok._equal?(nil) 
	type = @env[id]
	if type._equal?( :lvar) 
	  #ok
	elsif type._equal?( nil ) || type._equal?(:dvar)
	   if @env.current_at(id)._equal?(  :dvar)  then
	     # ok
	   elsif type._equal?( :dvar) then
	     @env.use(id)
	   end
	   #
	else
	   internal_error("in assignable:  unknown type: #{@env[id]}")
	end
      end
      if type._equal?( nil)
	@env[id] = :lvar
      end
      nil
    end

    def min_line(a_line, b_line)
      if a_line._equal?(nil)
	return b_line
      end
      if b_line._equal?(nil)
	return a_line
      end
      return a_line < b_line ? a_line : b_line
    end

    def block_append(head, tail)  #  , strip_tail_block=false
	  # can't find any callers passing strip_tail_block==true
      h_arg = head
      return head unless tail
      return tail unless head

      # deleted  # when :lit, :str then  # :str 
      # to fix Trac 764

      # head = remove_begin(head)
      head = head.kbegin_value

      #  head = s(:block, head) unless head[0] == :block
      unless head.class._equal?(RubyBlockNode)
	head = RubyBlockNode.s( [ head ] )
      end

      # if strip_tail_block and Sexp === tail and tail[0] == :block then 
      #  head.push(*tail.values)
      # else
      #  head << tail
      # end               # note strip_tail_block always false
      head.append_to_block( tail )
      head
    end

    def cond_or_nil(node)
      if node._equal?(nil)
	nil
      else
	cond(node)
      end
    end 

    def cond(arg)
      node = value_expr(arg)
      node = node.as_cond(self)
   # # as_cond(aMagRp) implements:
   #  case node.first
  # following two 'when's  deleted as of ruby_parser2.0.3
   #  when :dregx then     # RubyDRegexpNode   # Ryan says not used
   #    return s(:match2, node, s(:gvar, "$_".to_sym))
   #  when :regex then     # RubyRegexpNode  # :regex is Rubinius only
   #    return s(:match, node)
  #
   #  when :lit then     # RubyRegexpNode
   #    if Regexp === node.last then
   #      return s(:match, node)
   #    else
   #      return node
   #    end
   #  when :and then	# RubyAndNode
   #    return s(:and, cond_or_nil(node[1]), cond_or_nil(node[2]))
   #  when :or then	# RubyOrNode
   #    return s(:or,  cond_or_nil(node[1]), cond_or_nil(node[2]))
   #  when :dot2 then     # RubyDotNode
   #    label = "flip#{node.hash}"
   #    env[label] = :lvar
   #    return s(:flip2, node[1], node[2])
   #  when :dot3 then     # RubyDotNode
   #    label = "flip#{node.hash}"
   #    env[label] = :lvar
   #    return s(:flip3, node[1], node[2])
   #  else		# RubyNode
   #    return node
   #  end
      node
    end

    def get_match_node( lhs, sel_tok, rhs) # TODO202: rename to new_match
      if lhs then
	#case lhs[0]
	#when :dregx, :dregx_once then
	#  return s(:match2, lhs, rhs)
	#when :lit then
	#  return s(:match2, lhs, rhs) if Regexp === lhs.last
	#end
	l_cls = lhs.class
	if l_cls._equal?(RubyDRegexpNode)  # :dregx and :dregx_once
	  res = RubyMatch2Node.s(lhs, rhs)
	  res.src_offset=( sel_tok.src_offset )
	  return res
	elsif l_cls._equal?(RubyRegexpNode)
	  res = RubyMatch2Node.s(lhs, rhs)
	  res.src_offset=( sel_tok.src_offset )
	  return res
	end
      end

      if rhs then
	#case rhs[0]
	#when :dregx, :dregx_once then
	#  return s(:match3, rhs, lhs)
	#when :lit then
	#  return s(:match3, rhs, lhs) if Regexp === rhs.last
	#end
	r_cls = rhs.class
	if r_cls._equal?(RubyDRegexpNode)  # :dregx and :dregx_once
	  res = RubyMatch2Node.s(rhs, lhs)  # we use Match2Node for :match3
	  res.src_offset=( sel_tok.src_offset )
	  return res
	elsif r_cls._equal?(RubyRegexpNode)
	  res = RubyMatch2Node.s(rhs, lhs)
	  res.src_offset=( sel_tok.src_offset )
	  return res
	end
      end

      # return s(:call, lhs, :"=~", s(:arglist, rhs))
      final_sel = RpNameToken.new( :"=~" , sel_tok.src_offset ) 
      res = new_call_1( lhs,  final_sel , rhs )
      res
    end

    def gettable(arg)
      # raise "no: #{id.inspect}" if Sexp === id
      #  id = id.to_sym if Sexp   === id # HACK
      if arg.class._equal?(RpNameToken)
	id = arg.symval
	src_ofs = arg.src_offset
      elsif arg._isSymbol
	id = arg 
	return RubySelfNode._new if id._equal?(:self) # s(:self)
	return RubyNilNode._new  if id._equal?(:nil)  # s(:nil)  
	return RubyTrueNode._new if id._equal?(:true) # s(:true)
	return RubyFalseNode._new if id._equal?(:false) # s(:false) 
	src_ofs = nil
      else
	internal_error("gettable unrecognized argument kind")
      end 
      
      first_ch = id[0]
      if first_ch <= ?Z
	if first_ch._equal?(  ?@ )
	  if id[1]._equal?( ?@ )
	    return RubyClassVarNode.s(id) # s(:cvar, id)
	  else
	    return RubyInstVarNode.s(id)  # s(:ivar, id)   
	  end
	elsif first_ch._equal?( ?$ )
	  res = RubyGlobalVarNode.s( id)  # s(:gvar )
	  res.src_offset=( src_ofs )
	  return res
	elsif RpStringScanner.ch_is_uc_alpha(first_ch)
	  return RubyConstNode.s( id, src_ofs )  # s(:const )
	end 
      end
      if first_ch._equal?( ?_ )
	if id._equal?(:__FILE__)   
	  return RubyStrNode.s( @file_name )        
	elsif id._equal?(:__LINE__) # s(:lit...)  # id was a RpNameToken
	  return RubyFixnumNode.s( src_ofs )
	end
      end
      cenv = @env
      type = cenv[id]
      if type._equal?(:lvar) then
	return RubyLocalVarNode.s(id)  # rp202 code always took this path ???
      elsif type._equal?(:dvar) && cenv.dynamic? then
	return RubyLocalVarNode.s(id)  # s(:lvar, id)  
      else
	# return  s(:call, nil, id, s(:arglist))  
	return new_vcall(RubySelfNode._new, arg )
      end
      internal_error("identifier #{id.inspect} is not valid")
    end

    def initialize
      # super()  # nothing in Parser to initialize
      lx = RubyLexer.new
      lx.parser = self
      @lexer = lx
      @env = Environment.new
      #  @comments  no longer used , this parser not for use with RDoc implementation
      lx._install_wordlist( @lexer_wordlist, @debuglevel )
    end

    def reset
      lexer.reset
      @env.reset
    end

    def list_append(list, item )  # TODO202: nuke me *sigh*
      # return s(:array, item) unless list
      # list = s(:array, list) unless Sexp === list && list.first == :array
      # list << item
      arrayCls = RubyRpCallArgs
      if list._equal?(nil)
	return arrayCls.s( item) 
      end
      unless list.class._equal?(arrayCls) 
	list = arrayCls.s(list)
      end
      list.append(item)
      list
    end

    def list_prepend( list, item )  # TODO202: nuke me *sigh*
      # list = s(:array, list) unless Sexp === list && list[0] == :array
      # list.insert 1, item
      # list
      arrayCls = RubyRpCallArgs
      if list.class._equal?(arrayCls)
	list.prepend(item)  
      else
	list = arrayCls.s(item, list)  
      end
      list
    end

    def literal_concat( head, tail)
      unless head
	return tail 
      end
      unless tail
	return head 
      end
      # htype, ttype = head[0], tail[0]
      htype = head.str_dstr_evstr_kind()  # 0,1,2  or nil
      ttype = tail.str_dstr_evstr_kind()

      if htype._equal?(2)  # htype == :evstr
	head = RubyDStrNode.s([ RubyStrNode.s('') , head] )   
      end

      # case ttype
      if ttype._equal?(0) #  when :str then
	if htype._equal?(0) #  htype == :str
	  # append tail's String to head's String
	  head.appendString( tail.strNodeValue )  # head[-1] << tail[-1]
	elsif  htype._equal?(1) and head.size._equal?(1) # htype == :dstr and head.size == 2 
	  # head is a dstr with a StrNode and no  further list elements
	  head.appendToHeadString( tail.strNodeValue ) #  head[-1] << tail[-1]
	else  # htype == :dstr , or was :evstr changed to :dstr above
	  head.appendToList( tail ) 
	end
      elsif ttype._equal?(1)  # when :dstr then
	if htype._equal?(0) #   htype == :str then
	  # tail[1] = head[-1] + tail[1]
	  tail.appendToHeadString( head.strNodeValue )
	  head = tail
	else  # tail and head both  :dstr  
	  #tail[0] = :array
	  #tail[1] = s(:str, tail[1])
	  #tail.delete_at 1 if tail[1] == s(:str, '')
	  # head.push(*tail[1..-1])
	  t_list = tail.dstrList
	  t_list_head = t_list[0]
	  if t_list_head.strNodeValue == ''
	    t_list.delete_at(0)
	  end
	  head.dstrList.push( *t_list)
	end
      elsif ttype._equal?(2)  #   when :evstr then  # tail is EvStrNode
	# head[0] = :dstr if htype == :str
	if htype._equal?(0)  # head is StrNode
	  t_body = tail.evStrBody
	  if t_body.class._equal?(RubyStrNode)
	    head.appendString( t_body.strNodeValue )
	  else
	    dstr = RubyDStrNode.s( [ head, tail ] )
	    head = dstr   # become not needed by design of .y file
	  end
	else
	  # head is a DStrNode
	  head.dstrList.push( tail)
	end
      else
	internal_error( "literal_concat unknown type")
      end
      return head
    end

  # def logop(type, left, right) # TODO202: rename logical_op
  #   left = value_expr(left)
  #   if left and left[0] == type and not left.paren then
  #     node, second = left, nil
  #     while (second = node[2]) && second[0] == type and not second.paren do
  #       node = second
  #     end
  #     node[2] = s(type, second, right)
  #     return left
  #   end
  #   return s(type, left, right)
  # end

    def logop(cls, left_arg, right )
      left = value_expr(left_arg)
      if left and left.class._equal?(cls) and not left.paren then
	node = left
	second = nil
	while (second = node.secondNode) && second.class._equal?(cls) and not second.paren 
	  node = second
	end
	node.secondNode=( cls.s( second, right ) )
	return left
      end
      if left.is_void_result
        raise SyntaxError, 'void value expression'
      end
      return cls.s( left, right)
    end

    def new_aref( val, vofs )
      # used from    | primary_value "[" aref_args tRBRACK
      v_two = val[vofs + 2]  # the aref_args , an arguments list
      if v_two._equal?(nil)
	v_two = RubyRpCallArgs._new
      end
      res = RubyCallNode.s( val[vofs ], :"[]" , v_two )
      res.src_offset=( val[vofs + 3].src_offset )  # val[3] is the tRBRACK, a RpNameToken
      res
    end

    def new_body( val, vofs)
      v_zero = val[vofs]
      result = v_zero
      v_one = val[vofs + 1]
      if v_one             # we have a rescue clause
	r_body = nil
	if v_zero 
	  r_body = v_zero
	end
	rescuebody = v_one
	#while rescuebody do
	#  result << resbody
	#  resbody = resbody.resbody(true)  # does not make sense
	#end

	r_else = val[vofs + 2]
	result = RubyRescueNode.s( r_body, rescuebody, r_else) # s(:rescue )
	result.src_offset=( rescuebody.src_offset )
      elsif not (v_two = val[vofs + 2])._equal?(nil) then
	lnum = self.line_for_offset( @lexer.last_else_src_offset )
	warning("else without rescue is useless, near line #{lnum} ")
	result = block_append(result, v_two)  # may create a new :block
      end
      v_three = val[vofs + 3]
      if v_three  # v_three is a RubyEnsureNode
        v_three.set_body( result ) 
	result = v_three 
      end
      return result
    end

    def new_fcall( sel_tok , arg)
      # receiver is implicit self
      if arg._equal?(nil)
	#  args ||= s(:arglist) , i.e. zero args
	return new_vcall(RubySelfNode._new, sel_tok)
      end
      arg_cls = arg.class
      if arg_cls._equal?(RubyRpCallArgs)
	lst = arg.list
	if lst.size._equal?(0)
	  return new_vcall(RubySelfNode._new, sel_tok)
	end
        sym = check_selector( sel_tok )
	result = RubyFCallNode.s( RubySelfNode._new, sym, arg ) # s(:vcall )
      elsif arg_cls._equal?( RubyBlockPassNode )
        sym = check_selector( sel_tok )
	result = RubyFCallNode.s( RubySelfNode._new, sym, nil) # s(:vcall )
	result.iter=( arg )
      else
        sym = check_selector( sel_tok )
	cArgs = RubyRpCallArgs._new
	cArgs.list=( [ arg ] )
	result = RubyFCallNode.s( RubySelfNode._new, sym , cArgs ) # s(:vcall )
      end
      result.src_offset=( sel_tok.src_offset )
      result
    end

    def check_selector(sel_tok)
      # returns selector symbol , guards against malformed elsif 
      sel = sel_tok.symval
      if sel[0]._equal?( ?e )
        # Maglev enhancement, guard against malformed   elsif 
        if sel._equal?( :elseif ) || sel._equal?( :elif )
          # disallow the other forms of elsif common in script languages
	  line = self.line_for(sel_tok)
          syntax_error("malformed elsif, see '#{sel}' at line #{line}")
        elsif sel.__at_equals(1, 'else') # one-based offset
	  line = self.line_for(sel_tok)
          warning("possible malformed elsif, see '#{sel}' at line #{line}")
        end
      end 
      sel
    end

    def new_vcall(recv, sel_tok)
      # call with void , i.e. zero , args
      sym = check_selector( sel_tok )
      result = RubyVCallNode.s(recv, sym)
      result.src_offset=( sel_tok.src_offset )
      result
    end

    def new_call(recv, sel_tok , arg)
      # used where rp202 had   new_call(r,sel,v[n]) without explicit s(:arglist, v)
      # convert arg list to a RubyRpCallArgs for hasRestArg AST->IR phase
      # generate VCallNode if possible, else FCallNode, else CallNode
      if arg._equal?(nil)
	#  args ||= s(:arglist) , i.e. zero args
	return new_vcall(recv, sel_tok)       # DONE
      end
      arg_cls = arg.class
      if arg_cls._equal?(RubyRpCallArgs)
	if arg.is_empty
	  return new_vcall(recv, sel_tok)     # DONE
	end
	cArgs = arg
      elsif arg_cls._equal?( RubyBlockPassNode )
	if recv.class._equal?( RubySelfNode )
	  result = RubyFCallNode.s( recv , sel_tok.symval , nil ) # s(:fcall)
	else
	  result = RubyCallNode.s( recv, sel_tok.symval , nil )  # s(:call )
	end
	result.iter=( arg )
	result.src_offset=( sel_tok.src_offset )
	return result     # DONE
      else
	cArgs = RubyRpCallArgs._new
	cArgs.list=( [ arg ] ) 
      end
      if recv.class._equal?( RubySelfNode )
	result = RubyFCallNode.s( recv , sel_tok.symval , cArgs ) # s(:fcall)
      else
	result = RubyCallNode.s( recv, sel_tok.symval , cArgs )  # s(:call )
      end
      result.src_offset=( sel_tok.src_offset )
      result
    end

    def new_call_1(recv, sel_tok , argone)
      # used where  rp202 had  new_call(r,sel, s(:arglist, argone))
      #   argone should never be a RubyBlockPassNode 
      if argone._equal?(nil)
	internal_error("new_call_1 unexpected nil arg")
	cArgs = nil
      else
	cArgs = RubyRpCallArgs._new  # s(:array )
	cArgs.list=( [ argone ] ) 
      end
      if recv.class._equal?( RubySelfNode )
	result = RubyFCallNode.s( recv , sel_tok.symval , cArgs ) # s(:fcall)
      else
	result = RubyCallNode.s( recv, sel_tok.symval , cArgs )  # s(:call )
      end
      result.src_offset=( sel_tok.src_offset )
      result
    end

    def new_case(expr, body)
      if body.class._not_equal?(RubyWhenNode)
	internal_error("new_case - bad body arg ")
      end
      # chaining of body nodes done in smalltalk
      RubyCaseNode.s(expr, body)
    end

    def new_class(val, vofs)
      #line, path, superclass, body = val[1], val[2], val[3], val[5]
      # line = val[1] # DELETED from .y
      path = val[vofs + 1]
      superclass = val[vofs + 2]
      body = val[vofs + 4]             
      k_end = val[vofs + 5]
      if k_end._equal?( :tEOF )
	msg = 'syntax error, unexpected $end, expecting kEND'
	line = self.line_for(path)
	msg << ", for   class    near line #{line} \n"
	msg << self.last_closed_def_message
	syntax_error( msg )
      end

      # scope = s(:scope, body).compact  
      # AST does not use  scope 

      # result = s(:class, path, superclass, scope)
      #   expect path to be either a Colon2Node or a Colon3Node
      result = RubyClassNode.s(path , superclass, body, @lexer.source_string) 
      result
    end

    def new_compstamt( v_zero )
      result = void_stmts(v_zero )
      if result
	result = result.kbegin_value 
      end
      result
    end

    def new_defn(val, vofs )
      # (line, bol), name, args, body = val[2], val[1], val[3], val[4]
      # line, bol = val[2]   # unused
      def_tok = val[vofs ] # a DefnNameToken
      name_tok = val[vofs + 1]
      args = val[vofs + 3]
      body = val[vofs + 4]
      k_end = val[vofs + 5]   # yacc_value for kEND synthesized EOF
      if k_end._equal?( :tEOF )
	msg = 'syntax error, unexpected $end, expecting kEND'
	msg << ", for   def   near line #{def_tok.line} "
	msg << self.last_closed_def_message
	syntax_error(msg)
      end

      # body ||= s(:nil)
      # body ||= s(:block)
      # body = s(:block, body) unless body.first == :block 
      if body._equal?(nil)
	body = RubyBlockNode._new
      elsif body.class._equal?(RubyBlockNode)
	# ok
      else
	body = RubyBlockNode.s( [ body ] )  # s(:block, body) 
      end
      if name_tok._isString
	name_sym = name_tok.__as_symbol  # from a reserved word
      else
	name_sym = name_tok.symval # expect a RpNameToken
      end
      result = RubyDefnNode.s(name_sym, args, body) # s(:defn )
      result.src_offset=( def_tok.src_offset )
      result.start_line=( def_tok.line )
      # source installed by walk-with scope phase now
      result
    end

    def new_defs(val, vofs)
      #recv, name, args, body = val[1], val[4], val[6], val[7]
      def_tok = val[vofs ] # a DefnNameToken
      recv = val[vofs + 1]
      name_tok = val[vofs + 4]
      args = val[vofs + 6]
      body = val[vofs + 7]
      k_end = val[vofs + 8]   # yacc_value for kEND synthesized EOF
      if k_end._equal?( :tEOF )
	msg = 'syntax error, unexpected $end, expecting kEND'
	line = self.line_for(name_tok)
	msg << ", for   def    near line #{line} "
	msg << self.last_closed_def_message
	syntax_error(msg)
      end

      #body ||= s(:block)
      #body = s(:block, body) unless body.first == :block  
      if body._equal?(nil)
	body = RubyBlockNode.s( [] )
      elsif body.class._equal?(RubyBlockNode)
	# ok
      else
	body = RubyBlockNode.s( [ body ] )  # s(:block, body)
      end

      result = RubyDefsNode.s(name_tok.symval, args, body) # s(:defs )
      result.receiver=(recv) 
      result.src_offset=( def_tok.src_offset )
      result.start_line=( def_tok.line )
      # source installed by walk-with scope phase now
      result
    end

    def new_for( expr, var, body )
      #result = s(:for, expr, var)
      #result << body if body
      #result
      res = RubyForNode.s(expr, var, body)
      res.srcOffset=(  var.srcOffset )
      res
    end

    def new_if( carg, t, f )
      # c = cond c
      # c, t, f = c.last, f, t if c[0] == :not
      # s(:if, c, t, f)
      c = carg.as_cond(self)
      if c.class._equal?(RubyNotNode)
	res = RubyIfNode.s( c.conditionNode, f, t)  # s(:if )
      else
	res = RubyIfNode.s( c, t, f)  # s(:if )
      end
      res
    end

    def new_iter(args, body)
      if args.class._equal?( RubyGlobalAsgnNode) 
	lnum = self.line_for_offset( args.src_offset )
	msg = 'assignment to global variable not supported as block arg, '
	msg << "near line #{line} "
	syntax_error(msg)
      end
      RubyIterRpNode.s( args, body ) 
    end

    def new_parasgn(lhs, src_ofs)
      src_line = 1
      if @mydebug 
        if src_ofs._not_equal?(nil) 
	  # src_ofs is zero based
	  src_line = @lexer.src_line_for_offset(src_ofs + 1)
        end
      end
      if lhs._equal?(nil)
	internal_error("lhs is nil in new_parasgn")
      end
      n = RubyParAsgnRpNode.s(lhs, src_line)
      unless src_ofs._equal?(nil)
        n.src_offset=( src_ofs)
      end
      n
    end

    def masgn_append_arg(lhs, rhs)
      rhs = value_expr(rhs)
      lhs.masgn_append_arg( rhs )
    end

    def masgn_append_mrhs(lhs, rhs)
      rhs = value_expr(rhs)
      # lhs.delete_at 1 if lhs[1].nil?  # MRI compat, no delete
      # lhs << rhs
      lhs.append_mrhs( rhs )
    end

    def new_module( val, vofs ) 
      # line, path, body = val[1], val[2], val[4]  # line deleted from .y
      path = val[vofs + 1]
      body = val[vofs + 3]
      k_end = val[vofs + 4]   # yacc_value for kEND synthesized EOF
      if k_end._equal?( :tEOF )
	msg = 'syntax error, unexpected $end, expecting kEND'
	line = self.line_for(path)
	msg << ", for   module    near line #{line} "
	msg << self.last_closed_def_message
	syntax_error(msg)
      end
      # body = s(:scope, body).compact  
      # AST does not use scope nodes

      # result = s(:module, path, body)
      result = RubyModuleNode.s(path, body, @lexer.source_string)
      result
    end

    def new_op_asgn(val, vofs)
      # lhs, asgn_op, arg = val[0], val[1].to_sym, val[2]
      lhs = val[vofs]
      # val[1] should be a RpNameToken
      asgn_sel_tok = val[vofs + 1] 
      asgn_op = asgn_sel_tok.symval
      arg = val[vofs + 2]
			  # lhs should be a RubyAssignableNode
      new_lhs = lhs.as_accessor 
      arg = arg.kbegin_value

      if asgn_op._equal?(:"||")
	lhs.node_assign_set_rhs(arg)
	# s(:op_asgn_or, self.gettable(name), lhs)
	res = RubyOpAsgnOrNode.s(  new_lhs, lhs )
	res.src_offset=( asgn_sel_tok.src_offset )
      elsif asgn_op._equal?(:"&&")
	lhs.node_assign_set_rhs(arg)
	# s(:op_asgn_and, self.gettable(name), lhs)
	res = RubyOpAsgnAndNode.s( new_lhs, lhs )
	res.src_offset=( asgn_sel_tok.src_offset )
      else
	# see original code   # 
	call = new_call_1(new_lhs, asgn_sel_tok,  arg) 
	lhs.node_assign_set_rhs( call )
	res = lhs
      end
      res
    end

    def new_regexp(val, vofs)  
      # node = val[1] || s(:str, '')
      options = val[vofs + 2] # an Array, lexer yacc_value for tREGEXP_END

      # o, k = 0, nil  # k not used
      o = 0

      have_once = false
      opt_idx = 0
      opt_len = options.length
      encod = 0  # select last of multiple encoding specifiers , Trac 565
      while opt_idx < opt_len
	ch = options[opt_idx]
	if ch <= ?n 
	  if ch._equal?( ?i) ; o = o | IGNORECASE
	  elsif ch._equal?( ?m) ; o = o | MULTILINE
	  elsif ch._equal?( ?n) ; encod = ENC_NONE
	  elsif ch._equal?( ?e) ; encod = ENC_EUC 
	  else
	    err_str = ' ' ; err_str[0] = ch 
	    syntax_error( "unknown regexp option: #{err_str}"  )
	  end 
	else 
	  if ch._equal?( ?x ) ; o = o | EXTENDED  
	  elsif ch._equal?( ?o) ; o = o | ONCE ; have_once = true
	  elsif ch._equal?( ?s) ; encod = ENC_SJIS
	  elsif ch._equal?( ?u) ; encod = ENC_UTF8
	  else
	    err_str = ' ' ; err_str[0] = ch 
	    syntax_error("unknown regexp option: #{err_str}" )
	  end 
	end
	opt_idx += 1
      end
      o = o | encod
      argnode = val[vofs + 1]
      arg_cls = argnode.class
      if  arg_cls._equal?(RubyStrNode) 
	# simple regexp, don't care about have_once because no substitutions
	node = nil
	begin
	  str = argnode.strNodeValue
	  rxlit = Regexp.__new( str, o, nil)
	  node = RubyRegexpNode.s(rxlit)
	rescue RegexpError => ex
	  regex_beg_tok = val[vofs]  # for tREGEXP_BEG
	  line = self.line_for(regex_beg_tok)
	  msg = "near line #{line}, #{ex.message} " 
	  syntax_error(msg)
	end
	return node
      end
      if argnode._equal?(nil)
	rxlit = Regexp.__new('', o, nil)
	node = RubyRegexpNode.s(rxlit)
	return node
      end
      node = have_once ? RubyDRegexpOnceNode._new : RubyDRegexpNode._new
      if arg_cls._equal?(RubyDStrNode) # when :dstr 
	node.list=( argnode.list )
      else
	d_list = [ RubyStrNode.s(''), argnode ]
	node.list=(d_list)
      end
      node.options=(o)
      node
    end

    def new_sclass( val, vofs)  
      # recv, in_def, in_single, body = val[3], val[4], val[6], val[7]
      cls_token = val[vofs] 
      recv = val[vofs + 2]
      in_def = val[vofs + 3]
      in_single = val[vofs + 5]
      body = val[vofs + 6]
      k_end = val[vofs + 7]   # yacc_value for kEND synthesized EOF
      if k_end._equal?( :tEOF )
	msg = 'syntax error, unexpected $end, expecting kEND'
        line = self.line_for(cls_token)
	msg << ", for   class   near line #{line} "
	msg << self.last_closed_def_message
	syntax_error(msg)
      end

      # scope = s(:scope, body).compact  # scope not used in AST
      # result = s(:sclass, recv, scope)
      result = RubySClassNode.s(recv, body)
      result.src_offset=( cls_token.src_offset )

      @in_def = in_def
      @in_single = in_single
      result
    end

    def new_super( val , vofs ) 
      sel_tok = val[vofs]
      args = val[vofs + 1]
      aryCls = RubyRpCallArgs 
      arg_cls = args.class
      if args._equal?(nil)
	# maybe this should be zsuper ??, but that's not what rp202 does
	res = RubySuperNode.s( aryCls._new  )
      elsif arg_cls._equal?(RubyBlockPassNode) # args[0] == :block_pass then
	res = RubySuperNode.s( aryCls._new  )
	res.iter=(args)
      elsif arg_cls._equal?(aryCls)
	res = RubySuperNode.s( args )
      else
	internal_error("new_super, invalid args")
      end
      res.src_offset=( sel_tok.src_offset )
      res
    end

    #def new_undef(n, m = nil)
    #  if m then
    #    block_append(n, s(:undef, m))
    #  else
    #    s(:undef, n)
    #  end
    #end

    def new_undef(sym_node)
      RubyUndefNode.s(sym_node.symNodeValue )
    end

    def append_undef( blk , nam )
      ud = new_undef(nam)
      block_append( blk , ud )
    end

    def new_until(block, expr)
      # expr = (expr.first == :not ? expr.last : s(:not, expr))
      if expr.class._equal?(RubyNotNode)
	expr = expr.conditionNode
      else
	expr = RubyNotNode.s(expr)
      end
      new_while( block, expr)
    end

    def premature_eof( name_tok )
      msg = 'syntax error, unexpected $end, expecting kEND'
      if name_tok.class._equal?(RpNameToken)
        line = self.line_for(name_tok)
        msg << ", for   #{name_tok.symval} near line #{line}, "
      end
      msg << self.last_closed_def_message
      syntax_error(msg)
    end

    def new_while( block, expr)
      preBool= true  # argument value from .y was always true
      # block, pre = block.last, false if block && block[0] == :begin
      if block.class._equal?(RubyBeginNode)
	block = block.kbegin_value
	preBool = false
      end
      expr = cond( expr)

      # result = if expr.first == :not then
      #           s(:until, expr.last, block, pre)
      #         else
      #           s(:while, expr, block, pre)
      #         end
      if expr.class._equal?(RubyNotNode) 
	result = RubyUntilNode.s( expr.conditionNode, block, preBool)
      else
	result = RubyWhileNode.s(expr, block, preBool)
      end
      result
    end

    def new_xstring( str)
      if str._equal?(nil)
	res = RubyXStrNode.s( '' )
      else
	# might need become on this path ???
	knd = str.str_dstr_evstr_kind
	if knd._equal?(0) # when :str
	  res = RubyXStrNode.s( str.strNodeValue ) # str[0] = :xstr
	elsif knd._equal?(1) #  when :dstr
	  res = RubyDXStrNode._new 
	  res.list=( str.list )
	else
	  res = RubyDXStrNode._new
	  res.list=( [ RubyStrNode.s('') , str ] )
	end
      end
      res
    end

    def new_yield(args )
      args_cls = args.class
    
      if args._equal?(nil)
	#  args ||= s(:arglist) , i.e. zero args
	cArgs = RubyRpCallArgs._new #  list left as nil 
      else
	args_cls = args.class
	if args_cls._equal?(RubyRpCallArgs)
	  cArgs = args.as_yield_args
	elsif args_cls._equal?(RubyBlockPassNode)
	  syntax_error("Block argument should not be given." )
	else
	  internal_error("new_yield, unrecognized args")
	end
      end
      # not sure how  setArrayWrapper  logic to be handled, nextNodeForParser:
    
      # s(:yield, *args[1..-1])
      return RubyYieldNode.s( cArgs )
    end

    def new_yield_0
      return RubyYieldNode.s( RubyRpCallArgs._new ) 
    end

    def node_assign(lhs, rhs) # TODO202: rename new_assign
      return nil unless lhs

      rhs = self.value_expr(rhs)

      # case lhs[0]
      #  # cannot find any generation of  :dasgn_curr , # Ryan says delete it
      #      but it is same as :dasgn for Maglev
      # when :gasgn, :iasgn, :lasgn, :dasgn,
      #  :masgn, :cdecl, :cvdecl, :cvasgn then
      #  lhs << rhs
      # when :attrasgn, :call then
      #  args = lhs.pop unless Symbol === lhs.last
      #  lhs << arg_add(args, rhs)
      # when :const then
      #  lhs[0] = :cdecl
      #  lhs << rhs

      new_lhs = lhs.node_assign_set_rhs(rhs)  # may raise error
      unless new_lhs._equal?(lhs)
	# :const to :cdecl conversion, or similar
	new_lhs._become(lhs) 
      end
      lhs
    end

    def parse(str, load_name)
      unless str._isString
	raise ArgumentError, 'expected a string'
      end
      @file_name = load_name  # used for __FILE__
      @source_string = str 
      @lexer.install_source( str )
      ast = self._racc_do_parse_rb()
      err_count = @syntax_err_count
      if err_count._not_equal?(0)
	syntax_error("#{err_count.to_s} syntax errors")
      end
      ast 
    end

    # deleted  remove_begin,  
    #  replaced by kbegin_value implemented in RubyNode and RubyBeginNode
    #def remove_begin node
    #  oldnode = node
    #  if node and :begin == node[0] and node.size == 2 then
    #    node = node[-1]
    #  end
    #  node
    #end

    def ret_args( node )
      # node should be a  RubyRpCallArgs
      if node.iter._not_equal?(nil)
	syntax_error("block argument should not be given")
      end
      lst = node.list
      if lst.size._equal?(1)
	node = lst[0]
      end
      node
    end

    def value_expr(oldnode)   # HACK
      # node = remove_begin oldnode
      # node[2] = value_expr(node[2]) if node and node[0] == :if
      # node  # e_nd of  original code
      #
      oldnode.kbegin_value
    end

    def void_stmts(node)
      return nil unless node

      # return node unless node[0] == :block
      # node[1..-1] = node[1..-1].map { |n| remove_begin(n) }
      node.blockNode_listdo_kbegin_value

      node
    end

    def warning( s)
      msg = "RpWarning: "
      msg << s 
      if @rpwarnings
        puts(msg)
      else
        arr = @save_warnings 
        if arr._equal?(nil)
          arr = [ msg ]
          @save_warnings = arr
        else
          arr << msg
        end
      end
    end

    def print_saved_warnings
      arr = @save_warnings
      if arr._not_equal?(nil)
        arr.each { | str | puts(str) }
      end
      @save_warnings = nil
    end

    def internal_error(msg)  # was raise_error
      print_saved_warnings
      puts "InternalParseError: #{msg} during #{@file_name}"
      if @debuglevel > 2
	nil.pause  
      end
      raise InternalParseError, msg
    end

    def syntax_error(msg)
      print_saved_warnings
      raise SyntaxError, "#{msg} of #{@file_name}"
    end 

    def backref_assign_error( a_val)  # method missing from rp202 code
      syntax_error( "backref_assign_error" )
    end

    def string_to_symbol(str)
      unless str._isString
        internal_error('expected value to be a String')
      end
      if str.size._equal?(0)
        yyerror( 'empty symbol literal' )
      end
      if str.__indexOfByte(0, 1)._not_equal?(0)
        yyerror( 'symbol string may not contain `\\0\' ')
      end
      str.__as_symbol
    end

  end  # }
end  #  }
