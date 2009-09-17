module MagRp 

  class RubyNode

    # do not use Ruby new , we want only Smalltalk instance creation
    # semantics, all instance creation in this file should use _new .
    class_primitive_nobridge '_new', '_basicNew'

    # _becomeMinimalChecks:  comes from Object
    primitive_nobridge '_become', '_becomeMinimalChecks:'

    def raise_error(msg = "")
      puts ("InternalParseError: " << msg)
      nil.pause  # TODO delete
      raise InternalParseError, "RubyNode invalid new"
    end
    def self.raise_error(msg = "")
      puts ("InternalParseError: " << msg)
      nil.pause  # TODO delete
      raise InternalParseError, "RubyNode invalid new"
    end

    def self.s
      raise_error("missing constructor")
    end
    def inspect
      raise_error("missing implem of inspect")
    end

    def kbegin_value
      self
    end
    def ifNode_kbegin_remove2
      self
    end
    # def line ; end # not used
    # def line=(a_line) ; end # not used
    
    def src_offset=(ofs)
      # returns receiver
      if ofs._isFixnum   # TODO remove check
        @position = ofs + 1 # convert to one-based
      else
        raise_error('invalid source offset') 
      end  
    end

    def srcOffset
      @position  # result is one based 
    end
    def src_offset
      # result is zero based 
      p = self.srcOffset
      if p.equal?(nil)
        raise_error('missing source offset') 
        return nil
      end
      p - 1
    end
 
    def srcOffset=(ofs)
      # returns receiver , arg is one based
      if ofs._isFixnum
        @position = ofs  # already one based
      else
        raise_error('invalid source offset')
      end
    end

    def blockNode_listdo_kbegin_value
      self
    end

    def prepend_to_block(val)
      list = [ val, self ]
      return RubyBlockNode.s( list )
    end

    def node_assign_set_rhs(rhs)
      raise_error("invalid lhs for node_assign ")
      nil
    end
    def isEmptyBlockNode
      false
    end
    def str_dstr_evstr_kind
      nil
    end
    def as_cond(aMagRp)
      self
    end
    def list
      []
    end
  end

  class RubyKBeginNode < RubyNode
    # instances not present in AST but used during parser execution
    def self.s(val)
      res = self._new
      res.init(val)
    end
    def init(val)
      @value = val
      self
    end 
    def kbegin_value
      @value
    end
  end


         class RubyBackRefNode
           # newForName: returns either an RubyNthRefNode or a RubyBackRefNode
           class_primitive_nobridge 's', 'newForName:'
           def inspect
             "[:back_ref , #{@selector.inspect} ]"
           end
         end

         class RubyNthRefNode
           primitive_nobridge 'number=', 'matchNumber:'
           class_primitive_nobridge 's', 's_forRp:'
           def inspect
             "[:nth_ref, #{@matchNumNode.inspect}]"
           end
         end


         class RubyMatch2Node
           def self.s(regexNode, rcvrNode)
             res = self._new
             res.init(regexNode, rcvrNode)
           end
           def init(regexNode, rcvrNode)
             @valueNode = regexNode
             @receiverNode = rcvrNode
             self
           end
           def inspect 
             "[:match2, #{@receiverNode.inspect} , #{@valueNode.inspect}]"
           end
         end

         class RubyMatchZeroNode
           # s(:match ) --> :match2,   produces a RubyMatch2Node 
           class_primitive_nobridge 's', 's_forRp:' # one arg, a regexNode"
           def inspect
             raise_error # expect no instances
           end
         end

       class RubyAbstractWhileNode
         class_primitive_nobridge 's', 's_forRp:block:bool:'
       end
         class RubyUntilNode
           # s_forRp:block:bool: inherited from RubyAbstractWhileNode
           def inspect
             "[:until, @{conditionNode.inspect}, @{bodyNode.inspect}, #{@selector}]"
           end
         end
         class RubyWhileNode
           # s_forRp:block:bool: inherited from RubyAbstractWhileNode
           def inspect
             "[:while, @{conditionNode.inspect}, @{bodyNode.inspect}, #{@selector}]"
           end
         end

       class RubyAliasNode
         def self.s(newnam, oldnam)
           res = self._new
           res.init(newnam, oldnam) 
         end
         def init(newnam, oldnam)
           @newName = newnam # a RubySymbolNode
           @oldName = oldnam # a RubySymbolNode
           self
         end
         def inspect
           "[:alias, @{oldName.inspect}, @{newNam.inspect} ]"
         end
       end

       class RubyGlobalVarAliasNode
         def self.s(newnam, oldnam) # inherited from RubyAliasNode
           n = RubySymbolNode.s(newnam)
           o = RubySymbolNode.s(oldnam)
           res = self._new
           res.init(n, o)
         end
         # init inherited from RubyAliasNode
       end

       class RubyAndNode
         def self.s( left, right )
           res = self._new
           res.firstNode=(left)
           res.secondNode=(right)
           res
         end
         primitive_nobridge 'secondNode', 'secondNode'
         primitive_nobridge 'firstNode=', 'firstNode:'
         primitive_nobridge 'secondNode=', 'secondNode:'
         def as_cond(aMagRp)
           #  when :and then      # RubyAndNode
           #    return s(:and, cond_or_nil(node[1]), cond_or_nil(node[2]))
           RubyAndNode.s( aMagRp.cond_or_nil(@firstNode), aMagRp.cond_or_nil(@secondNode))
         end
         def inspect
           "[:and, #{@firstNode.inspect}, #{@secondNode.inspect}]"
         end
       end


       class RubyAttrAssignNode
          def self.s(rcvr, sel, args)
            res = self._new
            res.init(rcvr, sel, args)
          end
          def self.s_tk(rcvr, name_tok, args)
            res = self._new
            sel = name_tok.symval.to_s 
            sel << "=" 
            sel = sel._as_symbol
            res.init(rcvr, sel, args)
            res.src_offset=( name_tok.src_offset)
            res
          end
          def init(rcvr, sel, args)
            @receiverNode = rcvr
            @name = sel
            @argsNode = args
            self
          end
          def node_assign_set_rhs(rhs)
            args = @argsNode
            if args.equal?(nil)
              @argsNode = RubyRpCallArgs.s( rhs )
            else  
              args.append( rhs )
            end
            self
          end
          def inspect
            "[:attrasgn, #{@receiverNode.inspect}, #{@name}, #{@argsNode.inspect}]"
          end
       end

       class RubyCallNode
         class_primitive_nobridge 's', 's_forRp:sel:args:'  # assumes rcvr is self
         primitive_nobridge 'iter=', 'iterNode:'
         def node_assign_set_rhs(rhs)
           # append rhs to args list of this call
           if rhs.equal?(nil)
             raise_error("invalid nil arg")
           end
           @argsNode.append(rhs) 
           self
         end
         def rcvr
           @receiverNode
         end
         def inspect
           "\n  [:call, #{@receiverNode.inspect}, :#{@callName}, #{@argsNode.inspect}]" 
         end
       end

       class RubyVCallNode
         class_primitive 's', 's_forRp:selector:'
         def inspect
           "\n  [:vcall, #{@rcvrNode.inspect}, :#{@callName}]"
         end
       end

       class RubyFCallNode
         # receiverNode is alway self
         class_primitive_nobridge 's', 's_forRp:sel:args:'  # assumes rcvr is self
         class_primitive_nobridge 's', 's_forRp:args:'  # synthesizes SelfNode for rcvr
         primitive_nobridge 'iter=', 'iterNode:'
         def inspect
           "\n  [:fcall, :#{@callName}, #{@argsNode.inspect} ]"
         end
       end

       class RubyDotNode
         def self.s(sym, a, b)
           res = self._new
           res.init(sym, a, b)
         end
         def init(sym, a, b)
           @beginNode = a
           @endNode = b
           @exclusive = sym.equal?(:dot3)
           self
         end
         def as_cond(aMagRp)
           #  when :dot2 then     # RubyDotNode
           #    label = "flip#{node.hash}"
           #    env[label] = :lvar
           #    return s(:flip2, node[1], node[2])
           #  when :dot3 then     # RubyDotNode
           #    label = "flip#{node.hash}"
           #    env[label] = :lvar
           #    return s(:flip3, node[1], node[2])
	   label = "flip#{self.hash}"
	   env = aMagRp.env
	   env[label] = :lvar 
           if @exclusive  # dot3
             raise_error # AST has never seen a flip3 yet
             nil
           else   # dot2 
             RubyFlipNode.s( @beginNode , @endNode ) # s(:flip2 )
           end
         end
         def inspect
           if @exclusive 
             sym = :dot3 
           else 
             sym =  :dot2
           end
           "[:#{sym}, #{@beginNode.inspect}, #{@endNode.inspect}]" 
         end
       end

       class RubyEnsureNode
         def self.s(body, ensure_body)
           res = self._new
           res.init(body, ensure_body)
         end
         def init(body, ensure_body)
           @bodyNode = body
           @ensureNode = ensure_body
           self
         end
         def inspect
           "[:ensure, #{@bodyNode.inspect}, #{@ensureNode.inspect}]"
         end
       end

       class RubyHashNode
         primitive_nobridge 'listNode=', 'listNode:'
         def self.s(list)
           lst_cls = list.class
           res = self._new
           if lst_cls.equal?(RubyArrayNode)
             res.listNode=(list)
           elsif lst_cls.equal?(RubyRpCallArgs)
             ary = RubyArrayNode._new 
             ary.list=(list.list)
             res.listNode=(ary)
           else 
             raise_error("RubyHashNode.s bad arg")
           end
           res
         end
         def inspect
           "[:hash, #{@listNode.inspect} ]"
         end
       end

       class RubyIfNode
         def self.s(cond, tb, eb)
           res = self._new
           res.init(cond, tb, eb) 
         end
         def init(cond, tb, eb)
           @condition = cond
           @thenBody =  tb
           @elseBody =  eb
           self
         end
         def ifNode_kbegin_remove2
           then_body = @thenBody
           if then_body._not_equal?(nil)
             ntb = then_body.kbegin_value
             @thenBody = ntb
             ntb.ifNode_kbegin_remove2
           end
         end
         def inspect
           "[:if, #{@condition.inspect}, #{@thenBody.inspect}, #{@elseBody.inspect}]"
         end
       end


       class RubyMethodDefNode
         class_primitive_nobridge 's', 's_forRp:args:body:'
         primitive_nobridge 'start_line=', 'startLine:' 
       end
         class RubyDefnNode
           # def self.s # inherited
           def inspect
             "\n[:defn, #{@nameNode.inspect}, #{@argsNode.inspect}, #{@bodyNode.inspect}]"
           end
         end
         class RubyDefsNode
           primitive_nobridge 'receiver=', 'receiverNode:'
           # def self.s # inherited
           def inspect
             "[:defs, #{@receiverNode.inspect}, #{@nameNode.inspect}, #{@argsNode.inspect}, #{@bodyNode.inspect}]"
           end
         end

       class RubyModuleNode
         class_primitive_nobridge 's', 's_forRp:body:source:'
	 def inspect
	   "[:module , #{@cpath.inspect},  #{@bodyNode.inspect}]"   
	 end
       end

         class RubyClassNode
           class_primitive_nobridge 's', 's_forRp:superCls:body:source:'
           def inspect   
             "[:class , #{@cpath.inspect}, #{@superNode.inspect}, #{@bodyNode.inspect}]"
           end
         end 

       class RubyNotNode
         def self.s(arg)
           res = self._new
           res.init(arg)
         end        
         def init(arg)
           @conditionNode = arg
           self
         end
         primitive_nobridge 'conditionNode', 'conditionNode'
         def inspect
           "[:not, #{@conditionNode.inspect}]"
         end
       end

       class RubyOpAsgnNode
         primitive_nobridge 'receiverNode=', 'receiverNode:'
         primitive_nobridge 'valueNode=', 'valueNode:'
         primitive_nobridge 'initSelectors', 'initAsgnSel:opSel:'
         def self.s(rcvr, asgnSelTok, opSelTok, val)
           res = self._new
           asgn_sel = (asgnSelTok.symval.to_s << '=' )._as_symbol
           res.initSelectors(asgn_sel, opSelTok.symval)
           res.receiverNode=(rcvr)
           res.valueNode=(val)
           res.src_offset=( asgnSelTok.src_offset ) 
           res
         end
         def inspect
           "[:op_asgn2 , #{@receiverNode.inspect}, :#{@variableAsgnCallName}, :#{@operatorCallName}, #{@valueNode.inspect} ]"
         end
       end

       class RubyOpAsgnAndNode
         def self.s(first, second)
           res = self._new
           res.init(first, second)
         end
         def init(first, second)
           @firstNode = first
           @secondNode = second
           self 
         end
         def inspect
           "[:op_asgn_and, #{@firstNode.inspect}, #{@secondNode.inspect} ]"
         end 
       end

       class RubyOpAsgnOrNode
         def self.s(first, second)
           res = self._new
           res.init(first, second)
         end
         def init(first, second)
           @firstNode = first
           @secondNode = second
           self 
         end
         def inspect
           "[:op_asgn_or, #{@firstNode.inspect}, #{@secondNode.inspect} ]"
         end 
       end

       class RubyOpElementAsgnNode
         def self.s(rcvr, args, asgn_tok, val)
           res = self._new
           res.init( rcvr, args, asgn_tok, val) 
         end
         def init(rcvr, args, asgn_tok, val)
           @receiverNode = rcvr
           @argsNode = args
           @callName = asgn_tok.symval # the methodName
           @valueNode = val
           self.src_offset=( asgn_tok.src_offset )
           self
         end
         def inspect
           "[:op_asgn1, #{@receiverNode.inspect}, #{@argsNode.inspect} :#{@callName}, #{@valueNode.inspect}]" 
         end
       end

       class RubyOrNode
         def self.s( left, right )
           res = self._new
           res.firstNode=(left)
           res.secondNode=(right)
           res
         end
         primitive_nobridge 'secondNode', 'secondNode'
         primitive_nobridge 'firstNode=', 'firstNode:'
         primitive_nobridge 'secondNode=', 'secondNode:'
         def as_cond(aMagRp)
           #  when :or then
           #    return s(:or,  cond_or_nil(node[1]), cond_or_nil(node[2]))
           RubyOrNode.s( aMagRp.cond_or_nil(@firstNode), aMagRp.cond_or_nil(@secondNode))
         end
         def inspect
           "[:or, #{@firstNode.inspect}, #{@secondNode.inspect}]"
         end
       end


       class RubyRescueNode
         def self.s(body, rescuebody, elsebody)
           unless rescuebody.class.equal?(RubyRescueBodyNode)
             raise_error # bad arg
           end
           res = self._new
           res.init(body, rescuebody, elsebody)
         end
	 def init(body, rescuebody, elsebody)
	   @bodyNode = body
	   @rescueBodyNode = rescuebody
	   @elseNode = elsebody
           self
         end   
         def inspect
          "[:rescue, #{@bodyNode.inspect}, <rescue>#{@rescueBodyNode.inspect}, <else> #{@elseNode.inspect}]" 
         end
       end

       class RubySClassNode
         class_primitive_nobridge 's', 's_forRp:body:'
         def inspect
          "[:sclass , #{@receiverNode.inspect} , #{@bodyNode.inspect}]"
         end
       end

       class RubySuperNode
         primitive_nobridge 'args=', 'argsNode:'
         primitive_nobridge 'iter=', 'iterNode:'
         def self.s(args)
           res = self._new
           res.args=(args)
           res
         end
         def inspect
           "\n  [:super, #{@argsNode.inspect}, #{@iterNode.inspect} ]" 
         end
       end

       class RubyUndefNode
         primitive_nobridge 'name=', 'name:'
         def self.s(nam)
           res = self._new
           res.name=(nam)
           res
         end
         def inspect
           "[:undef , #{@name} ]"
         end
       end


       class RubyYieldNode
         primitive_nobridge 'argsNode=', 'argsNode:'
         def self.s(args)
           res = self._new
           res.argsNode=(args)
           res 
         end
         # not sure how we are getting @arrayWrapper bool from RP
         def inspect
           "[:yield , #{@argsNode.inspect}, arrayWrapper:#{@arrayWrapper.inspect}]"
         end
       end

       class RubyZSuperNode
         # def self._new ;  inherited from RubyNode
         # end
         def inspect
           ":zsuper"
         end
       end

       class RubyAbstractBreakNode
         primitive_nobridge 'valueNode=', 'valueNode:'
	 def self.s(val)
	   res = self._new
	   res.valueNode=(val)
	   res
	 end
       end

         class RubyBreakNode
           # def self.s(val) ; end # inherited from RubyAbstractBreakNode
           def inspect
             "[:break, #{@valueNode.inspect}]"
           end
         end

         class RubyNextNode
           # def self.s(val) ; end # inherited from RubyAbstractBreakNode
           def inspect
             "[:next, #{@valueNode.inspect}]"
           end
         end
       class RubyRedoNode
         # no args, self._new inherited  from RubyNode
         def inspect
           "[:redo ]"
         end
       end

       class RubyRetryNode
         # no args, self._new inherited  from RubyNode
         def inspect
           "[:retry ]"
         end
       end

     class RubyAbstractLiteralNode
       def self.s(*args)
         self.raise_error
       end
     end
       class RubyAbstractNumberNode
         class_primitive 'value_to_number', 'valueToNumber:' 
         def self.s(val)
           if val._isFixnum
             res = RubyFixnumNode._new
             res.init(val)
           elsif val._isFloat
             res = RubyFloatNode._new
             res.init(val)
           elsif val._isInteger
             res = RubyFixnumNode._new  # FixnumNode handles any Integer
             res.init(val)
           elsif val._isString
             val = self.value_to_number(val)
             if val._isFixnum
               res = RubyFixnumNode._new
             else
               res = RubyFloatNode._new
             end
             res.init(val)
           else
             raise_error("invalid arg to RubyAbstractNumberNode")
             res = nil
           end 
           res
         end
       end
         

         class RubyFixnumNode
           # RubyFixnumNode may hold either Fixnum or Bignum value
           # instance created by RubyAbstractNumberNode
           def init(num)
             @value = num 
             self
           end
           def inspect
             "[:lit, #{@value}]"
           end
         end

	 class RubyFloatNode
           # instance created by RubyAbstractNumberNode
           def init(num)
             @value = num
             self
           end
           def inspect
             "[:lit, #{@value}]"
           end
         end
       class RubyFalseNode
         # def self._new ;  inherited from RubyNode
         # end
         def inspect
           ":false"
         end
       end

       class RubyNilNode
         # def self._new ;  inherited from RubyNode
         # end
         def inspect
           ":nil"
         end
       end

       class RubyRegexpNode
         def as_cond(aMagRp)
           #  when :regex then  #  :regex is used in Rubinius only
           #    return s(:match, node)
           #  when :lit then       # we have RubyRegexpNode here
           #    if Regexp === node.last then
           #      return s(:match, node)
           #    else
           #      return node
           #    end
           return RubyMatchZeroNode.s(self)  # s(:match ) --> :match2
         end
         class_primitive_nobridge 's', 's_forRp:'
         def inspect
           "[:regex, #{@regexpLit.inspect}]"
         end
       end

       class RubyStrNode
         primitive_nobridge 'value=', 'value:'
         def self.s(val)
           unless val._isString
             raise_error("RubyStrNode arg not a String")
           end
           res = self._new
           res.value=(val)
           res
         end
         def strNodeValue
           @value
         end
         def appendString(aString)
           unless aString._isString
             raise_error("RubyStrNode appendString arg not a String")
           end
           @value << aString
         end
         def str_dstr_evstr_kind
           0
         end
         def inspect
           "[:str, #{@value.inspect} ]"
         end
       end

         class RubyXStrNode
           # def self.s # inherited from RubyStrNode
           def inspect
             "[:xstr, #{@value.inspect} ]"
           end
         end

       class RubySymbolNode
         def self.s(the_sym )
           unless the_sym._isSymbol ; raise_error; end
           res = self._new
           res.init(the_sym)
         end
         def init(the_sym)
           @name = the_sym
           self
         end 
         def strNodeValue
           @name
         end
         def symNodeValue
           @name
         end
         def inspect
           ":#{@name}"
         end
       end

       class RubyTrueNode
         # def self._new ;  inherited from RubyNode
         # end
         def inspect
           ":true"
         end
       end

       class RubyClassVarNode
         primitive_nobridge 'name=', 'name:'
         def self.s(sym)
           res = self._new
           res.name=(sym)
           res 
         end
         def inspect
           "[:cvar, :#{@name} ]"
         end
       end

       class RubyColon3Node
         def self.s(name_tok)
           res = self._new
           res.name=(name_tok.symval)
           res.src_offset=( name_tok.src_offset )
           res
         end
         primitive_nobridge 'name=', 'name:'
         def inspect
           "[:colon3, #{@name}]"
         end
       end

         class RubyColon2Node
           primitive_nobridge 'leftNode=', 'leftNode:'
           def self.s(left, name_tok)
             res = self._new
             res.leftNode=(left) 
             res.name=(name_tok.symval)
             res.src_offset=( name_tok.src_offset )
             res
           end
           def self.simple(a_sym, src_ofs )
             res = self._new
             res.name=(a_sym)
             res.src_offset=( src_ofs )
             res
           end
           def inspect
             "[:colon2, #{@leftNode}, #{@name}]"
           end
         end

           class RubyClassNameNode
             def inspect
               if @isColon3
                 "[:classname, '::', #{@name}]"
               else
                 "[:classname, #{@leftNode}, #{@name}]"
               end
             end
           end

       class RubyConstNode
         def self.s(sym, src_ofs)
           res = RubyColon2Node.simple(sym, src_ofs)
           res
         end
         def node_assign_set_rhs(rhs)
           # caller responsible for become
           ofs = self.src_offset .
           c2n = RubyColon2Node.simple( @name, ofs )
           res = RubyConstDeclNode.s( c2n, rhs)
           res.src_offset=( ofs )
           res
         end
         def inspect
           "[:const, #{@name.inspect}]"
         end
       end

       class RubyGlobalVarNode
         # s_ForRp:  includes SpecialGlobalNodeClasses logic
         class_primitive_nobridge 's', 's_ForRp:'

         def inspect
           "[:gvar, :#{@name}]"
         end
       end

       class RubyInstVarNode
         primitive_nobridge 'name=', 'name:'
         def self.s(sym)
           res = self._new
           res.name=(sym)
           res
         end
         def inspect
           "[:ivar, :#{@name}]"
         end
       end

       class RubyLocalVarNode
         primitive_nobridge 'name=', 'name:'
         def self.s(sym)
           res = self._new
           res.name=(sym)
           res
         end
         def inspect
           "[:lvar, :#{@name}]"
         end
       end


       class RubySelfNode
         # def self._new #  inherited from RubyNode
         # end
         def inspect
           ":self"
         end
       end

     class RubyArgsNode
       # self._new  inherited, all instVars left as nil
       def add_arg(sym)
         unless sym._isSymbol 
           raise_error("add_arg - arg not a Symbol")
         end
         args = @arguments
         if args.equal?(nil)
           args = RubyListNode._new
           @arguments = args
         end
         args.append( RubyArgumentNode.s( sym ) )
         self
       end

       def add_block_arg(arg)
         if arg.equal?(nil)
           # do nothing  ,    opt_f_block_arg term is nil
         else
           if @blockArgNode.equal?(nil)
             if arg.class.equal?( RubyBlockArgNode )
               @blockArgNode = arg
             else
               # expect an RpNameToken
               @blockArgNode = RubyBlockArgNode.s(idTok.symval)
             end
           else
             raise_error('RubyArgsNode - block arg already installed')
           end
         end
         self
       end

       def add_star_arg(sym)
         unless sym._isSymbol 
           raise_error("add_star_arg - arg not a Symbol")
         end
         if @restArgNode.equal?(nil)
           @restArgNode = RubyArgumentNode.s(sym)
         else
           raise_error('RubyArgsNode - star arg already installed')
         end
         self
       end

       def add_optional_arg(node)
         # maybe node is an Array of :lasgn ??
         unless node.class.equal?(RubyBlockNode)
           raise_error("add_optional_arg bad arg kind")
         end
         oblk = @optArgs
         if oblk.equal?(nil)
           @optArgs = node
         else
           raise_error('optional_arg already assigned')
         end 
         self
       end

       def inspect
         res = "[:args "
         unless @arguments.equal?(nil)
           res << @arguments.inspect_list
         end
         unless @restArgNode.equal?(nil)
           res << ', :"*'
           res << @restArgNode.identifier.to_s
         end
         res << "]"
         unless @blockArgNode.equal?(nil)
           res << ', [:block_arg, '
           res << @blockArgNode.name.to_s
           res << '],' 
         end
         res
       end
     end

     class RubyArgumentNode
       primitive_nobridge 'identifier=', 'identifier:'
       primitive_nobridge 'identifier',  'identifier'
       def self.s(sym)
         res = self._new
         res.identifier=(sym) 
         res
       end
       def inspect
         @identifier.inspect
       end
     end


       class RubyClassVarDeclNode
         def self.s(nam, val)
           # used for both :cvasgn and :cvdecl
           res = self._new
           res.init(nam, val)
         end
         def init(nam, val)
           @name = nam
           @valueNode = val
           self
         end
         def as_accessor
           RubyClassVarNode.s(@name)
         end
         def node_assign_set_rhs(rhs)
           if @valueNode.equal?(nil)
             @valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def inspect
           "[:cvasgn, :#{@name}, #{@valueNode}]"
         end
       end

       class RubyConstDeclNode
         def self.s(c2node, value)
           res = self._new
           res.init(c2node, value)
         end
         def init(c2node, value)
           @constNode = c2node
           @valueNode = value
           self
         end
         def node_assign_set_rhs(rhs)
           if @valueNode.equal?(nil)
             @valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def as_accessor
           @constNode.dup		# fix Trac 588 ?
         end
         def inspect
           "[:cdecl, #{@constNode.inspect}, #{@valueNode.inspect}]"
         end
       end

       class RubyDAsgnNode
         def node_assign_set_rhs(rhs)
           if @valueNode.equal?(nil)
             @valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def as_accessor
           raise_error('as_accessor not implemented')
           # probably return a RubyDVarNode
         end
         def inspect
           "[:dasgn, :#{@name}, #{@valueNode}]"
         end
       end

       class RubyGlobalAsgnNode
         #  s_ForRp:value:  includes SpecialGlobalNodeClasses logic
         class_primitive_nobridge 's', 's_ForRp:value:'
         def node_assign_set_rhs(rhs)
           if @valueNode.equal?(nil)
             @valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def as_accessor
           RubyGlobalVarNode.s(@name)
         end
         def inspect
           "[:gasgn, :#{@name}, #{@valueNode}]"
         end
       end

       class RubyGlobalLastExcBackTrace
         def inspect
           '[:gvar_last_ex_bt, :$@ ]'
         end
       end

       class RubyGlobalNotAssignable
         def node_assign_set_rhs(rhs)
           if @valueNode.equal?(nil)
             @valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def inspect
           "[:gasgnNotAssignable, :#{@name}, #{@valueNode}]"
         end
       end

       class RubyGlobalLastExceptionAsgn
         def node_assign_set_rhs(rhs)
           if @valueNode.equal?(nil)
             @valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def inspect
           "[:gasgnLastExc, #{@valueNode}]"
         end
       end

       class RubyInstAsgnNode
         def self.s(nam, val)
           res = self._new
           res.init(nam, val)
         end
         def init(nam, val)
           @name = nam
           @valueNode = val
           self
         end
         def node_assign_set_rhs(rhs)
           if @valueNode.equal?(nil)
             @valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def as_accessor
           RubyInstVarNode.s(@name)
         end
         def inspect
           "[:iasgn, :#{@name}, #{@valueNode}]"
         end
       end

       class RubyLocalAsgnNode
         def self.s(nam, val)
           res = self._new
           res.init(nam, val)
         end 
         def init(nam, val)
           @name = nam 
           @valueNode = val
           @isBlockArg = false
           self
         end 
         def node_assign_set_rhs(rhs)
           if @valueNode.equal?(nil)
             @valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def as_accessor
           RubyLocalVarNode.s(@name)
         end
         def inspect
           if @valueNode._not_equal?(nil)
             "[:lasgn, :#{@name}, #{@valueNode.inspect}]"
           else
             "[:lasgn, :#{@name}]"
           end
         end
       end

       class RubyVcGlobalAsgNode
         def node_assign_set_rhs(rhs)
           if @valueNode.equal?(nil)
             @valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def inspect
           "[:globalAsg, :#{@name}, #{@valueNode.inspect}]"
         end
       end

     class RubyBlockArgNode
       primitive_nobridge 'name' , 'name'
       primitive_nobridge 'name=', 'name:'
       def self.s(sym)
         res = self._new  # inherited
         res.name=(sym)
         res
       end 
       def inspect
         "[:block_arg, #{@name} ]"
       end
     end

     class RubyBlockPassNode
       primitive_nobridge 'bodyNode=', 'bodyNode:'
       def self.s( body)
         res = self._new
         res.bodyNode=(body)
         res
       end
       def inspect
         "[:block_pass , #{@bodyNode.inspect}]"
       end
     end

     class RubyCaseNode
       class_primitive_nobridge 's', 's_ForRp:body:' # s_ForRp: exprNode body: caseBody
       def inspect   
         "[:case, #{@caseNode.inspect}, #{@caseBody.inspect}]"
       end
     end

     class RubyDefinedNode
       primitive_nobridge 'expressionNode=', 'expressionNode:'
       def self.s(exprNode)
         res = self._new
         res.expressionNode=(exprNode)
         res
       end
       def inspect
         "[:defined, @{expressionNode.inspect} ]"
       end
     end

     class RubyEvStrNode
       primitive_nobridge 'body=', 'body:'
       def self.s(body)  # body is a RubyNode
         res = self._new
         res.body=(body)
         res
       end
       def str_dstr_evstr_kind
	 2
       end
       def evStrBody
         @body
       end
       def evStrBodyIsStrNode
         @body.class.equal?(RubyStrNode)
       end
       def inspect
        "[:evstr, #{@body.inspect}]"
       end
     end

     class RubyFlipNode 
       def self.s(first, second)
         res = self._new
         res.init(first, second)
       end
       def init(first, second)
         @firstNode = first
         @secondNode = second
         self
       end
       def inspect
         "[:flip2, #{@firstNode.inspect}, #{@secondNode.inspect}]"
       end
     end

     class RubyIterRpNode
       def self.s(args, body)  # call installed later
         res = self._new
         res.init(args, body)
       end
       def init(args, body)
         # @callNode left as nil
         @varNode = args    # further processing  during walk, in astAnalyzeArgs
         @bodyNode = body
         self
       end
       def call=(aCallNode)
         if @callNode.equal?(nil)
           @callNode = aCallNode
         else
           raise_error("call node already assigned")
         end 
         @position = aCallNode.srcOffset()  # one-based already
       end
       def inspect
         "[:iterRp, #{@callNode.inspect}, #{@varNode.inspect}, #{@bodyNode.inspect} ]" 
       end
     end

       class RubyForNode
         def self.s( iter, var, body )
           res = self._new
           res.init( iter, var, body)
         end
         def init(iter, var, body)
           @iterNode = iter
           @varNode = var
           @bodyNode = body 
           self
         end
         def inspect
           "[:for, #{@iterNode.inspect}, #{@varNode.inspect}, #{@bodyNode.inspect} ]"
         end
       end

     class RubyListNode
       # _new inherited , @list left as nil
       primitive_nobridge 'list=', 'list:'
       primitive_nobridge 'append', '_append:'  # returns receiver
       def inspect_list
         res = ""
	 if @list
	   sep = ""
	   @list.each { | ea |  
              res << (sep << "#{ea.inspect}") 
              sep = ", "
           }
	 end
	 res
       end
     end


       class RubyArrayNode
         class_primitive_nobridge '_new', '_new' # inits @list to Smalltalk #() 
         primitive_nobridge 'list', 'list'
         primitive_nobridge 'list=', 'list:'
         class_primitive_nobridge 's', '_new:'  # one arg
         class_primitive_nobridge 's', '_new:with:'  # two arg
         primitive_nobridge '_append', '_append:'  # returns receiver
         primitive_nobridge 'appendAll', '_appendAll:'  # returns receiver
         def at(idx)
           @list[idx]  # returns nil if list is empty
         end
         def append(v)
           if v.equal?(nil)
             raise_error('invalid nil arg')
           end
           self._append(v)
           self
         end
         def arrayDup
           res = RubyArrayNode._new
           res.list=( @list.dup )
           res
         end
         def arrayLength
           @list.length
         end
         primitive_nobridge 'prepend' , '_prepend:'  # returns receiver

         def srcOffset
           p = @position
           if p._isFixnum
             return p
           end
           lst = @list
           if lst.size >= 1
             return lst[0].srcOffset
           end
           nil
         end

         def inspect
           self._inspect("array")
         end
         def _inspect(id)
           res = "[:#{id} "
           if @list
             @list.each { | ea |  res << ", #{ea.inspect}" }
           end
           res << "]"
           res
         end
       end

         class RubyRpCallArgs
           # most meths  inherited from RubyArraynode
           #  .mcz contains reimplementation of hasRestArg

           def is_empty
             @list.size.equal?(0) && @iterNode.equal?(nil)
           end

           def append_arg(v)
             if v.equal?(nil)
               raise_error('invalid nil arg')
             end
             if v.class.equal?(RubyBlockPassNode)
               raise_error('must use append_blk_arg ')
             end
             self._append(v)
             self
           end

           def iter
             @iterNode
           end

           def list
             @list
           end

           def append_blk_arg(node)
             if @iterNode.equal?(nil)
               @iterNode = node
             else
               raise_error("block argument already present")
             end
             self
           end
           def as_yield_args
             lst = @list
             if lst.size.equal?( 1) && @iterNode.equal?(nil)
               nod = lst[0]
               if nod.class.equal?( RubySplatNode)
                 return nod  # for   yield *one_arg
               end
             end
             self
           end
           def inspect
             self._inspect("callargs")
           end
         end

       class RubyBlockNode
         def self.s(a_list)
           res = self._new
           res.list=(a_list)  # a_list should be an Array 
           res
         end
         def append_to_block(val)
           @list << val 
         end
         def prepend_to_block(val)
           @list.insert(0, val)
           self
         end
         def isEmptyBlockNode
           lst = @list
           if lst
             lst.size.equal?(0)
           else
             true
           end
         end

         def blockNode_listdo_kbegin_value
           lst = @list
           if lst
             sz = lst.size
             n = 0
             while n < sz
               lst[n] = lst[n].kbegin_value
               n = n + 1 
             end
           end
         end
         def inspect
           res = "\n[:block, "
           if @list
             @list.each { | ea |  res << ", #{ea.inspect}" }
           end
           res << "]"
           res
         end
       end

       class RubyDRegexpNode
         # instance creation via inherited _new
         def as_cond(aMagRp)
           #  when :dregex then
           #    return s(:match2, node, s(:gvar, "$_".to_sym))
           return RubyMatch2Node.s(self, RubyGlobalVarNode.s( :"$_" ))
         end
         primitive_nobridge       'options=', 'options:'
         def inspect
           "[:dregex, #{self.inspect_list} ]"
         end
       end

         class RubyDRegexpOnceNode
           # instance creation via inherited _new
           def inspect
             "[:dregex_once, #{self.inspect_list} ]"
           end
         end

       class RubyDStrNode
         primitive_nobridge 'list',  'list'
         primitive_nobridge 'list=', 'list:'
         def self.s( arg ) # list is [RubyStrNode, RubyNode...RubyNode ]
           unless arg._isArray
             raise_error("arg to RubyDStrNode.s not an Array")
           end
           res = self._new
           res.list=(arg)
           res
         end  
         def size
           @list.size
         end
         def appendToHeadString(aString)
           list[0].appendString(aString)
         end
         def appendToList(node)
           @list << node
         end
         def dstrList
           @list
         end
         def str_dstr_evstr_kind
           1
         end

         def asDSymbolNode
           res = RubyDSymbolNode._new
           lst = @list.dup 
           lst[0] = RubySymbolNode.s( lst[0].strNodeValue._as_symbol ) 
           res.list=( lst )
           res
         end

         def inspect
          "[:dstr, #{self.inspect_list}]"
         end
       end

         class RubyDXStrNode
           def inspect
            "[:dxstr, #{self.inspect_list}]"
           end
         end

       class RubyDSymbolNode
         primitive_nobridge 'list=', 'list:'
         def self.s( arg ) # list is [RubyStrNode, RubyNode...RubyNode ]
           unless arg._isArray
             raise_error("arg to RubyDStrNode.s not an Array")
           end
           res = self._new
           res.list=(arg)
           res
         end  
         def inspect
          "[:dsym, #{self.inspect_list}]"
         end
       end


     class RubyParAsgnRpNode
       # instances converted to RubyParAsgnNode by become during walkWithScope:
       def self.s(first, src_line )
         res = self._new
         res.init(first , src_line)
       end

       # def init ; end #  is in  RubyNode_dynamic.rb

       def masgn_append_arg(val)
         if @thirdNode.equal?( nil)
           f = @firstNode
           if f.equal?(nil)
             # path probably never taken
             raise_error(' masgn_append_arg lhs is nil')
             @thirdNode = RubyArrayNode.s( val )
             @toAry = false
           else
             @thirdNode = val
             @toAry = true
           end
         else
           raise_error(' masgn_append_arg rhs already present')
         end
         self
       end
       def append_mrhs(val)
         if @thirdNode.equal?(nil)
           v_cls = val.class
           if v_cls.equal?( RubyRpCallArgs) || v_cls.equal?( RubySplatNode)
             @thirdNode = val
           else
             raise_error('append_mrhs invalid arg')
           end
         else
           raise_error('append_mrhs rhs already present')
         end
         self
       end
       def inspect
         "\n[:masgnRp bofs #{@position.inspect}, #{@firstNode.inspect}, #{@thirdNode.inspect}]"
       end
     end



     class RubyRescueBodyNode
       def self.s( ex_list, body, next_rescue_body)
         res = self._new
         res.init(ex_list, body, next_rescue_body)
       end
       def self.s( ex_list, body )
         res = self._new
         res.init(ex_list, body, nil)
       end
       def init(ex_list, body, next_rescue_body)
         @exceptionNodes = ex_list
         @bodyNode = body
         @nextRescueBody = next_rescue_body
         self
       end
       def inspect
         "[:resbody, #{@exceptionNodes.inspect} , #{@bodyNode.inspect}, #{@nextRescueBody.inspect}]"
       end
     end

     class RubyReturnNode
       primitive_nobridge 'valueNode=', 'valueNode:'
       def self.s(val)
         res = self._new
         res.valueNode=(val)
         res
       end  
       def inspect
         "[:return, #{@valueNode.inspect} ]"
       end
     end

     class RubyRootNode
       def inspect
         @bodyNode.inspect
       end
       def line_for_offset(byte_ofs)
         if byte_ofs._isFixnum
           byte_ofs = byte_ofs - 1 # to zero based
           ofs = 0
           str = @source
           lnum = 1
           while ofs <= byte_ofs
             if str[ofs].equal?( ?\n )
               lnum += 1
             end
             ofs += 1
           end
           return lnum
         end
         -1
       end
     end

     class RubySplatNode
       primitive_nobridge 'node=', 'node:'
       def self.s(arg)
         res = self._new 
         res.node=(arg)
         res
       end
       def inspect
         "[:splat, #{@node.inspect}]"
       end
     end

     class RubySValueNode
       primitive_nobridge 'node=', 'node:'
       def self.s(arg)
         res = self._new
         res.node=(arg)
         res
       end
       def inspect
         "[:svalue, #{@node.inspect}]"
       end
     end

     class RubyToAryNode
       primitive_nobridge 'node=', 'node:'
       def self.s(arg)
         res = self._new
         res.node=(arg)
         res
       end
       def inspect
         "[:to_ary, #{@node.inspect}]"
       end
     end

     class RubyWhenNode
       def self.s(expr, body, nxt)
         res = self._new
         res.init(expr, body, nxt)
       end
       def init(expr, body, nxt)
         @expressionNodes = expr
         @bodyNode = body
         @nextCase = nxt  # MRI may not be doing this linkage ??
         self
       end
       def inspect
         "[:when , #{@expressionNodes.inspect}, #{@bodyNode.inspect}, <nxt>#{@nextCase.inspect} ]"
       end
     end

  class RpNameToken
    # encapsulate byte offsets in the source
    #  with method name tokens for use in new_call .... AST construction
    def self.new(str, ofs)
      o = self.allocate
      o.initialize(str, ofs)
    end
    def initialize(str, ofs)
      @val = str._as_symbol
      @src_offset = ofs
      self
    end
    def src_offset
      @src_offset
    end
    def symval
      @val
    end
    def inspect
      "(RpNameToken #{@val} @#{@src_offset})"
    end
  end

end

