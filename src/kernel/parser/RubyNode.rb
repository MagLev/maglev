module MagRp

  class RubyNode

    # do not use Ruby new , we want only Smalltalk instance creation
    # semantics, all instance creation in this file should use _new .
    class_primitive_nobridge '_new', '_basicNew'

    # _becomeMinimalChecks:  comes from Object
    primitive_nobridge '_become', '_becomeMinimalChecks:'

    def raise_error(msg = "")
      # not yet implemented:  @parser.print_saved_warnings 
      puts ("InternalParseError: " << msg)
      # nil.pause
      raise InternalParseError, "RubyNode invalid new"
    end
    def self.raise_error(msg = "")
      # not yet implemented:  @parser.print_saved_warnings 
      puts ("InternalParseError: " << msg)
      # nil.pause
      raise InternalParseError, "RubyNode invalid new"
    end

    def self.s
      raise_error("missing constructor")
    end
    def inspect
      raise_error("missing implem of inspect")
    end

    def is_void_result
      false
    end
    def kbegin_value
      self
    end
    # def line ; end # not used
    # def line=(a_line) ; end # not used

    def src_offset=(ofs)
      # returns receiver
      if ofs._isFixnum   # TODO remove check
        @_st_position = ofs + 1 # convert to one-based
      else
        raise_error('invalid source offset')
      end
    end

    def srcOffset
      @_st_position  # result is one based
    end
    def src_offset
      # result is zero based
      p = self.srcOffset
      if p._equal?(nil)
        raise_error('missing source offset')
        return nil
      end
      p - 1
    end

    def srcOffset=(ofs)
      # returns receiver , arg is one based
      if ofs._isFixnum
        @_st_position = ofs  # already one based
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

  class RubyBeginNode 
    def self.s(val)
      res = self._new
      res.init(val)
    end
    def init(val)
      @_st_bodyNode = val
      self
    end
    def kbegin_value
      @_st_bodyNode
    end
    def inspect
      "[:Begin , #{@_st_bodyNode.inspect} ]"
    end
  end


         class RubyBackRefNode
           # newForName: returns either an RubyNthRefNode or a RubyBackRefNode
           class_primitive_nobridge 's', 'newForName:'
           def inspect
             "[:back_ref , #{@_st_selector.inspect} ]"
           end
         end

         class RubyNthRefNode
           primitive_nobridge 'number=', 'matchNumber:'
           class_primitive_nobridge 's', 's_forRp:'
           def inspect
             "[:nth_ref, #{@_st_matchNumNode.inspect}]"
           end
         end


         class RubyMatch2Node
           def self.s(regexNode, rcvrNode)
             res = self._new
             res.init(regexNode, rcvrNode)
           end
           def init(regexNode, rcvrNode)
             @_st_valueNode = regexNode
             @_st_receiverNode = rcvrNode
             self
           end
           def inspect
             "[:match2, #{@_st_receiverNode.inspect} , #{@_st_valueNode.inspect}]"
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
           primitive_nobridge '_base_selector', 'baseSelector'
           # s_forRp:block:bool: inherited from RubyAbstractWhileNode
           def inspect
             "[:until, #{@_st_conditionNode.inspect}, #{@_st_bodyNode.inspect}, #{self._base_selector}]"
           end
         end
         class RubyWhileNode
           primitive_nobridge '_base_selector', 'baseSelector'
           # s_forRp:block:bool: inherited from RubyAbstractWhileNode
           def inspect
             "[:while, #{@_st_conditionNode.inspect}, #{@_st_bodyNode.inspect}, #{self._base_selector}]"
           end
         end

       class RubyAliasNode
         def self.s(newnam, oldnam)
           res = self._new
           res.init(newnam, oldnam)
         end
         def init(newnam, oldnam)
           @_st_newName = newnam # a RubySymbolNode
           @_st_oldName = oldnam # a RubySymbolNode
           self
         end
         def inspect
           "[:alias, @_st_oldName.inspect}, @_st_newNam.inspect} ]"
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
           RubyAndNode.s( aMagRp.cond_or_nil(@_st_firstNode), aMagRp.cond_or_nil(@_st_secondNode))
         end
         def inspect
           "[:and, #{@_st_firstNode.inspect}, #{@_st_secondNode.inspect}]"
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
            sel = sel.__as_symbol
            res.init(rcvr, sel, args)
            res.src_offset=( name_tok.src_offset)
            res
          end
          def init(rcvr, sel, args)
            @_st_receiverNode = rcvr
            @_st_name = sel
            @_st_argsNode = args
            self
          end
          def node_assign_set_rhs(rhs)
            args = @_st_argsNode
            if args._equal?(nil)
              @_st_argsNode = RubyRpCallArgs.s( rhs )
            else
              args.append( rhs )
            end
            self
          end
          def inspect
            "[:attrasgn, #{@_st_receiverNode.inspect}, #{@_st_name}, #{@_st_argsNode.inspect}]"
          end
       end

       class RubyCallNode
         class_primitive_nobridge 's', 's_forRp:sel:args:'  
         primitive_nobridge 'iter=', 'iterNode:'
         def node_assign_set_rhs(rhs)
           # append rhs to args list of this call
           if rhs._equal?(nil)
             raise_error("invalid nil arg")
           end
           @_st_argsNode.append(rhs)
           self
         end
         def rcvr
           @_st_receiverNode
         end
         def inspect
           "\n  [:call, #{@_st_receiverNode.inspect}, :#{@_st_callName}, #{@_st_argsNode.inspect}]"
         end
       end

       class RubyVCallNode
         # a VCall has no args coded in the source code
         #  .mcz code assumes it always has a non-nil receiver 
         class_primitive 's', 's_forRp:selector:'
         def inspect
           "\n  [:vcall, #{@_st_rcvrNode.inspect}, :#{@_st_callName}]"
         end
       end

       class RubyFCallNode
         # a FCall node has no receiver coded in the source code,
          
         class_primitive_nobridge 's', 's_forRp:sel:args:'  
            # caller generates an implicit self for receiver

         primitive_nobridge 'iter=', 'iterNode:'
         def inspect
           "\n  [:fcall, :#{@_st_callName}, #{@_st_argsNode.inspect} ]"
         end
       end

       class RubyDotNode
         def self.s(sym, a, b)
           res = self._new
           res.init(sym, a, b)
         end
         def init(sym, a, b)
           @_st_beginNode = a
           @_st_endNode = b
           @_st_exclusive = sym._equal?(:dot3)
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
     label = 'flip'
     label << self.object_id.to_s 
     env = aMagRp.env
     env[label] = :lvar # label is the only use of anon-Symbol key added to Env
           # both dot3 and dot2 get a FlipNode here
           fn = RubyFlipNode.s( @_st_beginNode , @_st_endNode , @_st_exclusive ) # s(:flip2 )
           fn.src_offset=( self.src_offset )
           fn
         end

         def inspect
           if @_st_exclusive
             sym = :dot3
           else
             sym =  :dot2
           end
           "[:#{sym}, #{@_st_beginNode.inspect}, #{@_st_endNode.inspect}]"
         end
       end

       class RubyEnsureNode
         def self.s(ensure_body)
           res = self._new
           res.init(ensure_body)
         end
         def init(ensure_body)
           @_st_ensureNode = ensure_body
           self
         end
         def set_body(node)
           @_st_bodyNode = node
         end
         def inspect
           "[:ensure, #{@_st_bodyNode.inspect}, #{@_st_ensureNode.inspect}]"
         end
       end

       class RubyHashNode
         primitive_nobridge 'listNode=', 'listNode:'
         def self.s(list)
           lst_cls = list.class
           res = self._new
           if lst_cls._equal?(RubyArrayNode)
             res.listNode=(list)
           elsif lst_cls._equal?(RubyRpCallArgs)
             ary = RubyArrayNode._new
             ary.list=(list.list)
             res.listNode=(ary)
           else
             raise_error("RubyHashNode.s bad arg")
           end
           res
         end
         def inspect
           "[:hash, #{@_st_listNode.inspect} ]"
         end
       end

       class RubyIfNode
         def self.s(cond, tb, eb)
           res = self._new
           res.init(cond, tb, eb)
         end

         def init(cond, tb, eb)
           @_st_condition = cond
           @_st_thenBody =  tb._equal?(nil) ? nil : tb.kbegin_value
           @_st_elseBody =  eb._equal?(nil) ? nil : eb.kbegin_value
           self
         end

         def inspect
           "[:if, #{@_st_condition.inspect}, #{@_st_thenBody.inspect}, #{@_st_elseBody.inspect}]"
         end
       end


       class RubyMethodDefNode
         class_primitive_nobridge 's', 's_forRp:args:body:'
         primitive_nobridge 'start_line=', 'startLine:'
       end
         class RubyDefnNode
           # def self.s # inherited
           def inspect
             "\n[:defn, #{@_st_nameNode.inspect}, #{@_st_argsNode.inspect}, #{@_st_bodyNode.inspect}]"
           end
         end
         class RubyDefsNode
           primitive_nobridge 'receiver=', 'receiverNode:'
           # def self.s # inherited
           def inspect
             "[:defs, #{@_st_receiverNode.inspect}, #{@_st_nameNode.inspect}, #{@_st_argsNode.inspect}, #{@_st_bodyNode.inspect}]"
           end
         end

       class RubyModuleNode
         class_primitive_nobridge 's', 's_forRp:body:source:'
   def inspect
     "[:module , #{@_st_cpath.inspect},  #{@_st_bodyNode.inspect}]"
   end
       end

         class RubyClassNode
           class_primitive_nobridge 's', 's_forRp:superCls:body:source:'
           def inspect
             "[:class , #{@_st_cpath.inspect}, #{@_st_superNode.inspect}, #{@_st_bodyNode.inspect}]"
           end
         end

       class RubyNotNode
         def self.s(arg)
           res = self._new
           res.init(arg)
         end
         def init(arg)
           @_st_conditionNode = arg
           self
         end
         primitive_nobridge 'conditionNode', 'conditionNode'
         def inspect
           "[:not, #{@_st_conditionNode.inspect}]"
         end
       end

       class RubyOpAsgnNode
         primitive_nobridge 'receiverNode=', 'receiverNode:'
         primitive_nobridge 'valueNode=', 'valueNode:'
         primitive_nobridge 'initSelectors', 'initAsgnSel:opSel:'
         def self.s(rcvr, asgnSelTok, opSelTok, val)
           res = self._new
           asgn_sel = (asgnSelTok.symval.to_s << '=' ).__as_symbol
           res.initSelectors(asgn_sel, opSelTok.symval)
           res.receiverNode=(rcvr)
           res.valueNode=(val)
           res.src_offset=( asgnSelTok.src_offset )
           res
         end
         def inspect
           "[:op_asgn2 , #{@_st_receiverNode.inspect}, :#{@_st_variableAsgnCallName}, :#{@_st_operatorCallName}, #{@_st_valueNode.inspect} ]"
         end
       end

       class RubyOpAsgnAndNode
         def self.s(first, second)
           res = self._new
           res.init(first, second)
         end
         def init(first, second)
           @_st_firstNode = first
           @_st_secondNode = second
           self
         end
         def inspect
           "[:op_asgn_and, #{@_st_firstNode.inspect}, #{@_st_secondNode.inspect} ]"
         end
       end

       class RubyOpAsgnOrNode
         def self.s(first, second)
           res = self._new
           res.init(first, second)
         end
         def init(first, second)
           @_st_firstNode = first
           @_st_secondNode = second
           self
         end
         def inspect
           "[:op_asgn_or, #{@_st_firstNode.inspect}, #{@_st_secondNode.inspect} ]"
         end
       end

       class RubyOpElementAsgnNode
         def self.s(rcvr, args, asgn_tok, val)
           res = self._new
           res.init( rcvr, args, asgn_tok, val)
         end
         def init(rcvr, args, asgn_tok, val)
           @_st_receiverNode = rcvr
           @_st_argsNode = args
           @_st_callName = asgn_tok.symval # the methodName
           @_st_valueNode = val
           self.src_offset=( asgn_tok.src_offset )
           self
         end
         def inspect
           "[:op_asgn1, #{@_st_receiverNode.inspect}, #{@_st_argsNode.inspect} :#{@_st_callName}, #{@_st_valueNode.inspect}]"
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
           RubyOrNode.s( aMagRp.cond_or_nil(@_st_firstNode), aMagRp.cond_or_nil(@_st_secondNode))
         end
         def inspect
           "[:or, #{@_st_firstNode.inspect}, #{@_st_secondNode.inspect}]"
         end
       end


       class RubyRescueNode
         def self.s(body, rescuebody, elsebody)
           unless rescuebody.class._equal?(RubyRescueBodyNode)
             raise_error # bad arg
           end
           res = self._new
           res.init(body, rescuebody, elsebody)
         end
   def init(body, rescuebody, elsebody)
     @_st_bodyNode = body
     @_st_rescueBodyNode = rescuebody
     @_st_elseNode = elsebody
           self
         end
         def inspect
          "[:rescue, #{@_st_bodyNode.inspect}, <rescue>#{@_st_rescueBodyNode.inspect}, <else> #{@_st_elseNode.inspect}]"
         end
       end

       class RubySClassNode
         class_primitive_nobridge 's', 's_forRp:body:'
         def inspect
          "[:sclass , #{@_st_receiverNode.inspect} , #{@_st_bodyNode.inspect}]"
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
           "\n  [:super, #{@_st_argsNode.inspect}, #{@_st_iterNode.inspect} ]"
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
           "[:undef , #{@_st_name} ]"
         end
       end


       class RubyYieldNode
         primitive_nobridge 'argsNode=', 'argsNode:'
         def self.s(args)
           res = self._new
           res.argsNode=(args)
           res
         end
         # not sure how we are getting @_st_arrayWrapper bool from RP
         def inspect
           "[:yield , #{@_st_argsNode.inspect}, arrayWrapper:#{@_st_arrayWrapper.inspect}]"
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
         def is_void_result
           true
         end
         def self.s(val)
           res = self._new
           res.valueNode=(val)
           res
         end
       end

         class RubyBreakNode
           # def self.s(val) ; end # inherited from RubyAbstractBreakNode
           def inspect
             "[:break, #{@_st_valueNode.inspect}]"
           end
         end

         class RubyNextNode
           # def self.s(val) ; end # inherited from RubyAbstractBreakNode
           def inspect
             "[:next, #{@_st_valueNode.inspect}]"
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
             @_st_value = num
             self
           end
           def inspect
             "[:lit, #{@_st_value}]"
           end
         end

   class RubyFloatNode
           # instance created by RubyAbstractNumberNode
           def init(num)
             @_st_value = num
             self
           end
           def inspect
             "[:lit, #{@_st_value}]"
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
           "[:regex, #{@_st_regexpLit.inspect}]"
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
           @_st_value
         end
         def appendString(aString)
           unless aString._isString
             raise_error("RubyStrNode appendString arg not a String")
           end
           @_st_value << aString
         end
         def str_dstr_evstr_kind
           0
         end
         def inspect
           "[:str, #{@_st_value.inspect} ]"
         end
       end

         class RubyXStrNode
           # def self.s # inherited from RubyStrNode
           def inspect
             "[:xstr, #{@_st_value.inspect} ]"
           end
         end

       class RubySymbolNode
         def self.s(the_sym )
           unless the_sym._isSymbol ; raise_error; end
           res = self._new
           res.init(the_sym)
         end
         def init(the_sym)
           @_st_name = the_sym
           self
         end
         def strNodeValue
           @_st_name
         end
         def symNodeValue
           @_st_name
         end
         def inspect
           ":#{@_st_name}"
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
           "[:cvar, :#{@_st_name} ]"
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
           "[:colon3, #{@_st_name}]"
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
             "[:colon2, #{@_st_leftNode.inspect}, #{@_st_name}]"
           end
         end

           class RubyClassNameNode
             def inspect
               if @_st_isColon3
                 "[:classname, '::', #{@_st_name}]"
               else
                 "[:classname, #{@_st_leftNode}, #{@_st_name}]"
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
           c2n = RubyColon2Node.simple( @_st_name, ofs )
           res = RubyConstDeclNode.s( c2n, rhs)
           res.src_offset=( ofs )
           res
         end
         def inspect
           "[:const, #{@_st_name.inspect}]"
         end
       end

       class RubyGlobalVarNode
         # s_ForRp:  includes SpecialGlobalNodeClasses logic
         class_primitive_nobridge 's', 's_ForRp:'

         def inspect
           "[:gvar, :#{@_st_name}]"
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
           "[:ivar, :#{@_st_name}]"
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
           "[:lvar, :#{@_st_name}]"
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
         args = @_st_arguments
         if args._equal?(nil)
           args = RubyListNode._new
           @_st_arguments = args
         end
         args.append( RubyArgumentNode.s( sym ) )
         self
       end

       def add_block_arg(arg)
         if arg._equal?(nil)
           # do nothing  ,    opt_f_block_arg term is nil
         else
           if @_st_blockArgNode._equal?(nil)
             if arg.class._equal?( RubyBlockArgNode )
               @_st_blockArgNode = arg
             else
               # expect an RpNameToken
               @_st_blockArgNode = RubyBlockArgNode.s(idTok.symval)
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
         if @_st_restArgNode._equal?(nil)
           @_st_restArgNode = RubyArgumentNode.s(sym)
         else
           raise_error('RubyArgsNode - star arg already installed')
         end
         self
       end

       def add_optional_arg(node)
         # maybe node is an Array of :lasgn ??
         unless node.class._equal?(RubyBlockNode)
           raise_error("add_optional_arg bad arg kind")
         end
         oblk = @_st_optArgs
         if oblk._equal?(nil)
           @_st_optArgs = node
         else
           raise_error('optional_arg already assigned')
         end
         self
       end

       def inspect
         res = "[:args "
         unless @_st_arguments._equal?(nil)
           res << @_st_arguments.inspect_list
         end
         unless @_st_restArgNode._equal?(nil)
           res << ', :"*'
           res << @_st_restArgNode.identifier.to_s
         end
         res << "]"
         unless @_st_blockArgNode._equal?(nil)
           res << ', [:block_arg, '
           res << @_st_blockArgNode.name.to_s
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
         @_st_identifier.inspect
       end
     end

     class RubyAssignableNode
     end

       class RubyClassVarDeclNode
         def self.s(nam, val)
           # used for both :cvasgn and :cvdecl
           res = self._new
           res.init(nam, val)
         end
         def init(nam, val)
           @_st_name = nam
           @_st_valueNode = val
           self
         end
         def as_accessor
           RubyClassVarNode.s(@_st_name)
         end
         def node_assign_set_rhs(rhs)
           if @_st_valueNode._equal?(nil)
             if rhs.is_void_result
               raise SyntaxError, 'void value expression'
             end 
             @_st_valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def inspect
           "[:cvasgn, :#{@_st_name}, #{@_st_valueNode.inspect}]"
         end
       end

       class RubyConstDeclNode
         def self.s(c2node, value)
           res = self._new
           res.init(c2node, value)
         end
         def init(c2node, value)
           @_st_constNode = c2node
           @_st_valueNode = value
           self
         end
         def node_assign_set_rhs(rhs)
           if @_st_valueNode._equal?(nil)
             if rhs.is_void_result
               raise SyntaxError, 'void value expression'
             end 
             @_st_valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def as_accessor
           @_st_constNode.dup   # fix Trac 588 ?
         end
         def inspect
           "[:cdecl, #{@_st_constNode.inspect}, #{@_st_valueNode.inspect}]"
         end
       end

       class RubyDAsgnNode
         def node_assign_set_rhs(rhs)
           if @_st_valueNode._equal?(nil)
             if rhs.is_void_result
               raise SyntaxError, 'void value expression'
             end 
             @_st_valueNode = rhs
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
           "[:dasgn, :#{@_st_name}, #{@_st_valueNode.inspect}]"
         end
       end

       class RubyGlobalAsgnNode
         #  s_ForRp:value:  includes SpecialGlobalNodeClasses logic
         class_primitive_nobridge 's', 's_ForRp:value:'
         def node_assign_set_rhs(rhs)
           if @_st_valueNode._equal?(nil)
             if rhs.is_void_result
               raise SyntaxError, 'void value expression'
             end 
             @_st_valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def as_accessor
           RubyGlobalVarNode.s(@_st_name)
         end
         def inspect
           "[:gasgn, :#{@_st_name}, #{@_st_valueNode.inspect}]"
         end
       end

       class RubyGlobalLastExcBackTrace
         def inspect
           '[:gvar_last_ex_bt, :$@_st_ ]'
         end
       end

       class RubyGlobalNotAssignable
         def node_assign_set_rhs(rhs)
           if @_st_valueNode._equal?(nil)
             if rhs.is_void_result
               raise SyntaxError, 'void value expression'
             end 
             @_st_valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def inspect
           "[:gasgnNotAssignable, :#{@_st_name}, #{@_st_valueNode.inspect}]"
         end
       end

       class RubyGlobalLastExceptionAsgn
         def node_assign_set_rhs(rhs)
           if @_st_valueNode._equal?(nil)
             if rhs.is_void_result
               raise SyntaxError, 'void value expression'
             end 
             @_st_valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def inspect
           "[:gasgnLastExc, #{@_st_valueNode.inspect}]"
         end
       end

       class RubyInstAsgnNode
         def self.s(nam, val)
           res = self._new
           res.init(nam, val)
         end
         def init(nam, val)
           @_st_name = nam
           @_st_valueNode = val
           self
         end
         def node_assign_set_rhs(rhs)
           if @_st_valueNode._equal?(nil)
             if rhs.is_void_result
               raise SyntaxError, 'void value expression'
             end 
             @_st_valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def as_accessor
           RubyInstVarNode.s(@_st_name)
         end
         def inspect
           "[:iasgn, :#{@_st_name}, #{@_st_valueNode.inspect}]"
         end
       end

       class RubyLocalAsgnNode
         def self.s(nam, val)
           res = self._new
           res.init(nam, val)
         end
         def init(nam, val)
           @_st_name = nam
           @_st_valueNode = val
           @_st_isBlockArg = false
           self
         end
         def node_assign_set_rhs(rhs)
           if @_st_valueNode._equal?(nil)
             if rhs.is_void_result
               raise SyntaxError, 'void value expression'
             end 
             @_st_valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def as_accessor
           RubyLocalVarNode.s(@_st_name)
         end
         def inspect
           if @_st_valueNode._not_equal?(nil)
             "[:lasgn, :#{@_st_name}, #{@_st_valueNode.inspect}]"
           else
             "[:lasgn, :#{@_st_name}]"
           end
         end
       end

       class RubyVcGlobalAsgNode
         def node_assign_set_rhs(rhs)
           if @_st_valueNode._equal?(nil)
             if rhs.is_void_result
               raise SyntaxError, 'void value expression'
             end 
             @_st_valueNode = rhs
           else
             raise_error("value already assigned")
           end
           self
         end
         def inspect
           "[:globalAsg, :#{@_st_name}, #{@_st_valueNode.inspect}]"
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
         "[:block_arg, #{@_st_name} ]"
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
         "[:block_pass , #{@_st_bodyNode.inspect}]"
       end
     end

     class RubyCaseNode
       class_primitive_nobridge 's', 's_ForRp:body:' # s_ForRp: exprNode body: caseBody
       def inspect
         "[:case, #{@_st_caseNode.inspect}, #{@_st_caseBody.inspect}]"
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
         "[:defined, #{@_st_expressionNode.inspect} ]"
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
         @_st_body
       end
       def evStrBodyIsStrNode
         @_st_body.class._equal?(RubyStrNode)
       end
       def inspect
        "[:evstr, #{@_st_body.inspect}]"
       end
     end

     class RubyFlipNode
       def self.s(first, second, excl)
         res = self._new
         res.init(first, second, excl)
       end
       def init(first, second, excl)
         @_st_firstNode = first
         @_st_secondNode = second
         @_st_isDot3 = excl
         self
       end
       def inspect
         dots = @_st_isDot3 ? '...' : '..'
         "[:flip, #{@_st_firstNode.inspect}, #{dots} , #{@_st_secondNode.inspect} ]"
       end
     end

     class RubyIterRpNode
       def self.s(args, body)  # call installed later
         res = self._new
         res.init(args, body)
       end
       def init(args, body)
         # @_st_callNode left as nil
         @_st_varNode = args    # further processing  during walk, in astAnalyzeArgs
         @_st_bodyNode = body
         self
       end
       def call=(aCallNode)
         if @_st_callNode._equal?(nil)
           @_st_callNode = aCallNode
         else
           raise_error("call node already assigned")
         end
         @_st_position = aCallNode.srcOffset()  # one-based already
       end
       def inspect
         "[:iterRp, #{@_st_callNode.inspect}, #{@_st_varNode.inspect}, #{@_st_bodyNode.inspect} ]"
       end
     end

       class RubyForNode
         def self.s( iter, var, body )
           res = self._new
           res.init( iter, var, body)
         end
         def init(iter, var, body)
           @_st_iterNode = iter
           @_st_varNode = var
           @_st_bodyNode = body
           self
         end
         def inspect
           "[:for, #{@_st_iterNode.inspect}, #{@_st_varNode.inspect}, #{@_st_bodyNode.inspect} ]"
         end
       end

     class RubyListNode
       # _new inherited , @_st_list left as nil
       primitive_nobridge 'list=', 'list:'
       primitive_nobridge 'append', '_append:'  # returns receiver
       def inspect_list
         res = ""
   if @_st_list
     sep = ""
     @_st_list.each { | ea |
              res << (sep << "#{ea.inspect}")
              sep = ", "
           }
   end
   res
       end
     end


       class RubyArrayNode
         class_primitive_nobridge '_new', '_new' # inits @_st_list to Smalltalk #()
         primitive_nobridge 'list', 'list'
         primitive_nobridge 'list=', 'list:'
         class_primitive_nobridge 's', '_new:'  # one arg
         class_primitive_nobridge 's', '_new:with:'  # two arg
         primitive_nobridge '_append', '_append:'  # returns receiver
         primitive_nobridge 'appendAll', '_appendAll:'  # returns receiver
         def at(idx)
           @_st_list[idx]  # returns nil if list is empty
         end
         def append(v)
           if v._equal?(nil)
             raise_error('invalid nil arg')
           end
           self._append(v)
           self
         end

         def append_for_mlhs(v)
           prev = @_st_list[-1]
           if prev._not_equal?(nil) && prev.isAmpersandBlockParam
             return false
           end
           if v._equal?(nil)
             raise_error('invalid nil arg')
           end
           self._append(v)
           true
         end           
 
         def arrayDup
           res = RubyArrayNode._new
           res.list=( @_st_list.dup )
           res
         end
         def arrayLength
           @_st_list.length
         end
 
         primitive_nobridge 'prepend' , '_prepend:'  # returns receiver

         def srcOffset
           p = @_st_position
           if p._isFixnum
             return p
           end
           lst = @_st_list
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
           if @_st_list
             @_st_list.each { | ea |  res << ", #{ea.inspect}" }
           end
           res << "]"
           res
         end
       end

         class RubyRpCallArgs
           # most meths  inherited from RubyArrayNode
           #  .mcz contains reimplementation of hasRestArg

           def is_empty
             @_st_list.size._equal?(0) && @_st_iterNode._equal?(nil)
           end

           def append_arg(v)
             if v._equal?(nil)
               raise_error('invalid nil arg')
             end
             vcls = v.class
             if vcls._equal?(RubyBlockPassNode)
               raise_error('must use append_blk_arg ')
             end
             if v.is_void_result
               raise SyntaxError, 'void value expression'
             end 
             self._append(v)
             self
           end

           def iter
             @_st_iterNode
           end

           def list
             @_st_list
           end

           def append_blk_arg(node)
             if @_st_iterNode._equal?(nil)
               if node._equal?(nil) || node.class._equal?(RubyBlockPassNode)
                 @_st_iterNode = node
               else
                 raise_error('invalid block argument')
               end
             else
               raise_error("block argument already present")
             end
             self
           end
           def as_yield_args
             lst = @_st_list
             if lst.size._equal?( 1) && @_st_iterNode._equal?(nil)
               nod = lst[0]
               if nod.class._equal?( RubySplatNode)
                 return nod  # for   yield *one_arg
               end
             end
             self
           end
           def inspect
             res = "[:callargs "
             if @_st_list
               @_st_list.each { | ea |  res << ", #{ea.inspect}" }
             end
             if @_st_iterNode 
               res << ", #{@_st_iterNode.inspect}"
             end
             res << "]"
             res
           end
         end

       class RubyBlockNode
         def self.s(a_list)
           res = self._new
           res.list=(a_list)  # a_list should be an Array
           res
         end
         def append_to_block(val)
           @_st_list << val
         end
         def prepend_to_block(val)
           @_st_list.insert(0, val)
           self
         end
         def isEmptyBlockNode
           lst = @_st_list
           if lst
             lst.size._equal?(0)
           else
             true
           end
         end

         def blockNode_listdo_kbegin_value
           lst = @_st_list
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
           if @_st_list
             sep = ""
             @_st_list.each { | ea |
                res << sep
                res << "#{ea.inspect}"
                sep = ", "
             }
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
           @_st_list.size
         end
         def appendToHeadString(aString)
           list[0].appendString(aString)
         end
         def appendToList(node)
           @_st_list << node
         end
         def dstrList
           @_st_list
         end
         def str_dstr_evstr_kind
           1
         end

         def asDSymbolNode
           res = RubyDSymbolNode._new
           lst = @_st_list.dup
           lst[0] = RubySymbolNode.s( lst[0].strNodeValue.__as_symbol )
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

       def append_mrhs(val)
         if @_st_thirdNode._equal?(nil)
           v_cls = val.class
           if v_cls._equal?( RubyRpCallArgs) || v_cls._equal?( RubySplatNode)
             @_st_thirdNode = val
           else
             raise_error('append_mrhs invalid arg')
           end
         else
           raise_error('append_mrhs rhs already present')
         end
         self
       end

       def isAmpersandBlockParam
         lv = @_st_firstNode.list.last
         lv._not_equal?(nil) && lv.isAmpersandBlockParam
       end

       def masgn_append_arg(val)
         if @_st_thirdNode._equal?( nil)
           f = @_st_firstNode
           if f._equal?(nil)
             # path probably never taken
             raise_error(' masgn_append_arg lhs is nil')
             @_st_thirdNode = RubyArrayNode.s( val )
             @_st_toAry = false
           else
             @_st_thirdNode = val
             @_st_toAry = true
           end
         else
           raise_error(' masgn_append_arg rhs already present')
         end
         self
       end
     
       def inspect
         "\n[:masgnRp bofs #{@_st_position.inspect}, #{@_st_firstNode.inspect}, #{@_st_thirdNode.inspect}]"
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
         @_st_exceptionNodes = ex_list
         @_st_bodyNode = body
         @_st_nextRescueBody = next_rescue_body
         self
       end
       def inspect
         "[:resbody, #{@_st_exceptionNodes.inspect} , #{@_st_bodyNode.inspect}, #{@_st_nextRescueBody.inspect}]"
       end
     end

     class RubyReturnNode
       primitive_nobridge 'valueNode=', 'valueNode:'
       def is_void_result
	 true
       end
       def self.s(val)
         res = self._new
         res.valueNode=(val)
         res
       end
       def inspect
         "[:return, #{@_st_valueNode.inspect} ]"
       end
     end

     class RubyRootNode
       def inspect
         @_st_bodyNode.inspect
       end
       def line_for_offset(byte_ofs)
         if byte_ofs._isFixnum
           byte_ofs = byte_ofs - 1 # to zero based
           ofs = 0
           str = @_st_source
           lnum = 1
           while ofs <= byte_ofs
             if str[ofs]._equal?( ?\n )
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
         "[:splat, #{@_st_node.inspect}]"
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
         "[:svalue, #{@_st_node.inspect}]"
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
         "[:to_ary, #{@_st_node.inspect}]"
       end
     end

     class RubyWhenNode
       def self.s(expr, body, nxt)
         res = self._new
         res.init(expr, body, nxt)
       end
       def init(expr, body, nxt)
         @_st_expressionNodes = expr
         @_st_bodyNode = body
         @_st_nextCase = nxt  # MRI may not be doing this linkage ??
         self
       end
       def inspect
         "[:when , #{@_st_expressionNodes.inspect}, #{@_st_bodyNode.inspect},\n <nxtWhen> #{@_st_nextCase.inspect} ]"
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
      @_st_val = str.__as_symbol
      @_st_src_offset = ofs
      self
    end
    def src_offset
      @_st_src_offset
    end
    def symval
      @_st_val
    end
    def inspect
      "(token \"#{@_st_val}\")"
    end
  end

  class GsMethodDictionary  # used by racc state machine token_table
    class_primitive '__new', 'new:'

    def initialize(*args)
      raise 'normal instance creation disallowed, must use __new'
    end

    def self.from_hash(a_hash)
      dict = self.__new(a_hash.size) 
      a_hash.each { | k,v |
        dict.at_put(k, v)
      }
      dict 
    end
    primitive 'at_put', 'at:put:' 
    primitive 'at_otherwise', 'at:otherwise:'
  end

  class StringKeyValueDictionary  # used in the lexer
    class_primitive '__new', 'new:'

    def initialize(*args)
      raise 'normal instance creation disallowed, must use __new'
    end

    primitive 'at_put', 'at:put:'
    primitive 'at_casesens_otherwise', '_stringAt:caseSensitive:otherwise:'
  end
end

