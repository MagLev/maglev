# -*- racc -*-

class MagRp::RubyParser

token kCLASS kMODULE kDEF kUNDEF kBEGIN kRESCUE kENSURE kEND kIF kUNLESS
      kTHEN kELSIF kELSE kCASE kWHEN kWHILE kUNTIL kFOR kBREAK kNEXT
      kREDO kRETRY kIN kDO kDO_COND kDO_BLOCK kRETURN kYIELD kSUPER
      kSELF kNIL kTRUE kFALSE kAND kOR kNOT kIF_MOD kUNLESS_MOD kWHILE_MOD
      kUNTIL_MOD kRESCUE_MOD kALIAS kDEFINED klBEGIN klEND k__LINE__
      k__FILE__ tIDENTIFIER tFID tGVAR tIVAR tCONSTANT tCVAR tNTH_REF
      tBACK_REF tSTRING_CONTENT tINTEGER tFLOAT tREGEXP_END tUPLUS
      tUMINUS tUMINUS_NUM tPOW tCMP tEQ tEQQ tNEQ tGEQ tLEQ tANDOP
      tOROP tMATCH tNMATCH tDOT tDOT2 tDOT3 tAREF tASET tLSHFT tRSHFT
      tCOLON2 tCOLON3 tOP_ASGN tASSOC tLPAREN tLPAREN2 tRPAREN tLPAREN_ARG
      tLBRACK tRBRACK tLBRACE tLBRACE_ARG tSTAR tSTAR2 tAMPER tAMPER2
      tTILDE tPERCENT tDIVIDE tPLUS tMINUS tLT tGT tPIPE tBANG tCARET
      tLCURLY tRCURLY tBACK_REF2 tSYMBEG tSTRING_BEG tXSTRING_BEG tREGEXP_BEG
      tWORDS_BEG tAWORDS_BEG tSTRING_DBEG tSTRING_DVAR tSTRING_END t_STRING
      tSYMBOL tNL tEH tCOLON tCOMMA tSPACE tSEMI tLBRACK_STR tLAST_TOKEN

prechigh
  right    tBANG tTILDE tUPLUS
  right    tPOW
  right    tUMINUS_NUM tUMINUS
  left     tSTAR2 tDIVIDE tPERCENT
  left     tPLUS tMINUS
  left     tLSHFT tRSHFT
  left     tAMPER2
  left     tPIPE tCARET
  left     tGT tGEQ tLT tLEQ
  nonassoc tCMP tEQ tEQQ tNEQ tMATCH tNMATCH
  left     tANDOP
  left     tOROP
  nonassoc tDOT2 tDOT3
  right    tEH tCOLON
  left     kRESCUE_MOD
  right    tEQL tOP_ASGN
  nonassoc kDEFINED
  right    kNOT
  left     kOR kAND
  nonassoc kIF_MOD kUNLESS_MOD kWHILE_MOD kUNTIL_MOD
  nonassoc tLBRACE_ARG
  nonassoc tLOWEST
preclow

rule

         program:   {
		      # program:
                      @lexer.lex_state=( RubyLexer::Expr_beg )
                      result = val[vofs]
                    }
                    compstmt
                    {
		      # compstmt
                      result = val[vofs+ 1]
                    }

        bodystmt: compstmt opt_rescue opt_else opt_ensure
                    {
		      # bodystmt: compstmt opt_rescue opt_else opt_ensure
                      result = new_body( val, vofs)
                    }

        compstmt: stmts opt_terms
                    {
		      # compstmt: stmts opt_terms
                      result = new_compstmt(val[vofs])
                    }

           stmts: none
                | stmt
                | stmts terms stmt
                    {
		      # | stmts terms stmt
                      result = self.block_append( val[vofs], val[vofs + 2])
                    }
                | error stmt
                    {
		      # | error stmt
                      result = val[vofs + 1]
                    }

            stmt: kALIAS fitem
                    {
		      # stmt: kALIAS fitem
                      lx = @lexer
                      lx.lex_state=( RubyLexer::Expr_fname )
                      result = -901 # lx.lineno_
                    }
                    fitem
                    {
		      # kALIAS fitem   fitem
                      result = RubyAliasNode.s(val[vofs + 1], val[vofs + 3])
                      result.src_offset=( val[vofs].src_offset ) # of kALIAS RpNameToken
                    }
                | kALIAS tGVAR tGVAR
                    {
		      # | kALIAS tGVAR tGVA
                      result = RubyGlobalVarAliasNode.s( val[vofs + 1].symval, val[vofs + 2].symval) # s(:valias)
                    }
                | kALIAS tGVAR tBACK_REF
                    {
		      # | kALIAS tGVAR tBACK_REF
                      result = RubyGlobalVarAliasNode.s( val[vofs + 1].symval, :"$#{val[vofs + 2]}" )  # s(:valias)
                    }
                | kALIAS tGVAR tNTH_REF
                    {
		      # | kALIAS tGVAR tNTH_REF
                      yyerror "can't make alias for the number variables"
                    }
                | kUNDEF undef_list
                    {
		      # | kUNDEF undef_list
                      result = val[vofs + 1]
                    }
                | stmt kIF_MOD expr_value
                    {
		      # | stmt kIF_MOD expr_value
                      result = new_if(val[vofs + 2], val[vofs ], nil ) 
                      result.src_offset=( val[vofs + 1].src_offset )
                    }
                | stmt kUNLESS_MOD expr_value
                    {
		      # | stmt kUNLESS_MOD expr_value
                      result = new_if( val[vofs + 2], nil, val[vofs])
                      result.src_offset=( val[vofs + 1].src_offset )
                    }
                | stmt kWHILE_MOD expr_value
                    {
		      # | stmt kWHILE_MOD expr_value  
                      # val_[1] is kWHILE_MOD RpNameToken
                      result = new_while( val[vofs ], val[vofs + 2] )
                      result.src_offset=( val[vofs + 1].src_offset) # kWHILE_MOD  RpNameToken
                    }
                | stmt kUNTIL_MOD expr_value
                    {
		      # | stmt kUNTIL_MOD expr_value   
                      # val_[1] is kUNTIL_MOD RpNameToken
                      result = new_until( val[vofs ], val[vofs + 2] )
                      result.src_offset=( val[vofs + 1].src_offset )
                    }
                | stmt kRESCUE_MOD stmt
                    {
		      # | stmt kRESCUE_MOD stmt
                      # result = s(:rescue, val_[0], s(:resbody, s(:array), val_[2]))
                      resbody = RubyRescueBodyNode.s(nil, val[vofs + 2])
                      result = RubyRescueNode.s( val[vofs ], resbody, nil)
                      ofs = val[vofs + 1].src_offset  # # kRESCUE position
                      result.src_offset=( ofs )
                      resbody.src_offset=( ofs )
                    }
                | klBEGIN
                    {
		      # | klBEGIN
                      if (@in_def || @in_single > 0) then
                        yyerror "BEGIN in method"
                      end
                      @env.extend( false)
                      result = val[vofs]
                    }
                    tLCURLY compstmt tRCURLY
                    {
		      # tLCURLY compstmt tRCURLY
                      # result = new_iter s(:preexe), nil, val[vofs + 3] # TODO: add test?
                      result = nil # TODO: since it isn't supposed to go in the AST
                    }
                | klEND tLCURLY compstmt tRCURLY
                    {
		      # | klEND tLCURLY compstmt tRCURLY
                      if (@in_def || @in_single > 0) then
                        yyerror "END in method; use at_exit"
                      end
                      # result = new_iter s(:postexe), nil, val_[2]
                      result = new_iter(nil, val[vofs + 2])
                    }
                | lhs tEQL command_call
                    {
		      # | lhs tEQL command_call
                      result = self.node_assign(val[vofs ], val[vofs + 2])
                    }
                | mlhs tEQL command_call
                    {
		      # | mlhs tEQL command_call
                      result = masgn_append_arg( val[vofs ], val[vofs + 2] ) 
                    }
                | var_lhs tOP_ASGN command_call
                    {
		      # | var_lhs tOP_ASGN command_call
                      result = new_op_asgn(val, vofs)
                    }
                | primary_value tLBRACK_STR aref_args tRBRACK tOP_ASGN command_call
                    {
		      # | primary_value tLBRACK_STR aref_args tRBRACK tOP_ASGN command_call
                      result = RubyOpElementAsgnNode.s(val[vofs ], val[vofs + 2], val[vofs + 4], val[vofs + 5]) # s(:op_asgn1 )
                    }
                | primary_value tDOT tIDENTIFIER tOP_ASGN command_call
                    {
		      # | primary_value tDOT tIDENTIFIER tOP_ASGN command_call
                      raise_error(":op_asgn never seen from MRI parser ")
                      # result = s(:op_asgn, val_[0], val_[4], val_[2], val_[3])
                      result = nil
                    }
                | primary_value tDOT tCONSTANT tOP_ASGN command_call
                    {
		      # | primary_value tDOT tCONSTANT tOP_ASGN command_call
                      raise_error(":op_asgn never seen from MRI parser ")
		      # result = s(:op_asgn, val_[0], val_[4], val_[2], val_[3])
		      result = nil
		    } 
		  | primary_value tCOLON2 tIDENTIFIER tOP_ASGN command_call
		      {
		        # | primary_value tCOLON2 tIDENTIFIER tOP_ASGN command_call
                        raise_error(":op_asgn never seen from MRI parser ")
			# result = s(:op_asgn, val_[0], val_[4], val_[2], val_[3])
                        result = nil
		      }
		  | backref tOP_ASGN command_call
		      {
			# | backref tOP_ASGN command_call
                        v_zero = val[vofs ] 
			self.backref_assign_error( v_zero )
                        result = v_zero 
		      }
		  | lhs tEQL mrhs
		      {
			# | lhs tEQL mrhs
			result = self.node_assign(val[vofs ], RubySValueNode.s( val[vofs + 2]))
		      }
		  | mlhs tEQL arg_value
		      {
			# | mlhs tEQL arg_value
			result =  masgn_append_arg(val[vofs ], val[vofs + 2] )
		      }
		  | mlhs tEQL mrhs
		      {
			# | mlhs tEQL mrhs
			result = masgn_append_mrhs( val[vofs ], val[vofs + 2] )
		      }
		  | expr

	      expr: command_call
		  | expr kAND expr
		      {
			# | expr kAND expr
			result = logop( RubyAndNode,  val[vofs ], val[vofs + 2])  # s(:and )
		      }
		  | expr kOR expr
		      {
			# | expr kOR expr
			result = logop( RubyOrNode, val[vofs ], val[vofs + 2]) # s(:or )
		      }
		  | kNOT expr
		      {
			# | kNOT expr
			result = RubyNotNode.s( val[vofs + 1]) # s(:not )
		      }
		  | tBANG command_call
		      {
			# | tBANG command_call
			result = RubyNotNode.s( val[vofs + 1]) # s(:not )
		      }
		  | arg

	expr_value: expr
		      {
			# expr_value: expr
			result = value_expr(val[vofs ])
		      }

      command_call: command
		  | block_command
		  | kRETURN call_args
		      {
			# | kRETURN call_args
			# result = s(:return, ret_args(val_[1]))
                        result = RubyReturnNode.s( ret_args(val[vofs + 1]))
			result.src_offset=( val[vofs ].src_offset ) # of the kRETURN
		      }
		  | kBREAK call_args
		      {
			# | kBREAK call_args
			# result = s(:break, ret_args(val_[1]))
                        result = RubyBreakNode.s( ret_args(val[vofs + 1]))
                        result.src_offset=( val[vofs ].src_offset ) #  of the kBREAK
		      }
		  | kNEXT call_args
		      {
			# | kNEXT call_args
			# result = s(:next, ret_args(val_[1]))
                        result = RubyNextNode.s( ret_args(val[vofs + 1]))
                        result.src_offset=( val[vofs ].src_offset ) #  of the kNEXT
		      }

     block_command: block_call
		  | block_call tDOT operation2 command_args
		      {
			# | block_call tDOT operation2 command_args
			result = new_call(val[vofs ], val[vofs + 2], val[vofs + 3])
		      }
		  | block_call tCOLON2 operation2 command_args
		      {
			# | block_call tCOLON2 operation2 command_args
			result = new_call(val[vofs ], val[vofs + 2], val[vofs + 3])
		      }

   cmd_brace_block: tLBRACE_ARG
		      {
			# cmd_brace_block: tLBRACE_ARG
			@env.extend( true ) # (:dynamic)
			result = -902 #  @lexer.lineno_
		      }
		      opt_block_var
		      {
			# opt_block_var
			result = @env.dynamic_keys
		      }
		      compstmt tRCURLY
		      {
			#  compstmt tRCURLY
			result = new_iter(val[vofs + 2], val[vofs + 4])
                      @env.unextend
                    }

         command: operation command_args =tLOWEST
                    {
		      # command: operation command_args =tLOWEST
                      result = new_fcall( val[vofs ], val[vofs + 1])
                    }
                | operation command_args cmd_brace_block
                    {
		      # | operation command_args cmd_brace_block
                      result = new_fcall( val[vofs ], val[vofs + 1])
                      v_two = val[vofs + 2]
                      if v_two then
                        if v_two.class.equal?(RubyBlockPassNode) # v_two[0] == :block_pass 
                          raise "both block arg and actual block given"
                        end
                        # result, operation = val_[2], result
                        # result.insert 1, operation
                        iter = v_two
                        call = result
                        iter.call=(call)
                        result = iter
                      end
                    }
                | primary_value tDOT operation2 command_args =tLOWEST
                    {
		      # | primary_value tDOT operation2 command_args =tLOWEST
                      result = new_call(val[vofs ], val[vofs + 2], val[vofs + 3])
                    }
                | primary_value tDOT operation2 command_args cmd_brace_block
                    {
		      # | primary_value tDOT operation2 command_args cmd_brace_block
                      result = new_call(val[vofs ], val[vofs + 2], val[vofs + 3])
                    }
                | primary_value tCOLON2 operation2 command_args =tLOWEST
                    {
		      # | primary_value tCOLON2 operation2 command_args =tLOWEST
                      result = new_call(val[vofs], val[vofs + 2], val[vofs + 3])
                    }
                | primary_value tCOLON2 operation2 command_args cmd_brace_block
                    {
		      # | primary_value tCOLON2 operation2 command_args cmd_brace_block
                      result = new_call(val[vofs ], val[vofs + 2], val[vofs + 3])
                      if val[vofs + 4] then
                        #if result[0] == :block_pass then # REFACTOR
                        if result.rcvr.class.equal?(RubyBlockPassNode) 
                          raise "both block arg and actual block given"
                        end
                        raise_error("dont know how to append to selector") 
                        val[vofs + 2] << result
                        result = val[vofs + 2]
                      end
                    }
                | kSUPER command_args
                    {
		      # | kSUPER command_args
                      result = new_super( val, vofs )
                    }
                | kYIELD command_args
                    {
		      # | kYIELD command_args
                      result = new_yield( val[vofs + 1] )
                      result.src_offset=( val[vofs ].src_offset ) # of the kYIELD
                    }

            mlhs: mlhs_basic
                | tLPAREN mlhs_entry tRPAREN
                    {
		      # | tLPAREN mlhs_entry tRPAREN
                      result = val[vofs + 1]
                    }

      mlhs_entry: mlhs_basic
                | tLPAREN mlhs_entry tRPAREN
                    {
		      # mlhs_entry: mlhs_basic #  | tLPAREN mlhs_entry tRPAREN
                      # result = s(:masgn, s(:array, val_[1]))
                      ofs = val[vofs ].src_offset  #  of the tLPAREN
                      result = new_parasgn( RubyArrayNode.s(val[vofs + 1]) , ofs )
                    }

      mlhs_basic: mlhs_head
                    {
		      # mlhs_basic: mlhs_head # result = s(:masgn, val_[0])
                      v_zero = val[vofs ]
                      result = new_parasgn( v_zero , v_zero.src_offset )
                    }
                | mlhs_head mlhs_item
                    {
		      # mlhs_basic: mlhs_head #  | mlhs_head mlhs_item
                      # result = s(:masgn, val_[0] << val_[1].compact)
                      v_zero = val[vofs ]
                      v_one = val[vofs + 1]
                      v_zero.append( v_one )
                      ofs = v_one.src_offset 
                      result = new_parasgn( v_zero, ofs )
                    }
                | mlhs_head tSTAR mlhs_node
                    { 
		      # mlhs_basic: mlhs_head #   mlhs_head tSTAR mlhs_node
                      # result = s(:masgn, val_[0] << s(:splat, val_[2])) 
                      v_zero = val[vofs ]
                      ofs = val[vofs + 1].src_offset # of the tSTAR
                      v_zero.append(  RubySplatNode.s(val[vofs + 2] ))
                      result = new_parasgn( v_zero , ofs )
                    }
                | mlhs_head tSTAR
                    {
		      # mlhs_basic: mlhs_head #   mlhs_head tSTAR
                      # result = s(:masgn, val_[0] << s(:splat))
                      v_zero = val[vofs ]
                      v_zero.append(  RubySplatNode.s( nil ))
                      ofs = val[vofs + 1].src_offset  #  of the tSTAR
                      result = new_parasgn( v_zero, ofs )
                    }
                | tSTAR mlhs_node
                    {  
		      # mlhs_basic: mlhs_head #   tSTAR mlhs_node
                      # result = s(:masgn, s(:array, s(:splat, val_[1])))
                      ofs = val[vofs ].src_offset  #  of the tSTAR
             result = new_parasgn( RubyArrayNode.s( RubySplatNode.s( val[vofs + 1])), ofs )
                    }
                | tSTAR
                    {
		      # mlhs_basic: mlhs_head #  | tSTAR
                      # result = s(:masgn, s(:array, s(:splat)))
                      ofs = val[vofs ].src_offset  #  of the tSTAR
                result = new_parasgn( RubyArrayNode.s( RubySplatNode.s(nil)), ofs )
                    }

       mlhs_item: mlhs_node
                | tLPAREN mlhs_entry tRPAREN
                    {
		      # mlhs_item: mlhs_node #  | tLPAREN mlhs_entry tRPAREN
                      result = val[vofs + 1]  # mlhs_item: mlhs_node ; tLPAREN mlhs_entry tRPAREN
                    }

       mlhs_head: mlhs_item tCOMMA
                    {
		      # mlhs_head: mlhs_item tCOMMA
                      # result = s(:array, val_[0]) # mlhs_head: mlhs_item tCOMMA
                      result = RubyArrayNode.s( val[vofs ])
                    }
                | mlhs_head mlhs_item tCOMMA
                    {
		      # mlhs_head: mlhs_item tCOMMA #  | mlhs_head mlhs_item tCOMMA
                      # result = val_[0] << val_[1].compact
                      v_zero = val[vofs ]
                      v_zero.append( val[vofs + 1] )
                      result = v_zero
                    }

       mlhs_node: variable
                    {
		      # mlhs_node: variable
                      result = self.assignable(val[vofs ], nil)
                    }
                | primary_value tLBRACK_STR aref_args tRBRACK
                    {
		      # mlhs_node: variable #  | primary_value tLBRACK_STR aref_args tRBRACK
                      result = RubyAttrAssignNode.s(val[vofs ], :"[]=", val[vofs + 2] )
                      result.src_offset=( val[vofs + 3].src_offset )  # position of tRBRACK
                    }
                | primary_value tDOT tIDENTIFIER
                    {
		      # mlhs_node: variable #  | primary_value tDOT tIDENTIFIER
                      # result = s(:attrasgn, val_[0], :"#{val_[2]}=", s(:arglist))
                      # the  tIDENTIFIER value will be a RpNameToken
                      #  all places where we send   symval   we expect a  RpNameToken
            result = RubyAttrAssignNode.s_tk(val[vofs ], val[vofs + 2], nil )
                    }
                | primary_value tCOLON2 tIDENTIFIER
                    {
		      # mlhs_node: variable #  | primary_value tCOLON2 tIDENTIFIER
                      # result = s(:attrasgn, val_[0], :"#{val_[2]}=", s(:arglist))
                      # the  tIDENTIFIER value will be a RpNameToken
           result = RubyAttrAssignNode.s_tk( val[vofs ], val[vofs + 2], nil )
                    }
                | primary_value tDOT tCONSTANT
                    {
		      # mlhs_node: variable #  | primary_value tDOT tCONSTANT
                      # result = s(:attrasgn, val_[0], :"#{val_[2]}=", s(:arglist))
           result = RubyAttrAssignNode.s_tk( val[vofs ], val[vofs + 2], nil)
                    }
                | primary_value tCOLON2 tCONSTANT
                    {
		      # mlhs_node: variable #  | primary_value tCOLON2 tCONSTANT
                      if (@in_def || @in_single > 0) then
                        yyerror "dynamic constant assignment"
                      end
                    # all RubyColon2Node.s  expect second arg to be a RpNameToken
                    #  result = s(:const, s(:colon2, val_[0], val_[2].to_sym), nil)
     result = RubyConstDeclNode.s( RubyColon2Node.s( val[vofs ], val[vofs + 2]), nil)
                    }
                | tCOLON3 tCONSTANT
                    {
		      # mlhs_node: variable #  | tCOLON3 tCONSTANT
                      if (@in_def || @in_single > 0) then
                        yyerror "dynamic constant assignment"
                      end
                      # all RubyColon3Node.s  expect second arg to be a RpNameToken
                      # result = s(:const, nil, s(:colon3, val_[1].to_sym))
                   result = RubyConstDeclNode.s( RubyColon3Node.s( val[vofs + 1] ), nil )
                    }
                | backref
                    {
		      # mlhs_node: variable #  | backref
                      self.backref_assign_error( val[vofs ] ) 
                      result = val[vofs]
                    }

             lhs: variable
                    {
		      # lhs: variable
                      result = self.assignable(val[vofs ], nil)
                    }
                | primary_value tLBRACK_STR aref_args tRBRACK
                    {
		      # lhs: variable #   | primary_value tLBRACK_STR aref_args tRBRACK
                      result = RubyAttrAssignNode.s(val[vofs ], :"[]=", val[vofs + 2] )
                      result.src_offset=( val[vofs + 3].src_offset )  # position of tRBRACK
                    }
                | primary_value tDOT tIDENTIFIER
                    {
		      # lhs: variable #  | primary_value tDOT tIDENTIFIER
                      # result = s(:attrasgn, val_[0], :"#{val_[2]}=")
                      # the  tIDENTIFIER value will be a RpNameToken
                      result = RubyAttrAssignNode.s_tk(val[vofs ], val[vofs + 2], nil)
                    }
                | primary_value tCOLON2 tIDENTIFIER
                    {
		      # lhs: variable #  | primary_value tCOLON2 tIDENTIFIER
                      # result = s(:attrasgn, val_[0], :"#{val_[2]}=")
                      # the  tIDENTIFIER value will be a RpNameToken
		      result = RubyAttrAssignNode.s_tk(val[vofs ], val[vofs + 2], nil)
                    }
                | primary_value tDOT tCONSTANT
                    {
		      # lhs: variable #  | primary_value tDOT tCONSTANT
                      # result = s(:attrasgn, val_[0], :"#{val_[2]}=")
                      result = RubyAttrAssignNode.s_tk( val[vofs ], val[vofs + 2], nil)
                    }
                | primary_value tCOLON2 tCONSTANT
                    {
		      # lhs: variable #  | primary_value tCOLON2 tCONSTANT
                      if (@in_def || @in_single > 0) then
                        yyerror "dynamic constant assignment"
                      end
                      # result = s(:const, s(:colon2, val_[0], val_[2].to_sym))
       result = RubyConstDeclNode.s( RubyColon2Node.s( val[vofs ], val[vofs + 2]), nil)
                    }
                | tCOLON3 tCONSTANT
                    {
		      # lhs: variable #  | tCOLON3 tCONSTANT
                      if (@in_def || @in_single > 0) then
                        yyerror "dynamic constant assignment"
                      end

                      # result = s(:const, s(:colon3, val_[1].to_sym))
        result = RubyConstDeclNode.s( RubyColon3Node.s( val[vofs + 1]), nil )
                    }
                | backref
                    {
		      # lhs: variable #  | backref
                      self.backref_assign_error( val[vofs ] ) 
                      result = val[vofs]
                    }

           cname: tIDENTIFIER
                    {
		      # cname: tIDENTIFIER
                      yyerror "class/module name must be CONSTANT"
                      result = val[vofs]
                    }
                | tCONSTANT

           cpath: tCOLON3 cname
                    {
		      # cpath: tCOLON3 cname
                      # result = s(:colon3, val_[1].to_sym)
                      result = RubyColon3Node.s( val[vofs + 1] )
                    }
                | cname
                    {
		      # cpath: tCOLON3 cname #   | cname  
# TODO, fix trac_1001.rb 
                      result = val[vofs ]  # a RpNameToken
                    }
                | primary_value tCOLON2 cname
                    {
		      #  cpath: tCOLON3 cname #   | primary_value tCOLON2 cname
                      # result = s(:colon2, val_[0], val_[2].to_sym)
                      result =  RubyColon2Node.s( val[vofs ], val[vofs + 2] )
                    }

           fname: tIDENTIFIER | tCONSTANT | tFID
                | op
                    {
		      # fname: tIDENTIFIER | tCONSTANT | tFID #  | op
                      @lexer.lex_state=( RubyLexer::Expr_end )
                      result = val[vofs ]   # val_[0] is a RpNameToken
                    }

                | reswords
                    {
		      # fname: tIDENTIFIER | tCONSTANT | tFID #  | reswords
                      @lexer.lex_state=( RubyLexer::Expr_end )
                      result = val[vofs ]  # val_[0] is a RpNameToken or a String
                    }

                    # TODO: cruby has fsym and dsym
           fitem: fname  
	            { 
                           # TODO: cruby has fsym and dsym
		      # fitem: fname
                      v_zero = val[vofs ]  # a RpNameToken
		      result = RubySymbolNode.s( v_zero.symval)  # s(:lit)
                      result.src_offset=( v_zero.src_offset )
                      # fitem - fname path
                    }
                | symbol 
		    { 
		       #  fitem: fname #    | symbol 
		       result = RubySymbolNode.s( val[vofs ])  # s(:lit)
			# fitem - symbol path 
                    }

      undef_list: fitem
                    {
		      # undef_list: fitem
                      vsym = val[vofs ]   # a RubySymbolNode
                      result = new_undef( vsym )
                      result.src_offset=( val[ vofs - 1 ].src_offset ) # of the kUNDEF
                    }
                | undef_list tCOMMA
                    {
		      # undef_list: fitem #  | undef_list tCOMMA
                      @lexer.lex_state=( RubyLexer::Expr_fname )
                    }
                    fitem
                    {
		      # undef_list: fitem #   fitem
                      result = append_undef( val[vofs ], val[vofs + 3] )
                    }

              op: tPIPE    | tCARET     | tAMPER2 | tCMP   | tEQ     | tEQQ
                | tMATCH   | tGT        | tGEQ    | tLT    | tLEQ    | tLSHFT
                | tRSHFT   | tPLUS      | tMINUS  | tSTAR2 | tSTAR   | tDIVIDE
                | tPERCENT | tPOW       | tTILDE  | tUPLUS | tUMINUS | tAREF
                | tASET    | tBACK_REF2

        reswords: k__LINE__ | k__FILE__   | klBEGIN | klEND  | kALIAS  | kAND
                | kBEGIN    | kBREAK      | kCASE   | kCLASS | kDEF    | kDEFINED
                | kDO       | kELSE       | kELSIF  | kEND   | kENSURE | kFALSE
                | kFOR      | kIN         | kMODULE | kNEXT  | kNIL    | kNOT
                | kOR       | kREDO       | kRESCUE | kRETRY | kRETURN | kSELF
                | kSUPER    | kTHEN       | kTRUE   | kUNDEF | kWHEN   | kYIELD
                | kIF_MOD   | kUNLESS_MOD | kWHILE_MOD | kUNTIL_MOD | kRESCUE_MOD

             arg: lhs tEQL arg
                    {
		      #  arg: lhs tEQL arg
                      result = self.node_assign(val[vofs ], val[vofs + 2])
                    }
                | lhs tEQL arg kRESCUE_MOD arg
                    {
		      # | lhs tEQL arg kRESCUE_MOD arg
         # result = self.node_assign(val_[0], s(:rescue, val_[2], s(:resbody, s(:array), val_[4])) )
                      resbody = RubyRescueBodyNode.s(nil, val[vofs + 4])
                      resbody.src_offset=( val[vofs + 3].src_offset )   # kRESCUE_MOD position
                      rescue_nod = RubyRescueNode.s( val[vofs + 2], resbody, nil)
                      rescue_nod.src_offset=( val[vofs + 1].src_offset )   # tEQL position
                      result = self.node_assign(val[vofs ], rescue_nod )        # Fix trac 545
                    }
                | var_lhs tOP_ASGN arg
                    {
		      # | var_lhs tOP_ASGN arg
                      result = new_op_asgn( val, vofs )
                    }
                | primary_value tLBRACK_STR aref_args tRBRACK tOP_ASGN arg
                    {
		      # | primary_value tLBRACK_STR aref_args tRBRACK tOP_ASGN arg
                      v_two = val[vofs + 2]
                      unless v_two.class.equal?(RubyRpCallArgs) ; 
                        raise_error('aref_args is not a RubyRpCallArgs')
                      end
                      result = RubyOpElementAsgnNode.s(val[vofs ], v_two, val[vofs + 4], val[vofs + 5])
                    }
                | primary_value tDOT tIDENTIFIER tOP_ASGN arg
                    {
		      # | primary_value tDOT tIDENTIFIER tOP_ASGN arg
                      # result = s(:op_asgn2, val_[0], :"#{val_[2]}=", val_[3].to_sym, val_[4])
                      # val_[2], val_[3] are RpNameToken
                     result = RubyOpAsgnNode.s(val[vofs ], val[vofs + 2], val[vofs + 3], val[vofs + 4])
                    }
                | primary_value tDOT tCONSTANT tOP_ASGN arg
                    {
		     # | primary_value tDOT tCONSTANT tOP_ASGN arg
                     # result = s(:op_asgn2, val_[0], :"#{val_[2]}=", val_[3].to_sym, val_[4])
                     # val_[2], val_[3] are RpNameToken
	             result = RubyOpAsgnNode.s(val[vofs ], val[vofs + 2], val[vofs + 3], val[vofs + 4])
                    }
                | primary_value tCOLON2 tIDENTIFIER tOP_ASGN arg
                    {
		      # | primary_value tCOLON2 tIDENTIFIER tOP_ASGN arg
                      raise_error(":op_asgn never seen from MRI parser ")
                      # result = s(:op_asgn, val_[0], val_[4], val_[2], val_[3])
                      result = nil
                    }
                | primary_value tCOLON2 tCONSTANT tOP_ASGN arg

                    {
		      # | primary_value tCOLON2 tCONSTANT tOP_ASGN ar
                      yyerror "constant re-assignment"
                    }
                | tCOLON3 tCONSTANT tOP_ASGN arg
                    {
		      # | tCOLON3 tCONSTANT tOP_ASGN arg
                      yyerror "constant re-assignment"
                    }
                | backref tOP_ASGN arg
                    {
		      # | backref tOP_ASGN arg
                      self.backref_assign_error( val[vofs ])  
                      result = val[vofs]
                    }
                | arg tDOT2 arg
                    {
		      # ! arg tDOT2 arg
                      v1 = val[vofs ]
                      v2 = val[vofs + 2]
                      result = RubyDotNode.s(:dot2, v1, v2)
                    }
                | arg tDOT3 arg
                    {
		      # | arg tDOT3 arg
                      v1 = val[vofs ]
                      v2 = val[vofs + 2]
                      result = RubyDotNode.s(:dot3, v1, v2)
                    }
                | arg tPLUS arg
                    {
		      # | arg tPLUS arg
                      result = new_call_1(val[vofs ], val[vofs + 1],  val[vofs + 2]  ) 
                    }
                | arg tMINUS arg
                    {
		      # | arg tMINUS arg
                      result = new_call_1(val[vofs ], val[vofs + 1],  val[vofs + 2]  )
                    }
                | arg tSTAR2 arg
                    {
		      # | arg tSTAR2 arg
                      result = new_call_1(val[vofs ], val[vofs + 1], val[vofs + 2]  )
                    }
                | arg tDIVIDE arg
                    {
		      # | arg tDIVIDE arg
                      result = new_call_1( val[vofs ], val[vofs + 1], val[vofs + 2] )  # tDIVIDE_a
                    }
                | arg tPERCENT arg
                    {
		      # | arg tPERCENT arg
                      result = new_call_1(val[vofs ], val[vofs + 1] , val[vofs + 2] )  # tPERCENT_a
                    }
                | arg tPOW arg
                    {
		      # | arg tPOW arg
                      result = new_call_1(val[vofs ], val[vofs + 1], val[vofs + 2] ) # tPOW_a
                    }
                | tUMINUS_NUM tINTEGER tPOW arg
                    {
		      # | tUMINUS_NUM tINTEGER tPOW arg
                      lit_one =  RubyAbstractNumberNode.s( val[vofs + 1])  # s(:lit )
                      pow_sel = val[vofs + 2]
                      minus_sel = RpNameToken.new( :"-@" , pow_sel.src_offset )
                      result = new_vcall( new_call_1( lit_one, pow_sel, val[vofs + 3] ), minus_sel )
                    }
                | tUMINUS_NUM tFLOAT tPOW arg
                    {
		      # | tUMINUS_NUM tFLOAT tPOW arg
                      lit_one = RubyAbstractNumberNode.s( val[vofs + 1])  # s(:lit )
                      pow_sel = val[vofs + 2]
                      minus_sel = RpNameToken.new( :"-@" , pow_sel.src_offset )
                      result = new_vcall(new_call_1( lit_one, pow_sel, val[vofs + 3] ), minus_sel )
                    }
                | tUPLUS arg
                    {
		      # | tUPLUS arg
                      v_one = val[vofs + 1]
                      if v_one.kind_of?(RubyAbstractLiteralNode) # val_[1][0] == :lit 
                        result = v_one
                      else
                        sel_tok = RpNameToken.new( :"+@" , val[vofs ].src_offset )
                        result = new_vcall( v_one, sel_tok )
                      end
                    }
                | tUMINUS arg
                    {
		      #  | tUMINUS arg
                      sel_tok = RpNameToken.new( :"-@" , val[vofs ].src_offset )
                      result = new_vcall( val[vofs + 1], sel_tok )
                    }
                | arg tPIPE arg
                    {
		      # | arg tPIPE arg
                      result = new_call_1( val[vofs ], val[vofs + 1],  val[vofs + 2] )
                    }
                | arg tCARET arg
                    {
		      # | arg tCARET arg
                      result = new_call_1(val[vofs ], val[vofs + 1],  val[vofs + 2] )
                    }
                | arg tAMPER2 arg
                    {
		      # | arg tAMPER2 arg
                      result = new_call_1(val[vofs ], val[vofs + 1],  val[vofs + 2] )
                    }
                | arg tCMP arg
                    {
		      # | arg tCMP arg
                      result = new_call_1(val[vofs ], val[vofs + 1], val[vofs + 2] )
                    }
                | arg tGT arg
                    {
		      # | arg tGT arg
                      result = new_call_1(val[vofs ], val[vofs + 1], val[vofs + 2] )
                    }
                | arg tGEQ arg
                    {
		      # | arg tGEQ arg
                      result = new_call_1(val[vofs ], val[vofs + 1], val[vofs + 2] )
                    }
                | arg tLT arg
                    {
		      # | arg tLT arg
                      result = new_call_1(val[vofs ], val[vofs + 1],  val[vofs + 2] )
                    }
                | arg tLEQ arg
                    {
		      # | arg tLEQ arg
                      result = new_call_1(val[vofs ], val[vofs + 1],  val[vofs + 2] )
                    }
                | arg tEQ arg
                    {
		      # | arg tEQ arg
                      result = new_call_1(val[vofs ], val[vofs + 1], val[vofs + 2] )
                    }
                | arg tEQQ arg
                    {
		      # | arg tEQQ arg
                      result = new_call_1(val[vofs ], val[vofs + 1],  val[vofs + 2] )
                    }
                | arg tNEQ arg
                    {
		      # | arg tNEQ arg
					# TODOryan: port call_op and clean these
                      v_zero = value_expr(val[vofs ] ) 
                      v_two = value_expr( val[vofs + 2])
                      sel_tok = RpNameToken.new( :"==" , val[vofs + 1].src_offset ) 
                      result = RubyNotNode.s( new_call_1(v_zero, sel_tok,  v_two ) ) # s(:not )
                    }
                | arg tMATCH arg
                    {
		      # | arg tMATCH arg
                      result = self.get_match_node( val[vofs ], val[vofs + 1], val[vofs + 2] )
                    }
                | arg tNMATCH arg
                    {
		      # | arg tNMATCH arg
                      result = RubyNotNode.s( self.get_match_node(val[vofs ], val[vofs + 1], val[vofs + 2])) # s(:not )
                    }
                | tBANG arg
                    {
		      # | tBANG arg
                      result = RubyNotNode.s( val[vofs + 1]) # s(:not )
                    }
                | tTILDE arg
                    {
		      # | tTILDE arg           # val_[0] already a NameToken
                      # val_[2] = value_expr( val[2]) # TODO ask ryan , why ??
                      # result = new_call val_[1], :"~", s(:arglist)
                      v_one = value_expr( val[vofs + 1] )
                      result = new_vcall( v_one, val[vofs ] )
                    }
                | arg tLSHFT arg
                    {
		      # | arg tLSHFT arg
                      v_zero = value_expr( val[vofs ])
	              v_two = value_expr( val[vofs + 2])
                      result = new_call_1(v_zero, val[vofs + 1],  v_two  )
                    }
                | arg tRSHFT arg
                    {
		      # | arg tRSHFT arg
                      v_zero = value_expr( val[vofs ])
                      v_two = value_expr( val[vofs + 2])
                      result = new_call_1(v_zero , val[vofs + 1],  v_two )
                    }
                | arg tANDOP arg
                    {
		      # | arg tANDOP arg
                      result = logop( RubyAndNode, val[vofs ], val[vofs + 2]) # s(:and )
                    }
                | arg tOROP arg
                    {
		      # | arg tOROP arg
                      result = logop( RubyOrNode, val[vofs ], val[vofs + 2]) # s(:or )
                    }
                | kDEFINED opt_nl arg
                    {
		      # | kDEFINED opt_nl arg
                      result = RubyDefinedNode.s( val[vofs + 2])  # s(:defined )
                    }
                | arg tEH arg tCOLON arg
                    {
		      # | arg tEH arg tCOLON arg
                      result = RubyIfNode.s(val[vofs ], val[vofs + 2], val[vofs + 4]) # s(:if )
                    }
                | primary

       arg_value: arg
                    { 
		      # arg_value: arg
                      result = value_expr(val[vofs ])
                    }

       aref_args: none
                | command opt_nl
                    {
		      #  | command opt_nl
                      warning 'parenthesize argument(s) for future version'
                      # result = s(:array, val_[0])
                      result = RubyRpCallArgs.s( val[vofs ])
                    }
                | args trailer
                    {
		      # | args trailer
                      result = val[vofs ]
                    }
                | args tCOMMA tSTAR arg opt_nl
                    {
		      #  | args tCOMMA tSTAR arg opt_nl
                      result = val[vofs ]
                      result.append( RubySplatNode.s( val[vofs + 3] ))
                    }
                | assocs trailer
                    {
		      #  | assocs trailer
                      # result = s(:array, s(:hash, *val_[0].values))
                      result = RubyRpCallArgs.s( RubyHashNode.s( val[vofs ] ))
                    }
                | tSTAR arg opt_nl
                    {
		      #  | tSTAR arg opt_nl
                      # result = s(:array, s(:splat, val_[1]))
                     result = RubyRpCallArgs.s( RubySplatNode.s( val[vofs + 1])) 
                    }

      paren_args: tLPAREN2 none tRPAREN
                    {
		      #  paren_args: tLPAREN2 none tRPAREN
                      result = val[vofs + 1]
                    }
                | tLPAREN2 call_args opt_nl tRPAREN
                    {
		      #  | tLPAREN2 call_args opt_nl tRPAREN
                      result = val[vofs + 1]
                    }
                | tLPAREN2 block_call opt_nl tRPAREN
                    {
		      #  | tLPAREN2 block_call opt_nl tRPAREN
                      warning "parenthesize argument(s) for future version"
                      # result = s(:array, val_[1])
                      result = RubyRpCallArgs.s( val[vofs + 1])
                    }
                | tLPAREN2 args tCOMMA block_call opt_nl tRPAREN
                    {
		      # | tLPAREN2 args tCOMMA block_call opt_nl tRPAREN
                      warning "parenthesize argument(s) for future version"
                      # result = val_[1].add val_[3]
                      result = val[vofs + 1].append(  val[vofs + 3] )
                    }

  opt_paren_args: none
                | paren_args

       call_args: command
                    {
		      # call_args: command
                      warning "parenthesize argument(s) for future version"
                      # result = s(:array, val_[0])
                      result = RubyRpCallArgs.s( val[vofs ])
                    }
                | args opt_block_arg
                    {
		      #  | args opt_block_arg
                      result = val[vofs ]  # should be a RubyRpCallArgs
                      result.append_blk_arg( val[vofs + 1])
                    }
                | args tCOMMA tSTAR arg_value opt_block_arg
                    {
		      #  | args tCOMMA tSTAR arg_value opt_block_arg
                      result = val[vofs ]  # should be a RubyRpCallArgs
                      result.append_arg( RubySplatNode.s( val[vofs + 3]) )
                      result.append_blk_arg( val[vofs + 4])
                    }
                | assocs opt_block_arg
                    {
		      #  | assocs opt_block_arg
                      # result = s(:array, s(:hash, *val_[0].values))
                      result = RubyRpCallArgs.s( RubyHashNode.s( val[vofs ] ))
                      result.append_blk_arg( val[vofs + 1] )
                    }
                | assocs tCOMMA tSTAR arg_value opt_block_arg
                    {
		      #  ! assocs tCOMMA tSTAR arg_value opt_block_arg
                      # result = self.arg_concat s(:array, s(:hash, *val_[0].values)), val_[3]
                      result = RubyRpCallArgs.s( RubyHashNode.s( val[vofs ]) )
                      result.append_arg( RubySplatNode.s( val[vofs + 3] ) )
                      result.append_blk_arg( val[vofs + 4] )
                    }
                | args tCOMMA assocs opt_block_arg
                    {
		      #  | args tCOMMA assocs opt_block_arg
                      # result = val_[0] << s(:hash, *val_[2].values)
                      result = val[vofs ]
                      result.append_arg( RubyHashNode.s( val[vofs + 2] ))
                      result.append_blk_arg( val[vofs + 3])
                    }
                | args tCOMMA assocs tCOMMA tSTAR arg opt_block_arg
                    {
		      #  | args tCOMMA assocs tCOMMA tSTAR arg opt_block_arg
                      # val_[0] << s(:hash, *val_[2].values)
                      result = val[vofs ]
                      result.append_arg( RubyHashNode.s( val[vofs + 2] ))
                      result.append_arg( RubySplatNode.s( val[vofs + 5] ))
                      result.append_blk_arg( val[vofs + 6])
                    }
                | tSTAR arg_value opt_block_arg
                    {
		      #  | tSTAR arg_value opt_block_arg
                      # result = self.arg_blk_pass s(:splat, val_[1]), val_[2]
                      result = RubyRpCallArgs.s( RubySplatNode.s( val[vofs + 1]))
                      result.append_blk_arg( val[vofs + 2] )
                    }
                | block_arg

      call_args2: arg_value tCOMMA args opt_block_arg
                    {
		      #  call_args2: arg_value tCOMMA args opt_block_arg
                      args = self.list_prepend( val[vofs + 2], val[vofs ] )
                      result = args.append_blk_arg( val[vofs + 3] )
                    }
                | arg_value tCOMMA block_arg
                    {
		      #  | arg_value tCOMMA block_arg
                      result = RubyRpCallArgs.s( val[vofs ])
                      result.append_blk_arg( val[vofs + 2])
                    }
                | arg_value tCOMMA tSTAR arg_value opt_block_arg
                    {
		      #  | arg_value tCOMMA tSTAR arg_value opt_block_arg
                      result = RubyRpCallArgs.s( val[vofs ])
                      result.append_arg( RubySplatNode.s( val[vofs + 3] ))
                      result.append_blk_arg( val[vofs + 4] )
                    }
                | arg_value tCOMMA args tCOMMA tSTAR arg_value opt_block_arg
                    {
		      #  ! arg_value tCOMMA args tCOMMA tSTAR arg_value opt_block_arg
                      #result = self.arg_concat s(:array, val_[0], s(:hash, *val_[2].values)), val_[5]
                      result = RubyRpCallArgs.s(val[vofs ], RubyHashNode.s( val[vofs + 2]) )
                      result.append_arg( val[vofs + 5])
                      result.append_blk_arg( val[vofs + 6] )
                    }
                | assocs opt_block_arg
                    {
		      #  | assocs opt_block_arg
                      # result = s(:array, s(:hash, *val_[0].values))
                      result = RubyRpCallArgs.s( RubyHashNode.s( val[vofs ] ))
                      result.append_blk_arg( val[vofs + 1])
                    }
                | assocs tCOMMA tSTAR arg_value opt_block_arg
                    {
		      #  ! assocs tCOMMA tSTAR arg_value opt_block_arg
                      result = RubyRpCallArgs.s( RubyHashNode.s( val[vofs ]), RubySplatNode.s(val[vofs + 3]) )
                      result.append_blk_arg( val[vofs + 4] )
                    }
                | arg_value tCOMMA assocs opt_block_arg
                    {
		      #  | arg_value tCOMMA assocs opt_block_arg
                      # result = s(:array, val_[0], s(:hash, *val_[2].values))
                      result = RubyRpCallArgs.s( val[vofs ], RubyHashNode.s( val[vofs + 2]))
                      result.append_blk_arg( val[vofs + 3] )
                    }
                | arg_value tCOMMA args tCOMMA assocs opt_block_arg
                    {
		      #  | arg_value tCOMMA args tCOMMA assocs opt_block_arg
                      result = RubyRpCallArgs.s( val[vofs ] )
                      result.appendAll( val[vofs + 2] ) 
                      result.append_arg( RubyHashNode.s( val[vofs + 4] ))
                      result.append_blk_arg( val[vofs + 5] )
                    }
                | arg_value tCOMMA assocs tCOMMA tSTAR arg_value opt_block_arg
                    {
		      #  | arg_value tCOMMA assocs tCOMMA tSTAR arg_value opt_block_arg
                      result = RubyRpCallArgs.s( val[vofs ] , RubyHashNode.s( val[vofs + 2] ))
                      result.append_arg( RubySplatNode.s( val[vofs + 5] ))
                      result.append_blk_arg( val[vofs + 6] )
                    }
                | arg_value tCOMMA args tCOMMA assocs tCOMMA tSTAR arg_value opt_block_arg
                    {
		      #  | arg_value tCOMMA args tCOMMA assocs tCOMMA tSTAR arg_value opt_block_arg
                      result = RubyRpCallArgs.s( val[vofs ] )
                      result.appendAll( val[vofs + 2] )  
                      result.append_arg( RubyHashNode.s( val[vofs + 4] ))
                      result.append_arg( RubySplatNode.s( val[vofs + 7]))
                      result.append_blk_arg( val[vofs + 8])
                    }
                | tSTAR arg_value opt_block_arg
                    {
		      #  | tSTAR arg_value opt_block_arg
                      result = RubyRpCallArgs.s( RubySplatNode.s( val[vofs + 1]))
                      result.result.append_blk_arg( val[vofs + 2] )
                    }
                | block_arg

    command_args:   {
		      #  command_args:
		      lex_cmdarg = @lexer.cmdarg_ 
                      result = lex_cmdarg.dup
                      lex_cmdarg.push( true )
                    }
                    open_args
                    {
		      #  open_args
                      # @lexer.cmdarg_.stack.replace( val_[0] )
                      @lexer.cmdarg=( val[vofs ].dup  )
                      result = val[vofs + 1]
                    }

       open_args: call_args
                | tLPAREN_ARG
                    {
		      # open_args: call_args #   | tLPAREN_ARG
                      @lexer.lex_state=( RubyLexer::Expr_endArg )
                      result = val[vofs]
                    }
                    tRPAREN
                    {
		      # open_args: call_args #   tRPAREN 
                      msg = "don't put space before argument parentheses"
                      if @mydebug ; msg << " (B)" ; end 
                      warning(msg)
                      result = nil
                    }
                | tLPAREN_ARG call_args2
                    {
		      #  | tLPAREN_ARG call_args2
                      @lexer.lex_state=( RubyLexer::Expr_endArg )
                      result = val[vofs]
                    }
                    tRPAREN
                    {
		      #  | tLPAREN_ARG call_args2
		      #    tRPAREN
                      msg = "don't put space before argument parentheses"
                      if @mydebug ; msg << " (C)" ; end 
                      warning(msg)
                      result = val[vofs + 1]
                    }

       block_arg: tAMPER arg_value
                    {
		      # block_arg: tAMPER arg_value
                      # result = s(:block_pass, val_[1])
                      result = RubyBlockPassNode.s(val[vofs + 1])
                    }

   opt_block_arg: tCOMMA block_arg
                    {
		      #  opt_block_arg: tCOMMA block_arg
                      result = val[vofs + 1]
                    }
                | none_block_pass

            args: arg_value
                    {
		      #  args: arg_value
                      result = RubyRpCallArgs.s( val[vofs ]) # s(:array )
                    }
                | args tCOMMA arg_value
                    {
		      #  | args tCOMMA arg_value
                      result = val[vofs ]
                      result.append_arg( val[vofs + 2])
                    }

            mrhs: args tCOMMA arg_value
                    {
		      # mrhs: args tCOMMA arg_value # result = val_[0] << val_[2] 
                      result = val[vofs ]
                      result.append_arg( val[vofs + 2] )
                    }
                | args tCOMMA tSTAR arg_value
                    {
		      # mrhs: args tCOMMA arg_value #  | args tCOMMA tSTAR arg_value
                      result = val[vofs ]
                      result.append_arg( RubySplatNode.s( val[vofs + 3]))
                    }
                | tSTAR arg_value
                    {
		      # mrhs: args tCOMMA arg_value #  | tSTAR arg_value
                      result = RubySplatNode.s( val[vofs + 1])  # s(:splat ) 
                    }

         primary: literal
                | strings
                | xstring
                | regexp
                | words
                | awords
                | var_ref
                | backref
                | tFID
                    {
		      # primary: literal
                      result = new_fcall( val[vofs ], nil) 
                    }
                | kBEGIN bodystmt kEND
                    {
		      # primary: kBEGIN bodystmt kEND
                      if val[vofs + 2].equal?( :tEOF )
                        raise SyntaxError, 'unexpected $end, expecting kEND for kBEGIN'
                      end
                      v_two = val[vofs + 1]  # the  bodystmt
                      unless v_two then
                        result = RubyNilNode._new # s(:nil)
                      else
                        result = RubyKBeginNode.s( v_two ) # s(:begin )
                      end
                    }
                | tLPAREN_ARG expr
                    {
		      # primary: #  | tLPAREN_ARG expr
                      @lexer.lex_state=( RubyLexer::Expr_endArg )
                      result = val[vofs]
                    }
                    opt_nl tRPAREN
                    {
		      # primary: #  opt_nl tRPAREN
                      warning "(...) interpreted as grouped expression"
                      result = val[vofs + 1]
                    }
                | tLPAREN compstmt tRPAREN
                    {
		      # primary: #  | tLPAREN compstmt tRPAREN
                      # result = val_[1] || s(:nil)
                      result = val[vofs + 1] || RubyNilNode._new 
                      result.paren=( true )
                    }
                | primary_value tCOLON2 tCONSTANT
                    {
		      # primary: #  | primary_value tCOLON2 tCONSTANT
                      # result = s(:colon2, val_[0], val_[2].to_sym) 
                      result = RubyColon2Node.s( val[vofs ], val[vofs + 2])
                    }
                | tCOLON3 tCONSTANT
                    {
		      # primary: # | tCOLON3 tCONSTANT
                      # result = s(:colon3, val_[1].to_sym)  
                      result = RubyColon3Node.s( val[vofs + 1])
                    }
                | primary_value tLBRACK_STR aref_args tRBRACK
                    {
		      # primary: #  | primary_value tLBRACK_STR aref_args tRBRACK
                      result = new_aref( val, vofs )
                    }
                | tLBRACK aref_args tRBRACK
                    {
		      # primary: #  | tLBRACK aref_args tRBRACK
                      # result = val_[1] || s(:array)
                      result = val[vofs + 1] || RubyRpCallArgs._new 
                    }
                | tLBRACE assoc_list tRCURLY
                    {
		      # primary:  # | tLBRACE assoc_list tRCURLY
                      # result = s(:hash, *val_[1].values)
                      result = RubyHashNode.s( val[vofs + 1] )
                    }
                | kRETURN
                    {
		      # primary:  # | kRETURN
                      # result = s(:return)
                      result = RubyReturnNode.s(nil)
		      result.src_offset=( val[vofs ].src_offset ) # of the kRETURN
                    }
                | kYIELD tLPAREN2 call_args tRPAREN
                    {
		      # primary:  # | kYIELD tLPAREN2 call_args tRPAREN
                      result = new_yield( val[vofs + 2])  
                      result.src_offset=( val[vofs ].src_offset ) # of the kYIELD
                    }
                | kYIELD tLPAREN2 tRPAREN
                    {
		      # primary:  # | kYIELD tLPAREN2 tRPAREN
                      result = new_yield_0
                      result.src_offset=( val[vofs ].src_offset ) # of the kYIELD
                    }
                | kYIELD
                    {
		      # primary:  # | kYIELD
                      result = new_yield_0
                      result.src_offset=( val[vofs ].src_offset ) # of the kYIELD
                    }
                | kDEFINED opt_nl tLPAREN2 expr tRPAREN
                    {
		      # primary:  # | kDEFINED opt_nl tLPAREN2 expr tRPAREN
                      result = RubyDefinedNode.s( val[vofs + 3])   # s(:defined )
                    }
                | operation brace_block
                    {
		      # primary:  # | operation brace_block
                      oper = val[vofs ]
                      iter = val[vofs + 1]
                      call = new_fcall( oper, nil  )  # zero args
                      # iter.insert(1, call)
                      iter.call=(call)
                      result = iter
                      # call.line ||= iter.line
                    }
                | method_call
                | method_call brace_block
                    {
		      # primary:  # | method_call
                      call = val[vofs ]
                      iter = val[vofs + 1]
                      # iter.insert(1, call)
                      iter.call=(call)
                      result = iter
                    }
                | kIF expr_value then compstmt if_tail kEND
                    {
		      # primary:  # | kIF expr_value then compstmt if_tail kEND
                      if val[vofs + 5].equal?( :tEOF )
                        premature_eof( val[vofs ] )
                      end
                      result = new_if( val[vofs + 1], val[vofs + 3], val[vofs + 4] )
                      result.src_offset=( val[vofs ].src_offset )
                    }
                | kUNLESS expr_value then compstmt opt_else kEND
                    {
		      # primary:  # | kUNLESS expr_value then compstmt opt_else kEND
                      if val[vofs + 5].equal?( :tEOF )
                        premature_eof( val[vofs ] )
                      end
                      result = new_if( val[vofs + 1], val[vofs + 4], val[vofs + 3])
                      result.src_offset=( val[vofs ].src_offset )
                    }
                | kWHILE
                    {
		      # primary:  # | kWHILE
                      @lexer.cond_.push( true )
                      result = val[vofs]
                    }
                    expr_value do
                    {
		      # kWHILE  # expr_value do
                      @lexer.cond_.pop
                      result = val[vofs]
                    }
                    compstmt kEND
                    {
		      # kWHILE  # compstmt kEND
                      if val[vofs + 6].equal?(:tEOF)
                        premature_eof( val[vofs ] )
                      end
                      result = new_while( val[vofs + 5], val[vofs + 2])
                      result.src_offset=( val[vofs ].src_offset) # kWhile is a RpNameToken
                    }
                | kUNTIL
                    {
		      # primary: | kUNTIL
                      @lexer.cond_.push( true )
                      result = val[vofs]
                    }
                    expr_value do
                    {
		      # kUNTIL # expr_value do
                      @lexer.cond_.pop
                      result = val[vofs]
                    }
                    compstmt kEND
                    {
		      # kUNTIL compstmt kEND
                      if val[vofs + 6].equal?(:tEOF)
                        premature_eof( val[vofs ] )
                      end
                      result = new_until( val[vofs + 5], val[vofs + 2] )
                      result.src_offset=( val[vofs ].src_offset ) # kUNTIL RpNameToken
                    }
                | kCASE expr_value opt_terms case_body kEND
                    {
		      #  | kCASE expr_value opt_terms case_body kEND
                      if val[vofs + 4].equal?( :tEOF )
                        premature_eof( val[vofs ] )
                      end
                      result = new_case( val[vofs + 1], val[vofs + 3])
                      result.src_offset=(   val[vofs ].src_offset ) # kCASE position
                    }
                | kCASE            opt_terms case_body kEND
                    {
		      # | kCASE    opt_terms case_body kEND
                      if val[vofs + 3].equal?( :tEOF )
                        premature_eof( val[vofs ] )
                      end
                      result = new_case( nil, val[vofs + 2] )
                      result.src_offset=(   val[vofs ].src_offset ) # kCASE position
                    }
                | kCASE opt_terms kELSE compstmt kEND # TODO: need a test
                    {
		      # | kCASE opt_terms kELSE compstmt kEND
                      if val[vofs + 4].equal?( :tEOF )
                        premature_eof( val[vofs ] )
                      end
                      result = new_case( nil, val[vofs + 3] )
                      result.src_offset=(   val[vofs ].src_offset ) # kCASE position
                    }
                | kFOR block_var kIN
                    {
		      # | kFOR block_var kIN
                      @lexer.cond_.push( true )
                      result = val[vofs]
                    }
                    expr_value do
                    {
		      # kFOR # expr_value do
                      @lexer.cond_.pop
                      result = val[vofs]
                    }
                    compstmt kEND
                    {
		      #  kFOR # compstmt kEND
                      if val[vofs + 8].equal?( :tEOF )
                        premature_eof( val[vofs ] )
                      end
                      result = new_for( val[vofs + 4], val[vofs + 1], val[vofs + 7])
                    }
                | kCLASS cpath superclass
                    {
		      # kCLASS cpath superclass
                      # @comments.push( @lexer.comments_ )
                      if (@in_def || @in_single > 0) then
                        yyerror "class definition in method body"
                      end
                      @env.extend( false)
                      result = val[vofs]
                    }
                    bodystmt kEND
                    {
		      # kCLASS # bodystmt kEND
                      result = new_class( val, vofs )
                      @env.unextend
                    }
                | kCLASS tLSHFT expr
                    {
		      # | kCLASS tLSHFT expr
                      result = @in_def
                      @in_def = false
                    }
                    term
                    {
		      # | kCLASS tLSHFT # term
                      result = @in_single
                      @in_single = 0
                      @env.extend( false)
                    }
                    bodystmt kEND
                    {
		      # | kCLASS tLSHFT # bodystmt kEND
                      result = new_sclass(val, vofs )
                      @env.unextend
                    }
                | kMODULE cpath
                    {
		      # | kMODULE cpath
                      # @comments.push( @lexer.comments_ )
                      if   @in_def or @in_single > 0
                        yyerror "module definition in method body" 
                      end
                      @env.extend( false)
                      result = val[vofs]
                    }
                    bodystmt kEND
                    {
		      # | kMODULE # bodystmt kEND
                      result = new_module( val, vofs )
                      @env.unextend
                    }
                | kDEF fname
                    {
		      #  | kDEF fname
                      lx = @lexer
                      # @comments.push( lx.comments_ )
                      @in_def = true
                      @env.extend( false)
                      result =  -907  # dummy result, replaces [line, beginOfLine]
                    }
                    f_arglist bodystmt kEND
                    {
		      #  | kDEF fname # f_arglist bodystmt kEND
                      result = new_defn( val , vofs )
                      @env.unextend
                      @in_def = false
                    }
                | kDEF singleton dot_or_colon
                    {
		      # | kDEF singleton dot_or_colon
                      lx = @lexer
                      # @comments.push( lx.comments_ )
                      lx.lex_state=( RubyLexer::Expr_fname )
                      result = val[vofs]
                    }
                    fname
                    {
		      # | kDEF singleton dot_or_colon # fname
                      @in_single += 1
                      @env.extend( false)
                      @lexer.lex_state=( RubyLexer::Expr_end )# force for args
                      result = val[vofs]
                    }
                    f_arglist bodystmt kEND
                    {
		      # | kDEF singleton dot_or_colon # f_arglist bodystmt kEND
                      result = new_defs( val , vofs )

                      @env.unextend
                      @in_single -= 1
                    }
                | kBREAK
                    {
		      # | kBREAK
                      result = RubyBreakNode.s(nil) # s(:break)
                      result.src_offset=( val[vofs ].src_offset ) #  of the kBREAK
                    }
                | kNEXT
                    {
		      # | kNEXT
                      result = RubyNextNode.s(nil) #  s(:next)
                      result.src_offset=( val[vofs ].src_offset ) #  of the kNEXT
                    }
                | kREDO
                    {
		      # | kREDO
                      result = RubyRedoNode._new # s(:redo)
                      result.src_offset=( val[vofs ].src_offset ) #  of the kREDO
                    }
                | kRETRY
                    {
		      # | kRETRY
                      result = RubyRetryNode._new # s(:retry)
                      result.src_offset=( val[vofs ].src_offset ) #  of the kRETRY
                    }

   primary_value: primary
                    {
		      # primary_value: primary
                      result = value_expr(val[vofs ])
                    }

            then: term
                | tCOLON
                | kTHEN
                | term kTHEN

              do: term
                | tCOLON
                | kDO_COND

         if_tail: opt_else
                | kELSIF expr_value then compstmt if_tail
                    {
		      # if_tail: opt_else ....
                      result = RubyIfNode.s(val[vofs + 1], val[vofs + 3], val[vofs + 4]) # s(:if )
                    }

        opt_else: none
                | kELSE compstmt
                    {
		      # opt_else: none ...
                      result = val[vofs + 1]
                    }

       block_var: lhs
                | mlhs
                    {
		      # block_var: lhs
                      # val_[0].delete_at 1 if val[0][1].nil? # HACK 
                      # Maglev, do nothing for now
                      result = val[vofs]
                    }

   opt_block_var: none
                | tPIPE tPIPE
                    {
		      # opt_block_var: none # | tPIPE tPIPE
                      result = 0
                    }
                | tOROP
                    {
		      # opt_block_var: none # | tOROP
                      result = 0
                    }
                | tPIPE block_var tPIPE
                    {
		      # opt_block_var: none # | tPIPE block_var tPIPE
                      result = val[vofs + 1]
                    }

        do_block: kDO_BLOCK
                    {
		      # do_block: kDO_BLOCK
		      @env.extend( true ) # (:dynamic)
                      result = val[vofs]
                    }
                    opt_block_var
                    {
		      # do_block: kDO_BLOCK # opt_block_var
                      result = @env.dynamic_keys
                    }
                    compstmt kEND
                    {
		      # do_block: kDO_BLOCK # compstmt kEND
                      if val[vofs + 5].equal?( :tEOF )
                        premature_eof( val[vofs] )  # of kDO
                      end
                      vars   = val[vofs + 2]
                      body   = val[vofs + 4]
                      result = new_iter(vars, body)
                      @env.unextend
                    }

      block_call: command do_block
                    {
		      # block_call: command do_block
                      v_zero = val[vofs ]
                      if v_zero.equal?(nil)
                        # ok
                      elsif v_zero.class.equal?(RubyBlockPassNode) 
                        raise SyntaxError, "Both block arg and actual block given." 
                      end
                      iter = val[vofs + 1]
                      iter.call=(v_zero)
                      result = iter
                    }
                | block_call tDOT operation2 opt_paren_args
                    {
		      # | block_call tDOT operation2 opt_paren_args
                      result = new_call(val[vofs ], val[vofs + 2], val[vofs + 3])
                    }
                | block_call tCOLON2 operation2 opt_paren_args
                    {
		      # | block_call tCOLON2 operation2 opt_paren_args
                      result = new_call(val[vofs ], val[vofs + 2], val[vofs + 3])
                    }

     method_call: operation paren_args
                    {
		      # method_call: operation  paren_args
                      result = new_fcall( val[vofs ], val[vofs + 1] )
                    }
                | primary_value tDOT operation2 opt_paren_args
                    {
		      # # method_call: | primary_value tDOT operation2 opt_paren_args
                      result = new_call(val[vofs ], val[vofs + 2], val[vofs + 3] )
                    }
                | primary_value tCOLON2 operation2 paren_args
                    {
		      # # method_call: | primary_value tCOLON2 operation2 paren_args
                      result = new_call(val[vofs ], val[vofs + 2], val[vofs + 3] )
                    }
                | primary_value tCOLON2 operation3
                    {
		      # # method_call: | primary_value tCOLON2 operation3
                      result = new_vcall( val[vofs ], val[vofs + 2] )
                    }
                | kSUPER paren_args
                    {
		      # # method_call: | kSUPER paren_args
                      result = new_super( val , vofs )
                    }
                | kSUPER
                    {
		      # method_call:  | kSUPER
                      result = RubyZSuperNode._new # s(:zsuper)
                      result.src_offset=( val[vofs].src_offset ) # of the kSUPER
                    }

     brace_block: tLCURLY
                    {
		      # brace_block: tLCURLY
		      @env.extend( true ) # (:dynamic)
                      result = -909 # @lexer.lineno_
                    }
                    opt_block_var
                    {
		      # brace_block: tLCURLY # opt_block_var
                      result = @env.dynamic_keys
                    }
                    compstmt tRCURLY
                    {
		      # brace_block: tLCURLY #  compstmt tRCURLY
                      # REFACTOR
                      args   = val[vofs + 2]
                      body   = val[vofs + 4]
                      result = new_iter(args, body)
                      @env.unextend
                    }
                | kDO
                    {
		      # brace_block: tLCURLY # | kDO
		      @env.extend( true ) # (:dynamic)
                      result = -910 # @lexer.lineno_      
                    }
                 opt_block_var
                    {
		      # brace_block: tLCURLY # opt_block_var
                      result = @env.dynamic_keys
                    }
                    compstmt kEND
                    {
		      # brace_block: tLCURLY # compstmt kEND
                      if val[vofs + 5].equal?( :tEOF )
                        premature_eof( val[vofs + 1] )
                      end
                      args = val[vofs + 2]
                      body = val[vofs + 4]
                      result = new_iter(args, body)
                      @env.unextend
                    }

       case_body: kWHEN when_args then compstmt cases
                    {
                      result = RubyWhenNode.s( val[vofs + 1], val[vofs + 3], val[vofs + 4])  # s(:when )
                      result.src_offset=(   val[vofs ].src_offset ) # kWHEN position
                    }

       when_args: args
                | args tCOMMA tSTAR arg_value
                    {
		      # when_args: args  # | args tCOMMA tSTAR arg_value
                      wh = RubyWhenNode.s(val[vofs + 3], nil, nil)
                      wh.src_offset=(   val[vofs + 2].src_offset ) # tSTAR position
                      result = self.list_append( val[vofs ], wh )
                    }
                | tSTAR arg_value
                    {
		      # # when_args: args # | tSTAR arg_value
                      # result = s(:array, s(:when, val_[1], nil))
                      wh = RubyWhenNode.s( val[vofs + 1], nil, nil)
                      wh.src_offset=(   val[vofs ].src_offset ) # tSTAR position
                      result = RubyRpCallArgs.s( wh )
                    }

           cases: opt_else | case_body

      opt_rescue: kRESCUE exc_list exc_var then compstmt opt_rescue
                    {
	              # opt_rescue: kRESCUE exc_list exc_var then compstmt opt_rescue
                      # klasses, var, body, rest = val_[1], val_[2], val_[4], val_[5]
                      # klasses ||= s(:array )  # Maglev not used
                      klasses = val[vofs + 1]   # ok if nil
                      var = val[vofs + 2]
                      body = val[vofs + 4]
                      rest = val[vofs + 5] 
                      if var 
                        rhs = RubyGlobalVarNode.s( :"$!" )  # s(:gvar )
                        asgn = node_assign(var, rhs )
                        if body.equal?(nil)
                          body = RubyBlockNode.s( [ asgn ] )
                        else
                          body = body.prepend_to_block( asgn )
                        end
                      end   
                      result = RubyRescueBodyNode.s(klasses, body, rest)   # s(:resbody )
                      result.src_offset=( val[vofs ].src_offset )   # kRESCUE position
                    }
                |
                    {
		      # opt_rescue: # |
                      result = nil
                    }

        exc_list: arg_value
                    {
		      # exc_list: arg_value
                      result = RubyArrayNode.s( val[vofs ])  # s(:array )
                    }
                | mrhs
                | none

         exc_var: tASSOC lhs
                    {
		      # exc_var: tASSOC lhs
                      result = val[vofs + 1]
                    }
                | none

      opt_ensure: kENSURE compstmt
                    {
		      # opt_ensure: kENSURE compstmt
                      v_one = val[vofs + 1]
                      if (v_one != nil) then
                        result = v_one
                      else
                        result = RubyNilNode._new # s(:nil)
                      end
                    }
                | none

         literal: numeric { 
			    # literal: numeric
                            result = RubyAbstractNumberNode.s( val[vofs ])  # s(:lit )
                            # literal - numeric path 
                          }
                | symbol  { 
			    # literal: numeric # | symbol
		            result = RubySymbolNode.s( val[vofs ]) # s(:lit )
			    # literal - symbol path 
                          }  
                | dsym

         strings: string
                    {
		      # strings: string
                      # val_[0] = s(:dstr, val[0].value) if val[0][0] == :evstr 
                      # result = val_[0]
                      v_zero = val[vofs ] 
                      if v_zero.class.equal?(RubyEvStrNode)
                        result = RubyDStrNode.s( [ v_zero.evStrBody ] )
                      else
                        result = v_zero
                      end 
                    }

          string: string1
                | string string1
                    {
		      # string: string1
                      result = self.literal_concat( val[vofs ], val[vofs + 1])  
                    }

         string1: tSTRING_BEG string_contents tSTRING_END
                    {
		      # string1: tSTRING_BEG string_contents tSTRING_END
                      result = val[vofs + 1]
                    }
                | t_STRING
                    {
		      # string1: tSTRING_BEG string_contents tSTRING_END # | t_STRING
                      result = RubyStrNode.s( val[vofs ])  # s(:str )
                    }

         xstring: tXSTRING_BEG xstring_contents tSTRING_END
                    {
		      # xstring: tXSTRING_BEG xstring_contents tSTRING_END
                      result = new_xstring( val[vofs + 1])
                    }

          regexp: tREGEXP_BEG xstring_contents tREGEXP_END
                    {
		      # regexp: tREGEXP_BEG xstring_contents tREGEXP_END
                      result = new_regexp(val, vofs)
                    }

           words: tWORDS_BEG tSPACE tSTRING_END
                    {
		      # words: tWORDS_BEG tSPACE tSTRING_END
                      result = RubyArrayNode._new # s(:array)
                    }
                | tWORDS_BEG word_list tSTRING_END
                    {
		      # words: | tWORDS_BEG word_list tSTRING_END
                      result = val[vofs + 1]
                    }

       word_list: none
                    {
		      # word_list: none
                      result = RubyArrayNode._new # s(:array)
                    }
                | word_list word tSPACE
                    {
		      # word_list: # | word_list word tSPACE
                      # word = val_[1][0] == :evstr ? s(:dstr, "", val[1]) : val[1] #
                      # result = val_[0] << word
                      v_one = val[vofs + 1]
                      if v_one.class.equal?(RubyEvStrNode)
                        word = RubyDStrNode.s([ RubyStrNode.s('') , v_one ])
                      else
                        word = v_one
                      end
                      result = val[vofs ].append( word)  # v[0] should be a RubyArrayNode
                    }

            word: string_content
                | word string_content
                    {
		      # word: string_content
                      result = self.literal_concat( val[vofs ], val[vofs + 1])
                    }

          awords: tAWORDS_BEG tSPACE tSTRING_END
                    {
		      # awords: tAWORDS_BEG tSPACE tSTRING_END
                      result = RubyArrayNode._new # s(:array)
                    }
                | tAWORDS_BEG qword_list tSTRING_END
                    {
		      # awords: # | tAWORDS_BEG qword_list tSTRING_END
                      result = val[vofs + 1]
                    }

      qword_list: none
                    {
		      # qword_list: none
                      result = RubyArrayNode._new # s(:array)
                    }
                | qword_list tSTRING_CONTENT tSPACE
                    {
		      # qword_list: # | qword_list tSTRING_CONTENT tSPACE
                      # result = val_[0] << s(:str, val_[1]) # assume val[0] is ArrayNode 
                      result = val[vofs ].append( RubyStrNode.s(val[vofs + 1]))
                    }

 string_contents: none
                    {
			# string_contents: none
                      	# result = s(:str, "")
			result = RubyStrNode.s( "")
                    }
                | string_contents string_content
                    {
		      # string_contents: # | string_contents string_content
                      result = self.literal_concat(val[vofs ], val[vofs + 1])
                    }

xstring_contents: none
                    {
		      # xstring_contents: none
                      result = nil
                    }
                | xstring_contents string_content
                    {
		      # xstring_contents: # | xstring_contents string_content
                      result = self.literal_concat(val[vofs ], val[vofs + 1])
                    }

  string_content: tSTRING_CONTENT
                    {
		      # string_content: tSTRING_CONTENT
                      # result = s(:str, val_[0])
		      result = RubyStrNode.s( val[vofs ])
                    }
                | tSTRING_DVAR
                    {
		      # string_content: # | tSTRING_DVAR
                      lx = @lexer
                      result = lx.lex_strterm_
                      lx.lex_strterm=( nil )
                      lx.lex_state=( RubyLexer::Expr_beg )
                    }
                    string_dvar
                    {
		      # string_content: # string_dvar
                      # result = s(:evstr, val_[2]) 
                      @lexer.lex_strterm=( val[vofs + 1])
                      result = RubyEvStrNode.s( val[vofs + 2] )
                    }
                | tSTRING_DBEG
                    {
		      # string_content: # | tSTRING_DBEG
                      lx = @lexer
                      result = lx.lex_strterm_
                      lx.lex_strterm=( nil )
                      lx.lex_state=( RubyLexer::Expr_beg )
                      lx.cond_.push( false)
                      lx.cmdarg_.push( false)
                    }
                    compstmt tRCURLY
                    {
		      # string_content: # compstmt tRCURLY
                      lx = @lexer
                      lx.lex_strterm=( val[vofs + 1] )
                      lx.cond_.lexpop
                      lx.cmdarg_.lexpop

                      v_two = val[vofs + 2] 
                      if v_two.equal?(nil) 
                        result = RubyEvStrNode.s(nil) #  s(:evstr ) 
                      else 
                        knd = v_two.str_dstr_evstr_kind  # MNU here if "unknown rescue body"
                        if knd.equal?(nil) 
                           result = RubyEvStrNode.s(v_two)
                        else
                           result = v_two  # v_two is one of  :str :dstr: evstr
                        end
                      end
                    }

     string_dvar: tGVAR { 
			  # string_dvar: tGVAR 
                          # result = s(:gvar, val_[0].to_sym)   
			  result = RubyGlobalVarNode.s( val[vofs ].symval )
			}
                | tIVAR { 
			  # string_dvar: # | tIVAR
		          # result = s(:ivar, val_[0].to_sym) 
			  result = RubyInstVarNode.s( val[vofs ].symval )
			}
                | tCVAR { 
			  # string_dvar: # | tCVAR
	                  # result = s(:cvar, val_[0].to_sym) 
			  result = RubyClassVarNode.s( val[vofs ].symval)
			}
                | backref


          symbol: tSYMBEG sym
                    {
		      # symbol: tSYMBEG sym
                      @lexer.lex_state=( RubyLexer::Expr_end )
                      result = val[vofs + 1].symval  # expect an RpNameToken
                    }
                | tSYMBOL
                    {
		      # symbol: # | tSYMBOL
                      result = val[vofs ].to_sym
                    }

             sym: fname | tIVAR | tGVAR | tCVAR

            dsym: tSYMBEG xstring_contents tSTRING_END
                    {
		      # dsym: tSYMBEG xstring_contents tSTRING_END
                      @lexer.lex_state=( RubyLexer::Expr_end )
                      v_one = val[vofs + 1]

		      v_cls = v_one.class
		      if v_cls.equal?(RubyDStrNode)  # convert :dstr to :dsym
		        result =v_one.asDSymbolNode 
                      elsif v_cls.equal?(RubyStrNode) # convert :str to :sym
                        str = v_one.strNodeValue
                        if str.size.equal?(0)
                          yyerror "empty symbol literal"
                        end
			result = RubySymbolNode.s( str.to_sym )
                      elsif v_one.equal?( nil) 
                        yyerror "empty symbol literal" 
                        result = nil
                      elsif v_cls.equal?(RubyEvStrNode)
                        result = RubyDSymbolNode.s([ RubyStrNode.s('') , v_one ])
                        # result = s(:dsym, "", result)
                      else
			raise_error("unimplemented dsym conversion")
                        result = nil
                      end
                    }

         numeric: tINTEGER
                | tFLOAT
                | tUMINUS_NUM tINTEGER =tLOWEST
                    {
		      # numeric: tINTEGER ...
                      result = val[vofs + 1] * -1 # TODO: pt_testcase
                    }
                | tUMINUS_NUM tFLOAT   =tLOWEST
                    {
		      # numeric: # | tUMINUS_NUM tFLOAT   =tLOWEST
                      result = val[vofs + 1] * -1 # TODO: pt_testcase
                    }

        variable: tIDENTIFIER
                | tIVAR
                | tGVAR
                | tCONSTANT
                | tCVAR
                | kNIL      {  # variable: | kNIL
                              result = :nil      
                            }
                | kSELF     {  # variable: | kSELF
			      result = :self     
                            }
                | kTRUE     {  # variable: | kTRUE
			      result = :true     
                            }
                | kFALSE    {  # variable: | kFALSE
			      result = :false    
                            }
                | k__FILE__ { # variable: | k__FILE__
			      result = :__FILE__ 
                            }
                | k__LINE__ { # variable: | k__LINE__
			      result =  RpNameToken.new( :__LINE__ ,  @lexer.line_num_)  
                            }

         var_ref: variable
                    {
		      # var_ref: variable
                      result = self.gettable( val[vofs ])
                    }

         var_lhs: variable
                    {
		      # var_lhs: variable
                      result = self.assignable(val[vofs ], nil)
                    }

         backref: tNTH_REF  { 
			      # backref: tNTH_REF
	                      # result = s(:nth_ref,  val_[0]) 
			      result = RubyNthRefNode.s(val[vofs ])
			    }
                | tBACK_REF { 
			      # backref: #  tBACK_REF
			      # result = s(:back_ref, val_[0])
			      result = RubyBackRefNode.s( val[vofs ] )
			    }

      superclass: term
                    {
		      # superclass: term
                      result = nil
                    }
                | tLT
                    {
		      # superclass: # | tLT
                      @lexer.lex_state=( RubyLexer::Expr_beg )
                      result = val[vofs]
                    }
                    expr_value term
                    {
		      # superclass: # expr_value term
                      result = val[vofs + 2]
                    }
                | error term
                    {
		      # superclass: # | error term
                      yyerrok
                      result = nil
                    }

       f_arglist: tLPAREN2 f_args opt_nl tRPAREN
                    {
		      # f_arglist: tLPAREN2 f_args opt_nl tRPAREN
                      result = val[vofs + 1]
                      @lexer.lex_state=( RubyLexer::Expr_beg )
                    }
                | f_args term
                    {
		      # f_arglist: # | f_args term
                      result = val[vofs ]
                    }

          f_args: f_arg tCOMMA f_optarg tCOMMA f_rest_arg opt_f_block_arg
                    {
		      # f_args: f_arg tCOMMA f_optarg tCOMMA f_rest_arg opt_f_block_arg
                      # result = args( val_[0], val_[2], val_[4], val_[5])
                      result = val[vofs ]
                      result.add_optional_arg( val[vofs + 2] )
                      result.add_star_arg( val[vofs + 4] )
                      result.add_block_arg( val[vofs + 5] )
                    }
                | f_arg tCOMMA f_optarg                opt_f_block_arg
                    {
		      # f_args: # | f_arg tCOMMA f_optarg  opt_f_block_arg
                      # result = args( val_[0], val_[2],    nil, val_[3])
                      result = val[vofs ] 
                      result.add_optional_arg( val[vofs + 2] )
                      result.add_block_arg( val[vofs + 3] )
                    }
                | f_arg tCOMMA              f_rest_arg opt_f_block_arg
                    {
		      # f_args: # | f_arg tCOMMA   f_rest_arg opt_f_block_arg
                      # result = args( val_[0],    nil, val_[2], val_[3])
                      result = val[vofs ] 
                      result.add_star_arg( val[vofs + 2] )
                      result.add_block_arg( val[vofs + 3] )
                    }
                | f_arg                             opt_f_block_arg
                    {
		      # f_args: # | f_arg  opt_f_block_arg
                      # result = args( val_[0],    nil,    nil, val_[1])
                      result = val[vofs ] 
                      result.add_block_arg( val[vofs + 1] )
                    }
                |           f_optarg tCOMMA f_rest_arg opt_f_block_arg
                    {
		      # f_args: # |    f_optarg tCOMMA f_rest_arg opt_f_block_arg
                      # result = args(    nil, val_[0], val_[2], val_[3])
                      result = RubyArgsNode._new
                      result.add_optional_arg(val[vofs ] )
                      result.add_star_arg( val[vofs + 2] )
                      result.add_block_arg( val[vofs + 3] )
                    }
                |           f_optarg                opt_f_block_arg
                    {
		      # f_args: # |  f_optarg  opt_f_block_arg
                      # result = args(    nil, val_[0],    nil, val_[1])
                      result = RubyArgsNode._new
                      result.add_optional_arg(val[vofs ])
                      result.add_block_arg( val[vofs + 1] )

                    }
                |                        f_rest_arg opt_f_block_arg
                    {
		      # f_args: # |   f_rest_arg opt_f_block_arg
                      # result = args(    nil,    nil, val_[0], val_[1])
                      result = RubyArgsNode._new
                      result.add_star_arg( val[vofs ] )
                      result.add_block_arg( val[vofs + 1] )
                    }
                |                                       f_block_arg
                    {
		      # f_args: # |   f_block_arg
                      # result = args(    nil,    nil,    nil, val_[0])
                      result = RubyArgsNode._new
                      result.add_block_arg( val[vofs ] )
                    }
                |
                    {
		      # f_args: # | #
                      #result = args(    nil,    nil,    nil,    nil)
                      result = RubyArgsNode._new
                    }

      f_norm_arg: tCONSTANT
                    {
		      # f_norm_arg: tCONSTANT
                      yyerror "formal argument cannot be a constant: #{val[vofs ]}"
                      result = val[vofs]
                    }
                | tIVAR
                    {
		      # f_norm_arg: # | tIVAR
                      yyerror "formal argument cannot be an instance variable"
                      result = val[vofs]
                    }
                | tCVAR
                    {
		      # f_norm_arg: # | tCVAR
                      yyerror "formal argument cannot be a class variable"
                      result = val[vofs]
                    }
                | tIDENTIFIER
                    {
		      # f_norm_arg: # | tIDENTIFIER
                      v_zero = val[vofs ]  # val_[0] will be a RpNameToken
                      @env[ v_zero.symval ] = :lvar
                      result = v_zero
                    }

           f_arg: f_norm_arg
                    {
		      # f_arg: f_norm_arg
                      # result = s(:args)
                      # result << val_[0].to_sym
                      result = RubyArgsNode._new
                      result.add_arg( val[vofs ].symval )
                    }
                | f_arg tCOMMA f_norm_arg
                    {
		      # f_arg: # | f_arg tCOMMA f_norm_arg
                      result = val[vofs ]
                      result.add_arg( val[vofs + 2].symval )
                    }

           f_opt: tIDENTIFIER tEQL arg_value
                    {
		      # f_opt: tIDENTIFIER tEQL arg_value
                      result = self.assignable(val[vofs ], val[vofs + 2])
                      # TODO: detect duplicate names  ??
                    }

        f_optarg: f_opt
                    {
		      # f_optarg: f_opt
                      result = RubyBlockNode.s( [ val[vofs ] ] ) # s(:block )
                    }
                | f_optarg tCOMMA f_opt
                    {
		      # f_optarg: # | f_optarg tCOMMA f_opt
                      result = val[vofs ] # a RubyBlockNode
                      result.append_to_block( val[vofs + 2] )
                    }

    restarg_mark: tSTAR2 | tSTAR

      f_rest_arg: restarg_mark tIDENTIFIER
                    {
		      # f_rest_arg: restarg_mark tIDENTIFIER
                      # TODO: differs from parse.y - needs tests
                      name = val[vofs + 1].symval    # expect a RpNameToken
                      self.check_assignable( name ) # updates env
                      result = name   # MagLev, no prefixing of f_rest_arg with '*' 
                    }
                | restarg_mark
                    {
		      # f_rest_arg: # | restarg_mark
                      name = :"*"
                      @env[name] = :lvar
                      result = name
                    }

     blkarg_mark: tAMPER2 | tAMPER

     f_block_arg: blkarg_mark tIDENTIFIER
                    {
		      # f_block_arg: blkarg_mark tIDENTIFIER
                      identifier = val[vofs + 1].symval

                      @env[identifier] = :lvar
                      # result = s(:block_arg, identifier.to_sym)
		      result = RubyBlockArgNode.s( identifier)
                    }

 opt_f_block_arg: tCOMMA f_block_arg
                    {
		      # opt_f_block_arg: tCOMMA f_block_arg
                      result = val[vofs + 1]
                    }
                |
                    {
		      # opt_f_block_arg: # | #
                      result = nil
                    }

       singleton: var_ref
                | tLPAREN2
                    {
		      # singleton: var_ref
                      @lexer.lex_state=( RubyLexer::Expr_beg )
                      result = val[vofs]
                    }
                    expr opt_nl tRPAREN
                    {
		      # singleton: # expr opt_nl tRPAREN
                      result = val[vofs + 2]
		      if result.kind_of?(RubyAbstractLiteralNode)
                        yyerror "Can't define singleton method for literals." 
		      end
                    }

      assoc_list: none # [!nil]
                    {
		      # assoc_list: none
                      result = RubyArrayNode._new # s(:array)
                    }
                | assocs trailer # [!nil]
                    {
		      # assoc_list: # | assocs trailer
                      result = val[vofs ]
                    }
                | args trailer
                    {
		      # assoc_list: # | args trailer
                      # size = val_[0].size
                      # if (size % 2 != 1) then # != 1 because of leading :array
                      v_zero = val[vofs ]
                      size = v_zero.arrayLength
                      unless (size & 1).equal?(0)
                        yyerror "Odd number (#{size}) list for Hash. #{v_zero.inspect}"
                      end
                      result = v_zero
                    }

          assocs: assoc
                | assocs tCOMMA assoc
                    {
		      # assocs: assoc
                      # list = val_[0].dup
                      # more = val_[2][1..-1]            
                      # list.push(*more) unless more.empty?
                      list = val[vofs ].arrayDup  # dup a RubyArrayNode
                      v_two = val[vofs + 2]
                      list.appendAll(v_two) # expect v_two to be a RubyArrayNode
                      result = list
                    }

           assoc: arg_value tASSOC arg_value
                    {
		      # assoc: arg_value tASSOC arg_value
                      result = RubyArrayNode.s( val[vofs ], val[vofs + 2]) # s(:array )
                    }

       operation: tIDENTIFIER | tCONSTANT | tFID
      operation2: tIDENTIFIER | tCONSTANT | tFID | op
      operation3: tIDENTIFIER | tFID | op
    dot_or_colon: tDOT | tCOLON2
       opt_terms:  | terms
          opt_nl:  | tNL
         trailer:  | tNL | tCOMMA

            term: tSEMI {  # term: tSEMI
                          yyerrok ; result = val[vofs] 
                         }
                | tNL

           terms: term
                | terms tSEMI {  # term: # | terms tSEMI 
				yyerrok  ; result = val[vofs] 
				}

            none: {  # none:
		      result = nil  
		  }

 none_block_pass: {  # none_block_pass:
			result = nil 
		  }

end

---- inner

require "kernel/parser/lexer.rb"

# Local Variables: **
# racc-token-length-max:14 **
# End: **
