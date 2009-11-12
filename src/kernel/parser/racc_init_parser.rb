module MagRp # 

  class Parser
     Racc_reduce_n_negated = 0 - Racc_reduce_n
  end
  Parser.__freeze_constants

  class RubyParser

    def analyze_arr(arr, name)
      if PrintRaccArrayInfo
        n = 0
        sz = arr.size
        min = 0
        max = 0
        zerocount = 0
        nilcount = 0
        while n < sz
          v = arr[n]
          if v.equal?(nil)
            nilcount += 1
          else
            if v < min ; min = v ; end
            if v < max ; max = v ; end
            if v == 0 ; zerocount += 1 ; end
          end
          n += 1
        end
        puts "#{name} size #{sz} min #{min} max #{max} zeros #{zerocount} nils #{nilcount}" 
     end
    end

    def _racc_setup
      # initialize the instance variables that are constants produced
      #  during ruby_parser.rb initialization . 

      arg = Racc_arg

      # arg[13] = true if arg.size < 14
      unless arg[13].equal?(true)
         raise "unexpected racc_use_result_var==false"
      end
      @action_table   = arg[0].freeze
      analyze_arr(@action_table, "@action_table");
      @action_check   = arg[1].freeze
      analyze_arr(@action_check, "@action_check");
      @action_default = arg[2].freeze
      analyze_arr(@action_default, "@action_default");
      @action_pointer = arg[3].freeze
      analyze_arr(@action_pointer, "@action_pointer");
      @goto_table     = arg[4].freeze
      analyze_arr(@goto_table, "@goto_table");
      @goto_check     = arg[5].freeze
      analyze_arr(@goto_check, "@goto_check");
      @goto_default   = arg[6].freeze
      analyze_arr(@goto_default, "@goto_default");
      @goto_pointer   = arg[7].freeze
      analyze_arr(@goto_pointer, "@goto_pointer");
      # Racc_nt_base  is a constant # arg[8]
      reduc_tab = arg[9]
      reduc_len = reduc_tab.size
      idx = 0
      while idx < reduc_len 
        sym = reduc_tab[idx + 2]
        if sym.equal?( :_reduce_none ) && reduc_tab[idx].equal?( 1 )
          new_sym = :_reduce_noneOne  # for "no net change to vstack" optimization
        else
          new_sym = (sym.to_s << '::' ).__as_symbol  # so we can use __perform__se
        end
        reduc_tab[idx + 2] = new_sym
        idx += 3
      end
      @reduce_table   = reduc_tab.freeze

      id_h = IdentityHash.from_hash( arg[10] )
      id_h.freeze
      @token_table =  id_h 

      # Racc_shift_n is a constant 
      # Racc_reduce_n is a constant
      # Racc_use_result_var  is a constant
    end

    def self.init_template  
      t = self.new 
      t._racc_setup
      t
    end
      
    MagTemplate = init_template()

#    def _racc_yyparse_rb(recv, mid, arg, c_debug)
#      # probably not used by Maglev  # TODO determine for sure
#      raise_error('unexpected call to _racc_yyparse_rb')
#      
#      _racc_init_sysvars
#      act = nil
#      i = nil
#      nerr = 0
#
#      catch(:racc_end_parse) {
#        until i = @action_pointer[@racc_state[-1]]
#          while act = _racc_evalact(@action_default[@racc_state[-1]])
#            ;
#          end
#        end
#        recv.__send__(mid) do |tok, val|
#          unless tok
#            cracc_t = 0
#          else
#            cracc_t = (@token_table[tok] or 1)   # error token
#          end
#          @racc_t = cracc_t
#          @racc_val = val
#          @racc_read_next = false
#          i += cracc_t
#          unless i >= 0 and
#                 act = @action_table[i] and
#                 @action_check[i] == @racc_state[-1]
#            act = @action_default[@racc_state[-1]]
#          end
#          while act = _racc_evalact(act)
#            ;
#          end
#
#          while not (i = @action_pointer[@racc_state[-1]]) or
#                not @racc_read_next or
#                @racc_t == 0   # $
#            unless i and i += @racc_t and
#                   i >= 0 and
#                   act = @action_table[i] and
#                   @action_check[i] == @racc_state[-1]
#              act = @action_default[@racc_state[-1]]
#            end
#            while act = _racc_evalact(act)
#              ;
#            end
#          end
#        end
#      }
#    end
  end
  RubyParser.__freeze_constants

  __smalltalk_global_put(:RubyParser , RubyParser)  # store into UserGlobals

end
