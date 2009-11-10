#
# $Id: parser.rb,v 1.7 2005/11/20 17:31:32 aamine Exp $
#
# Copyright (c) 1999-2005 Minero Aoki
#
# This program is free software.
# You can distribute/modify this program under the same terms of ruby.
#
# As a special exception, when this code is copied by Racc
# into a Racc output file, you may use that output file
# without restriction.
#
# Hacked by Ben Matasar <ben@dabbledb.com> to work with MagLev.

# Copied from lib and code moved to module MagRp for the internal parser
# by Allen Otis.


module MagRp
  class RubyParser

    # initialziation of MagTemplate is in first opening in  racc_init_parser.rb

    def self.new_instance
      key = :MAGLEV_RubyParser_Template
      t = Maglev::System.session_temp( key )
      if t.equal?(nil)
        t = MagTemplate.dup # make copy of persistent template
        t._bind_instvars
        t._install_wordlist( Keyword.create_transient_wordlist )
        Maglev::System.session_temp_put( key, t)
      end
      res = t.dup
      res.initialize # lexer and other per-parse state
      res
    end
  end
end

module MagRp # {
  class Parser
    ###
    ### do_parse
    ###

    #def do_parse
    #  Maglev - this method not currently used
    #  __send__(Racc_Main_Parsing_Routine, _racc_setup(), false)
    #end

    #def next_token  # implemented in extras.rb
    #  raise NotImplementedError, "#{self.class}\#next_token is not defined"
    #end

    def _racc_do_parse_rb()
      _racc_init_sysvars
      tok = act = i = nil
      nerr = 0

      catch(:racc_end_parse) {
        cracc_state = @racc_state
        caction_pointer = @action_pointer
        caction_check = @action_check
        caction_default = @action_default
        ctoken_table = @token_table
        caction_table = @action_table
        lex = @lexer
        not_eof = true
        cracc_t = @racc_t
        while true
          ntok_holder = [nil, 0]
          if i = caction_pointer[cracc_state[-1]]
            if @racc_read_next
              if cracc_t != 0   # not EOF
                tok = lex.advance
                @racc_val = lex.yacc_value_
                if @mydebug
                  puts "next_token: #{tok.inspect} line #{lex.line_num_} yacc_value: #{@racc_val.inspect}"
                end
                if tok.equal?( :tEOF ) # EOF
                  if @env.is_extended
                    # attempt to issue premature eof error
                    tok = :kEND  # and yacc_value will be :eof
                    cracc_t = (ctoken_table[tok] or 1)
                  else
                    cracc_t = 0
                  end
                  @racc_t = cracc_t
                else
                  cracc_t = ctoken_table[tok]
                  unless cracc_t
                    cracc_t = 1  # error token
                    @racc_t = cracc_t
                  end
                end
                @racc_read_next = false
              end
            end
            i += cracc_t
            unless i >= 0 and
                   act = caction_table[i] and
                   caction_check[i] == cracc_state[-1]
              act = caction_default[cracc_state[-1]]
            end
          else
            act = caction_default[cracc_state[-1]]
          end
          while act = _racc_evalact(act)
            ;
          end
        end
      }
    end

    ###
    ### yyparse
    ###
#  not used by Maglev
#   def yyparse(recv, mid)
#     __send__(Racc_YY_Parse_Method, recv, mid, _racc_setup(), true)
#   end

    # _racc_yyparse_rb moved to  racc_init_parser.rb

    ###
    ### common
    ###

  def _racc_evalact(act) # [

    nerr = 0   # tmp

    cracc_state = @racc_state   # cracc_state hopefully on stack
    if act >= 0
      if act.equal?(0)   # TODO delete check
   raise_error("_racc_evalact, unexpected act==0")
      end
      if act._not_equal?(Racc_shift_n)
  # shift
  #
  cracc_error_status = @racc_error_status
  if cracc_error_status > 0
    unless @racc_t == 1   # error token
      @racc_error_status = cracc_error_status - 1
    end
  end
  @racc_vstack.push( @racc_val )
  # puts "VpushedA:   #{@racc_vstack.inspect} "   if @mydebug
  cracc_state.push act
  @racc_read_next = true
      else
  # accept
  #
  throw :racc_end_parse, @racc_vstack[0]
      end
    else
      if act._not_equal?(- Racc_reduce_n)
  # reduce
  #       replaced  catch(:racc_jump) which had only one use case
  #       with rescue of RaccJumpError
        last_len = 0
  begin
#     cracc_state.push _racc_do_reduce( act)
# inline _racc_do_reduce [
      state = cracc_state
      vstack = @racc_vstack

      i = act * -3
      creduce_table = @reduce_table
      len       = creduce_table[i]
      last_len = len
      # method_id = creduce_table[i+2]
      sel = creduce_table[i+2]

      if sel.equal?( :_reduce_noneOne )
  # optimization - no net change to vstack
        # state[-len, len]  = void_array
        state.size=( state.size - len )

  # puts "      #{sel}"                         if @mydebug
      else

        # Maglev: optimization , don't make copy of part of stack,
        #   pass reference to whole stack and offset to val[0]
        # tmp_v = vstack[-len, len]
        # vstack[-len, len] = void_array

        # state[-len, len]  = void_array
        state.size=( state.size - len )

        # puts "VvoidA:     #{vstack.inspect} "    if @mydebug

        # vstack must be updated AFTER method call
        #  Maglev:  use_result  is generated as constant true by .y-->.rb processing,
        #  Maglev: optimization use __perform
        #    and omit the tmp_v[0] arg since it is never used
        # if use_result
        #  vstack.push __send__(method_id, tmp_v, vstack, tmp_v[0])
        # else
        #  vstack.push __send__(method_id, tmp_v, vstack)
        # end
        #
        puts "      #{sel}"                         if @mydebug

        vstack_siz = vstack.size
        vofs = vstack_siz - len   # in a reduce method, val[0] is vstack[0 + vofs]

        # Maglev optimization use two args  , and pass stack and offset
        vres =  __perform__se( vstack, vofs, creduce_table[i+2], 2 );

        # delete last len elements of vstack and push vres
        vstack[vofs] = vres
        vstack.size=( vofs + 1)

        # puts "after #{sel}    #{vstack.inspect} "    if @mydebug
      end
      reduce_to = creduce_table[i+1]

      k1 = reduce_to - Racc_nt_base
      if i = @goto_pointer[k1]
        i += state[-1]
        if i >= 0 and (curstate = @goto_table[i]) and @goto_check[i] == k1
          cracc_state.push( curstate )
        else
          cracc_state.push( @goto_default[k1] )
        end
      else
        cracc_state.push( @goto_default[k1] )
      end
# inline _racc_do_reduce ]
  rescue Exception
            ex = $!
            vstk = @racc_vstack
            vstk.size=( vstk.size - last_len )
            if ex.class.equal?(RaccJumpError)
        # when 1 # yyerror
        @racc_user_yyerror = true   # user_yyerror
        return  - Racc_reduce_n
            else
              __reraise(ex)
            end
  end
      elsif act.equal?( - Racc_reduce_n)
  # error
  #
        cracc_error_status = @racc_error_status
        if cracc_error_status.equal?(0)
          if @racc_t.equal?( 0)   # is EOF
            throw :racc_end_parse, nil
          end
          unless @racc_user_yyerror # unless arg[21]    # user_yyerror
            nerr += 1
            on_error( @racc_t, @racc_val, @racc_vstack )
          end
        elsif cracc_error_status.equal?(3)
          if @racc_t.equal?( 0)   # is $   # EOF
            throw :racc_end_parse, nil
          end
          @racc_read_next = true
        end
        @racc_user_yyerror = false
        @racc_error_status = 3
        cracc_vstack = @racc_vstack
        caction_table = @action_table
        caction_check = @action_check
        caction_pointer = @action_pointer
        while true
          if i = caction_pointer[cracc_state[-1]]
            i += 1   # error token
            if  i >= 0 and
                (act = caction_table[i]) and
                caction_check[i] == cracc_state[-1]
              break
            end
          end
          throw :racc_end_parse, nil if cracc_state.size <= 1
          cracc_state.pop
          cracc_vstack.pop
          # puts "VpopB:    #{cacc_vstack.inspect} "    if @mydebug
        end
        return act
      else
        raise "[Racc Bug] unknown action #{act.inspect}"
      end
    end
    nil
  end  # ]

    def on_error(t, val, vstack)
      # reworked for better messages, not sure how to continue parsing.
      str = "parse error on value #{val.inspect} "
      hint = ""
      if t._not_equal?(nil)
        str << " #{token_to_str(t)} "
      end
      if val.equal?(nil)
        hint = " (check for incomplete statement)"
        str << hint
      elsif val == 'end'
        if @lexer.near_eos?(3)
          ofs = @env.first_top_level_def_offset
          if ofs > 0
            lnum = @lexer.line_for_offset(ofs)
            hint = " (check end(s) before 'def' near line #{lnum})"
            str << hint
          else
          end
        else
        end
      end
      puts str
      puts "SyntaxError: missing or unexpected #{val.inspect} #{hint}, near line #{@lexer.lineno_} "
      @syntax_err_count += 1
      raise SyntaxError
    end

    def yyerror(msg = "")
      # Enter error recovering mode.
      #  If this method returns,
      #  parser should enter "error recovering mode". ??
      msg << ", near line #{@lexer.lineno_} "
      puts "SyntaxError: #{msg}"
      # throw :racc_jump, 1
      @syntax_err_count += 1
      raise RaccJumpError
      # raise SyntaxError, msg
    end

    def yyaccept
      # appears to be unused
      #  Exit parser.
      raise_error('unexpected call to yyaccept')
      # throw :racc_jump, 2
    end

    def yyerrok
      # Leave error recovering mode.
      @racc_error_status = 0
    end

    #
    # for debugging output
    #

    def racc_read_token(t, tok, val)
      @racc_debug_out.print 'read    '
      @racc_debug_out.print tok.inspect, '(', racc_token2str(t), ') '
      @racc_debug_out.puts val.inspect
      @racc_debug_out.puts
    end

    def racc_shift(tok, tstack, vstack)
      @racc_debug_out.puts "shift   #{racc_token2str tok}"
      racc_print_stacks tstack, vstack
      @racc_debug_out.puts
    end

    def racc_reduce(toks, sim, tstack, vstack)
      out = @racc_debug_out
      out.print 'reduce '
      if toks.empty?
        out.print ' <none>'
      else
        toks.each {|t| out.print ' ', racc_token2str(t) }
      end
      out.puts " --> #{racc_token2str(sim)}"

      racc_print_stacks tstack, vstack
      @racc_debug_out.puts
    end

    def racc_accept
      @racc_debug_out.puts 'accept'
      @racc_debug_out.puts
    end

    def racc_e_pop(state, tstack, vstack)
      @racc_debug_out.puts 'error recovering mode: pop token'
      racc_print_states state
      racc_print_stacks tstack, vstack
      @racc_debug_out.puts
    end

    def racc_next_state(curstate, state)
      @racc_debug_out.puts  "goto    #{curstate}"
      racc_print_states state
      @racc_debug_out.puts
    end

    def racc_print_stacks(t, v)
      out = @racc_debug_out
      out.print '        ['
      t.each_index do |i|
        out.print ' (', racc_token2str(t[i]), ' ', v[i].inspect, ')'
      end
      out.puts ' ]'
    end

    def racc_print_states(s)
      out = @racc_debug_out
      out.print '        ['
      s.each {|st| out.print ' ', st }
      out.puts ' ]'
    end

    def racc_token2str(tok)
      # self.class::Racc_token_to_s_table[tok] or raise "[Racc Bug] can't convert token #{tok} to string"
      RubyParser::Racc_token_to_s_table[tok] or raise "[Racc Bug] can't convert token #{tok} to string"
    end

    def token_to_str(t)
      #   used for error messages
      # self.class::Racc_token_to_s_table[t]
      if t.equal?(nil)
        "nil"
      else
        RubyParser::Racc_token_to_s_table[t]
      end
    end

  end

end # }
