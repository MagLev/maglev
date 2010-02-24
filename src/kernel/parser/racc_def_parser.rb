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
# 
# copied from lib and code moved to module MagRp for the internal parser

module MagRp
  class ParseError < StandardError; end
end

module MagRp # {

  # deleted Racc_No_Extentions, not used

  class Parser # [

    PrintRaccArrayInfo = false

    Racc_Runtime_Version = '1.4.5'
    Racc_Runtime_Revision = '$Revision: 1.7 $'.split[1]

    Racc_Runtime_Core_Version_R = '1.4.5'
    Racc_Runtime_Core_Revision_R = '$Revision: 1.7 $'.split[1]
    Racc_Main_Parsing_Routine    = :_racc_do_parse_rb
    Racc_YY_Parse_Method         = :_racc_yyparse_rb
    Racc_Runtime_Core_Version    = Racc_Runtime_Core_Version_R
    Racc_Runtime_Core_Revision   = Racc_Runtime_Core_Revision_R
    Racc_Runtime_Type            = 'ruby'

    def Parser.racc_runtime_type
      Racc_Runtime_Type
    end

    def _bind_instvars
      # defines the fixed instVars
      # ensures instvars are ram_oops , returns self
      # Used to initialze a transient copy of MagTemplate
      #
      # MagRpDEBUG values from maglev-ruby script or smalltalk main program:
      #   0 no tracing
      #   1  trace files parsed by RubyParser
      #   2  include lexer and racc state machine tracing (requires
      #        parser to be generated with racc.sh -D before loading prims)
      # MAGLEV_parseWarn is either true or false, from RubyArgs>>_parseRubyArgs:
      system_cls = Maglev::__system 
      debug_level = system_cls.session_temp(:MagRpDEBUG)
      if debug_level._equal?(nil)
        debug_level = 0
      end
      @debuglevel = debug_level
      @mydebug = debug_level > 1
      @rpwarnings = system_cls.session_temp(:MAGLEV_parseWarn)
      @save_warnings = nil  

      rt = @reduce_table.dup  # size about 1497
      len = rt.size
      idx = 0
      while idx < len
        ofs = idx + 2
        rt[ofs] = rt[ofs] # fault in symbols
        idx += 3
      end
      rt.__set_nostubbing
      rt.freeze
      @reduce_table = rt

      _validate_goto_tables

      # fault-in all of the following and convert some to TransientShortArray,
      #  for faster access
      @racc_debug_out = @racc_debug_out
      @action_table = TransientShortArray._with_shorts(@action_table).freeze # size about 23880
      @action_check = TransientShortArray._with_shorts(@action_check).freeze #  same size as action_table
      @action_default = @action_default  # size about 896
      @action_pointer = @action_pointer  #   same size as action_default
      @goto_table = TransientShortArray._with_shorts(@goto_table).freeze # size about 2302
      @goto_check = TransientShortArray._with_shorts(@goto_check).freeze #  same size as goto_table
      @goto_default = @goto_default # size about 141
      @goto_pointer = @goto_pointer #   same size as  goto_default

      @token_table = @token_table
      @lexer = nil
      @env = nil 
      @syntax_err_count = 0
      @save_last_len = nil
      @counts_array = nil
      _init_counts_array
      self.__set_nostubbing  # prevent stubbing of refs to the faulted-in objects
      self
    end

    def _validate_goto_tables
      goto_table = @goto_table
      goto_check = @goto_check.dup
      @goto_check = goto_check  
      goto_pointer = @goto_pointer
      lim = goto_table.size
      unless lim == goto_check.size
        raise 'inconsistent goto table sizes'
      end
      # ensure that nils in @goto_table and @goto_check match.
      #  remember that the TransientShortArrays represent nils as 0
      #  and the primitive _rubyParserShortAt:   translates 0 to nil on read
      n = 0
      while n < lim
        chk = goto_check[n]
        unless (goto_table[n]._equal?(nil))._equal?( chk._equal?(nil) )
          raise 'mismatch on nils in goto tables' 
        end
        n += 1
      end 
      n = 0
      reduce_table = @reduce_table
      n = 0
      lim = reduce_table.size
      while n < lim
        val = reduce_table[n+1] - Racc_nt_base
        if val >= 0x7FFF
          raise "reduce_table.#{n} - Racc_nt_base exceeds 0x7FFF "
        end
        n += 3
      end
    end

    def _racc_init_sysvars
      # initialize the instance variables which change during parsing
      @racc_state  = [0]
      @racc_tstack = []
      @racc_vstack = []

      @racc_t = nil
      @racc_val = nil

      @racc_read_next = true

      @racc_user_yyerror = false
      @racc_error_status = 0
    end

    def env
      @env
    end

    # remaining code is in files racc_init_parser.rb and racc_parser.rb 

  end # ]

  class RubyParser < Parser  # [

    VERSION = '2.0.2'

    def _bind_instvars
      # Used to initialze a transient copy of MagTemplate, returns receiver
      @in_single = 0
      @in_def = false
      super
    end

    def _install_wordlist(hash)
      @lexer_wordlist = hash
    end

    def _wordlist
      @lexer_wordlist
    end

    def file_name
      @file_name
    end
    def source_string
      @source_string
    end

    InvalidAssignableLhs = IdentitySet.with_all(
       [ :self , :nil , :true , :false , :"__LINE__" , :"__FILE__" ] )

    # replicate some constants from Regexp so they can be
    # resolved at boot compile time in extras.rb
    IGNORECASE = Regexp::IGNORECASE
    MULTILINE = Regexp::MULTILINE
    EXTENDED = Regexp::EXTENDED
    #  defined?  not supported during boostrap
    #  assume Regexp::ONCE not defined in 1.8.6
    #if defined?( Regexp::ONCE)
    #  ONCE = Regexp::ONCE
    #  ENC_NONE = Regexp::ENC_NONE
    #  ENC_EUC = Regexp::ENC_EUC
    #  ENC_SJIS = Regexp::ENC_SJIS
    #  ENC_UTF8 = Regexp::ENC_UTF8
    #else
      ONCE     = 0 # 16 # ?
      ENC_NONE = /x/n.options
      ENC_EUC  = /x/e.options
      ENC_SJIS = /x/s.options
      ENC_UTF8 = /x/u.options
    #end

    # replicate some constants from the lexer so they can be
    #  bound at boot compile time in ruby_parser.rb
    Expr_beg = RubyLexer::Expr_beg
    Expr_end = RubyLexer::Expr_end
    Expr_fname = RubyLexer::Expr_fname
    Expr_endArg = RubyLexer::Expr_endArg

  end # ]
  RubyParser.__freeze_constants

  class Keyword # [
    def self.create_transient_wordlist
      # create a StringKeyValueDictionary from WORDLIST, 
      # The dictionary, which is not committed,
      #  will have memory pointer references to all keys and values
      # Use a StringKeyValueDictionary instead of a Hash
      #  because it has a C primitive for at() .
      list = WORDLIST
      h = StringKeyValueDictionary.__new( list.size )
      list.each { | arr |
	k = arr[0]
	v = arr[1].dup
	v_siz = v.size
	for i in 0..v_siz-1 do
	  v[i] = v[i] # make ref a RamOop
	end
	h.at_put(k, v)
      }
      h.freeze
      h
    end
  end # ]

end # }

