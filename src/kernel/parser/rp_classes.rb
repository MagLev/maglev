
module MagRp

  def self.debug
    # MagRpDEBUG values are
    #   0 no tracing
    #   1  trace files parsed by RubyParser
    #   2  also include lexer and racc state machine tracing
    d = Gemstone.session_temp(:MagRpDEBUG)
    if d.equal?(nil)
      d = 0
    end
    d
  end

  class ScanError < StandardError; end

  class RaccJumpError < StandardError; end

  # deleted  Sexp, SexpMatchSpecial, SexpAny

  class InternalParseError < StandardError ; end

  class DefnNameToken < RpNameToken
    #  RpNameToken is a smalltalk class
    def line
      @line_num  # instVar defined by DefnNameToken,
      # all other instVars inherited from RpNameToken
    end 
    def self.new(str, ofs, line)
      o = self.allocate
      o.initialize(str, ofs, line)
    end
    def initialize(str, ofs, line)
      @val = str._as_symbol
      @line_num = line
      @src_offset = ofs
      self
    end
    def inspect
      "(DefnNameToken #{@val}  @#{@src_offset} line:#{@line_num})"
    end
  end

  class Environment # {
    # reworked from Ryans' code to reverse order of arrays, so that extend
    # can add to the end of the arrays, and unextend decrements curridx .
    # reworked to have a single array of triples to reduce
    #  instVar references

    #  env is @arr[@curridx] , dyn is @arr[@curridx+1], use is @arr[@curridx+2]

    # OFF_env = 0
    OFF_dyn = 1   
    OFF_use = 2
    OFF_byte_offset = 3  # approximate location of  def , class, module
    ENTRY_SIZE = 4

    def initialize
      @src_scanner = nil
      @arr = []
      @curridx = -ENTRY_SIZE
      @extend_ofs_last_unextend = -1
      @first_top_level_def_offset = -1
      @module_count = 0
    end

    def last_closed_def_offset
      @extend_ofs_last_unextend
    end

    def first_top_level_def_offset
      @first_top_level_def_offset
    end

    def scanner=( scanner )
      @src_scanner = scanner
    end
  
    # end of first opening,  all constants and fixed instVars defined
  end # }
  Environment._freeze_constants

  class RubyLexer # {
    def command_start_
      @command_start
    end
    def cmdarg_
      @cmdarg
    end
    def cmdarg=( v )
      @cmdarg = v
    end
    def cond_
      @cond
    end
    def nest_
      @nest
    end
    def lineno_
      @line_num
    end

    def src_regions_
      @src_regions
    end

    # ESC_RE = /\\([0-7]{1,3}|x[0-9a-fA-F]{1,2}|M-.|(C-|c)\?|(C-|c).|[^0-7xMCc])/
    # ESC_RE is no longer used (after fix for Trac 580)

    CHAR_LIT_VT_WHITE_ERRS = {
                    " " => 's',
                    "\n" => 'n',
                    "\t" => 't',
                    "\v" => 'v',
                    "\r" => 'r',
                    "\f" => 'f' }

    UNESCAPE_TABLE = {
      "a"    => "\007",
      "b"    => "\010",
      "e"    => "\033",
      "f"    => "\f",
      "n"    => "\n",
      "r"    => "\r",
      "s"    => " ",
      "t"    => "\t",
      "v"    => "\13",
      "\\"   => '\\',
      "\n"   => "",
      "C-\?" => 0177.chr,
      "c\?"  => 0177.chr }

    # Additional context surrounding tokens that both the lexer and
    # grammar use.
    def lex_state_
      @lex_state
    end

    def lex_strterm_
      @lex_strterm
    end
    def lex_strterm=(v)
      if v._not_equal?(nil)           # TODO remove consistency check
        sym = v[0]
        unless sym.equal?( :strterm ) || sym.equal?( :heredoc )
          raise 'invalid arg to lex_strterm='
        end
      end
      @lex_strterm = v
    end

    def parser_ # HACK for very end of lexer... *sigh*
      @parser
    end
    def parser=(p)
      @parser = p
    end
   
    def string_buffer_
      @string_buffer
    end

    def mydebug_
      @mydebug
    end

    # Stream of data that yylex examines.
    #  attr_reader :src
    def the_scanner
      @src_scanner
    end
 
    def source_string
      @src_scanner.string
    end

    # Value of last token which had a value associated with it.
    def yacc_value_
      @yacc_value
    end

    def line_num_
      # maintained by yylex and parsing of string constants
      @line_num
    end

    def _keyword_table
      @keyword_table  # a Hash , replaces Keyword.keyword
    end

    def last_else_src_offset
      @last_else_src_offset
    end

    EOF = :eof_haha!
    
    # ruby constants for strings (should this be moved somewhere else?)
    STR_FUNC_BORING = 0x00 
    STR_FUNC_ESCAPE = 0x01 # TODO202: remove and replace with REGEXP
    STR_FUNC_EXPAND = 0x02
    STR_FUNC_REGEXP = 0x04
    STR_FUNC_AWORDS = 0x08
    STR_FUNC_SYMBOL = 0x10 
    STR_FUNC_INDENT = 0x20 # <<-HEREDOC
    
    STR_SQUOTE = STR_FUNC_BORING 
    STR_DQUOTE = STR_FUNC_BORING | STR_FUNC_EXPAND
    STR_XQUOTE = STR_FUNC_BORING | STR_FUNC_EXPAND 
    STR_REGEXP = STR_FUNC_REGEXP | STR_FUNC_ESCAPE | STR_FUNC_EXPAND
    STR_SSYM   = STR_FUNC_SYMBOL 
    STR_DSYM   = STR_FUNC_SYMBOL | STR_FUNC_EXPAND

    def self.build_strterm(arr)
      arr << Regexp.new(Regexp.escape(arr[2]))
      arr << "\0"
      arr << /\000/ 
      arr
    end

    STRTERM_DQUOTE = build_strterm( [ :strterm,  STR_DQUOTE, '"' ] )
    STRTERM_SSYM =   build_strterm( [ :strterm, STR_SSYM, "'" ] )
    STRTERM_DSYM =   build_strterm( [ :strterm, STR_DSYM, '"' ] )
    STRTERM_XQUOTE = build_strterm( [ :strterm, STR_XQUOTE, '`'] )
    STRTERM_REGEXP = build_strterm( [ :strterm, STR_REGEXP, '/'] )

    # define lexer states as bits in a Fixnum for more efficient testing
    #   of  one of several states  
    Expr_beg =     0x1	# :expr_beg    = ignore newline, +/- is a sign.
    Expr_end =     0x2 	# :expr_end    = newline significant, +/- is a operator.
    Expr_arg =     0x4	# :expr_arg    = newline significant, +/- is a operator.
    Expr_cmdArg =  0x8	# :expr_cmdarg = newline significant, +/- is a operator.
    Expr_endArg = 0x10	# :expr_endarg = newline significant, +/- is a operator.
    Expr_mid =    0x20	# :expr_mid    = newline significant, +/- is a operator.
    Expr_fname =  0x40	# :expr_fname  = ignore newline, no reserved words.
    Expr_dot =    0x80	# :expr_dot    = right after . or ::, no reserved words.
    Expr_class = 0x100	# :expr_class  = immediate after class, no here document.

    Expr_IS_argument = Expr_arg | Expr_cmdArg
    Expr_IS_fname_dot = Expr_fname | Expr_dot
    Expr_IS_beg_mid = Expr_beg | Expr_mid
    Expr_IS_beg_fname_dot_class = Expr_beg | Expr_fname | Expr_dot | Expr_class 

    Expr_IS_beg_mid_class = Expr_beg | Expr_mid | Expr_class
    Expr_IS_end_endarg = Expr_end | Expr_endArg 

    Expr_IS_argument_end = Expr_IS_argument | Expr_end 

    Expr_IS_end_dot_endarg_class = Expr_end | Expr_dot | Expr_endArg | Expr_class

    Expr_IS_beg_mid_dot_arg_cmdarg = Expr_beg | Expr_mid | Expr_dot | Expr_arg | Expr_cmdArg

  end # }
  RubyLexer._freeze_constants

  class Keyword # {
#     class KWtable		# class no longer used
#	attr_accessor :name, :state, :id0, :id1 
#	def initialize(name, id=[], state=nil)
#	  @name  = name
#	  @id0, @id1 = id
#	  @state = state
#	end
#      end

      ##
      # :stopdoc:
      #
      # lexer states changed to Fixnums, see rp_classes.rb 
      #  Expr_beg    = ignore newline, +/- is a sign.
      #  Expr_end    = newline significant, +/- is a operator.
      #  Expr_arg    = newline significant, +/- is a operator.
      #  Expr_cmdarg = newline significant, +/- is a operator.
      #  Expr_endarg = newline significant, +/- is a operator.
      #  Expr_mid    = newline significant, +/- is a operator.
      #  Expr_fname  = ignore newline, no reserved words.
      #  Expr_dot    = right after . or ::, no reserved words.
      #  Expr_class  = immediate after class, no here document.

      wordlist = [
		    # negated new state means yacc_value gets encapsulated in an RpNameToken
		    # and/or gets other special handling
		  ["end",      [:kEND,      :kEND        , RubyLexer::Expr_end   ]],
		  ["else",     [:kELSE,     :kELSE       , RubyLexer::Expr_beg   ]],
		  ["case",     [:kCASE,     :kCASE       , - RubyLexer::Expr_beg   ]],
		  ["ensure",   [:kENSURE,   :kENSURE     , RubyLexer::Expr_beg   ]],
		  ["module",   [:kMODULE,   :kMODULE     , RubyLexer::Expr_beg   ]],
		  ["elsif",    [:kELSIF,    :kELSIF      , RubyLexer::Expr_beg   ]],
		  ["def",      [:kDEF,      :kDEF        , - RubyLexer::Expr_fname ]],  
		  ["rescue",   [:kRESCUE,   :kRESCUE_MOD , - RubyLexer::Expr_mid   ]],  
		  ["not",      [:kNOT,      :kNOT        , RubyLexer::Expr_beg   ]],
		  ["then",     [:kTHEN,     :kTHEN       , RubyLexer::Expr_beg   ]],
		  ["yield",    [:kYIELD,    :kYIELD      , - RubyLexer::Expr_arg   ]],
		  ["for",      [:kFOR,      :kFOR        , RubyLexer::Expr_beg   ]],
		  ["self",     [:kSELF,     :kSELF       , RubyLexer::Expr_end   ]],
		  ["false",    [:kFALSE,    :kFALSE      , RubyLexer::Expr_end   ]],
		  ["retry",    [:kRETRY,    :kRETRY      , - RubyLexer::Expr_end   ]],
		  ["return",   [:kRETURN,   :kRETURN     , - RubyLexer::Expr_mid   ]],
		  ["true",     [:kTRUE,     :kTRUE       , RubyLexer::Expr_end   ]],
		  ["if",       [:kIF,       :kIF_MOD     , - RubyLexer::Expr_beg   ]], 
		  ["defined?", [:kDEFINED,  :kDEFINED    , RubyLexer::Expr_arg   ]],
		  ["super",    [:kSUPER,    :kSUPER      , - RubyLexer::Expr_arg   ]], 
		  ["undef",    [:kUNDEF,    :kUNDEF      , - RubyLexer::Expr_fname ]],
		  ["break",    [:kBREAK,    :kBREAK      , - RubyLexer::Expr_mid   ]],
		  ["in",       [:kIN,       :kIN         , RubyLexer::Expr_beg   ]],
		  ["do",       [:kDO,       :kDO         , - RubyLexer::Expr_beg   ]], 
		  ["nil",      [:kNIL,      :kNIL        , RubyLexer::Expr_end   ]],
		  ["until",    [:kUNTIL,    :kUNTIL_MOD  , - RubyLexer::Expr_beg   ]], 
		  ["unless",   [:kUNLESS,   :kUNLESS_MOD , - RubyLexer::Expr_beg   ]], 
		  ["or",       [:kOR,       :kOR         , RubyLexer::Expr_beg   ]],
		  ["next",     [:kNEXT,     :kNEXT       , - RubyLexer::Expr_mid   ]],
		  ["when",     [:kWHEN,     :kWHEN       , - RubyLexer::Expr_beg   ]],
		  ["redo",     [:kREDO,     :kREDO       , - RubyLexer::Expr_end   ]],
		  ["and",      [:kAND,      :kAND        , RubyLexer::Expr_beg   ]],
		  ["begin",    [:kBEGIN,    :kBEGIN      , RubyLexer::Expr_beg   ]],
		  ["__LINE__", [:k__LINE__, :k__LINE__   , RubyLexer::Expr_end   ]],
		  ["class",    [:kCLASS,    :kCLASS      , - RubyLexer::Expr_class ]],
		  ["__FILE__", [:k__FILE__, :k__FILE__   , RubyLexer::Expr_end   ]],
		  ["END",      [:klEND,     :klEND       , RubyLexer::Expr_end   ]],
		  ["BEGIN",    [:klBEGIN,   :klBEGIN     , RubyLexer::Expr_end   ]],
		  ["while",    [:kWHILE,    :kWHILE_MOD  , - RubyLexer::Expr_beg   ]], 
		  ["alias",    [:kALIAS,    :kALIAS      , - RubyLexer::Expr_fname ]]
		 ]

      # :startdoc:

      ht = Hash.new
      WORDLIST = ht
      wordlist.each { | elem |
        ht[ elem[0] ] = elem[1]
      } 
 
      # def self.keyword( str)  ; end # no longer used

  end # }
  Keyword._freeze_constants

  class SrcRegion
    # describes a portion of the source string, used in heredoc implementation
    def initialize(lnum, ofs, lim)
      @line_num = lnum
      @offset = ofs
      @limit  = lim
    end
    def line_num
      @line_num
    end
    def offset
      @offset
    end
    def limit
      @limit
    end
  end 

end

