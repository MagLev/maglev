
module MagRp

  def self.debug
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
      @val = str.to_sym
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
    end

    def last_closed_def_offset
      @extend_ofs_last_unextend
    end

    def scanner=( scanner )
      @src_scanner = scanner
    end
  
    # end of first opening,  all constants and fixed instVars defined
  end # }
  Environment._freeze_constants

  class RubyLexer
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

    ESC_RE = /\\([0-7]{1,3}|x[0-9a-fA-F]{1,2}|M-.|(C-|c)\?|(C-|c).|[^0-7xMCc])/

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

  end
  RubyLexer._freeze_constants

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

