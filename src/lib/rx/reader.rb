require 'rx/null_api'

module RX
 class Reader
    def initialize(doc, api = NullAPI.new)
      here = "/opt/gemstone/maglev/src/rx"
      @api = api
      @input = Input.new doc
      @encoding = @input.encoding
      @state = 1
      @well_formed = true
      @error = ''
      @last_lt_offset = -1
      @state_stack = []
      @saving = false
      @buf = []
      @in_buf = []
      @in_index = 0
      @declared_encoding = nil
      @saved_names = []
      @pushed_events = []
      @standalone_value = 'no'
      @element_stack = []
      @empty = false
      @text = ''

      # debugging crap
      #define_state_names

      # init class variables 
#      if @@machine == nil
        @@machine = File.read "#{here}/machine"
        @@actions = File.read "#{here}/actions"
        CharClass.load_char_classes
        load_actions
#      end
    end
  end
end

require 'rx/input'
require 'rx/char_classes'
require 'rx/support'
require 'rx/qname'

module RX

  attr_reader :encoding, :error
  
  class Reader
     @@amp = 38
     @@machine = nil

    def namespace_binding(prefix)
      @element_stack.reverse_each do |element|
        ns = element.namespaces[prefix]
        return ns if ns
      end
      nil
    end

    def one_char
      if @in_index == @in_buf.length
        @in_buf = @input.next_chars
        @in_index = 0
        return nil if @in_buf == nil
      end

      c = @in_buf[@in_index]
      @in_index += 1
      c
    end

    def go
      while true do
        if @pushed_events.empty?
          action = crank
        else
          action = @pushed_events.shift
        end

        case action
        when :stag
          e = Element.new(@saved_names, self)
          @element_stack.push e
          @saved_names.clear
          r = @api.stag e
          return r if r
          
        when :etag
          stag = @element_stack.pop
          if @empty
            @empty = false
          else
            q = QName.from_bytes(@saved_names[0], self)
            raise(SyntaxError, q) unless q.kind_of? QName
            if q != stag.type
              raise(SyntaxError,
                    "End-tag #{q} does not match start-tag #{stag.type}")
            end
          end
          r = @api.etag stag
          @saved_names.clear
          @pushed_events << :end if @element_stack.empty?
          return r if r
          
        when :text
          r = @api.text @text
          @buf.clear
          return r if r

        when :doctype
          # later, dude

        when :pi
          r = @api.pi @saved_names
          @saved_names.clear
          return r if r
          
        when :xml_decl
        when :subset_exists
        when :end
          r = @api.end
          return r if r
        end
      end
    end

    # crank the machine a few times until until it comes back with a
    #  non-false action, which indicates something ought to be reported
    #
    def crank
      action = false
      while !action

        if @in_index == @in_buf.length
          @in_buf = @input.next_chars
          @in_index = 0
          # puts "NC: #{@in_buf[0 .. 5].inspect}"
          return :end unless @in_buf
        end

        # constraint: if next_chars returns a buffer of length
        #  greater than 1, it will never contain < or &
        #
        if @state == @@state_InDoc && @in_buf.length > 1
          @buf = @buf + @in_buf[@in_index .. -1]
          @in_index = @in_buf.length
          @text = @buf.ustr
          return :text unless @buf.empty?
        end

        c = @in_buf[@in_index]
        @in_index += 1
        
        # b is the DFA index
        b = c

        # but the DFA only works for chars < 128
        if c > 127

          # do any character-class transitions apply?
          class_trans = @@name_class_transitions[@state]
          if class_trans == 0x2a # '*'
            
            # none apply.  If it's an illegal character, signal with 0
            if (c > 0xd7ff && c < 0xe000) ||
                (c == 0xfffe) || (c == 0xffff) || (c > 0x10ffff)
              b = 0

            else
              # we'll pretend we saw '~', which only maches the '.' class
              b = 0x7e # '~'
            end

          elsif (class_trans == 0x63) && CharClass.is_in(c, 'NameChar')
            # if name-character transition is possible in this state
            #  ('c' == 0x63), and the character we saw is a name-char
            #  pretend we saw a '1'
            b = 0x31 # '1'

          elsif (class_trans == 0x73) && CharClass.is_in(c, 'NameStart')
            # if name-start transition is possible in this state
            #  ('s' == 0x73), and the character we saw is a name-start,
            #  pretend we saw an 'a'
            b = 0x61

          else
            # whatever we saw was illegal
            b = 0
          end
        end

        # on a HotStart, c will go in the buffer and @saving gets
        #  turned on.  On a ColdStart, @saving gets turned on.  On an
        #  endSave, @saving gets turned off.  Putting this here means
        #  that the HotStart action has to stuff the character and
        #  EndSave has to pop it back out.  We could put this after
        #  the action call, but then it's hard to see how to
        #  handle ColdStart properly
        #
        @buf << c if @saving

        index = (@state * 128) + b
        @to = nil
        action = do_action(@@actions[index], c) if @@actions[index] != 0

        # turn crank unless action already set the next state
        @to = @@machine[index] unless @to

        # debug(c, index)

        @state = @to
      end
      return action
    end

    def debug(c, index)
      print "#{c} (#{[c].ustr}) => #{@to} (#{@@state_names[@to]})"
      if @@actions[index] != 0
        print " action=#{@@actions[index]}"
      end
      print "\n"
    end
    
    def ill_formed(message)
      @error = message
      @well_formed = false
    end

    def last_string
      @buf.ustr
    end

    def stop_saving
      @buf.pop
      @saving = false
    end

    def start_saving
      @saving = true
      @buf = []
    end

    ################################################################
    # Actions from automaton
    # They return a symbol if something should be reported to caller
    #
    def a_CharRef(c, to_add)
      # the &... whatever is sitting in the buffer
      (@buf.length - @buf.rindex(@@amp)).times { @buf.pop }
      @buf << to_add
      false
    end
    
    def a_ColdStart(c)
      start_saving
      false
    end
    
    def a_EndAttrVal(c)
      @saved_names << @buf
      stop_saving
      false
    end

    # really EndAttrName
    def a_EndAttribute(c)
      @saved_names << @buf
      stop_saving
      false
    end
    
    def a_EndGI(c)
      @saved_names << @buf
      stop_saving
      false
    end
    
    def a_EndSave(c)
      @saved_names << @buf
      stop_saving
      false
    end
    
    def a_EntityReference(c)
      # the &... whatever is sitting in the buffer
      amp_at = @buf.rindex(@@amp)
      ref = @buf[amp_at .. -1].ustr
      (@buf.length - amp_at).times { @buf.pop }
      ref = $1 if ref =~ /&(.*)$/
      val = nil
      val = @entities[ref] if @entities
      if val
        @buf = @buf + val.explode
      else
        # XXX have to check if it's OK to miss an entity
      end
      false
    end
    
    def a_FloatingMSE(c)
      complain(:floatingMSE)
      false
    end

    def a_GotEncoding(c)
      @declared_encoding = last_string
      false
    end
    
    def a_GotXMLD(c)
      @input.encoding = @declared_encoding

      # if there's something in the buffer, it can only be the
      #  value of the standalone declaration
      if !@buf.empty?
        @standalone_value = last_string
      end
    end
    
    def a_HashRef(c)
      # the &... whatever is sitting in the buffer
      amp_at = @buf.rindex(@@amp)
      ref = @buf[amp_at .. -1].ustr
      (@buf.length - amp_at).times { @buf.pop }
      if ref =~ /&.x(.*)$/
        @buf << $1.hex
      else
        @buf << $1.to_i
      end
      false
    end
    
    def a_HotStart(c)
      start_saving
      @buf << c
      false
    end
    
    def a_KW(c, word, after, offset)
      # this doesn't happen too often, just walk through it
      offset.upto(word.length - 1) do |i|
        c = one_char
        if c != word[i]
          ill_formed("Failed to match '#{word}")
          return true
        end
        @buf << c if @saving
      end
      @to = after
      false
    end
    
    def a_MarkLt(c)
      @last_lt_offset = @input.chars - 1
      false
    end
    
    def a_Pop(c)
      @to = @state_stack.pop
      false
    end
    
    def a_Push(c, to)
      @state_stack.push to
      false
    end
    
    def a_ReportDoctype(c)
      :doctype
    end
    
    def a_ReportETag(c)
      start_saving
      :etag
    end
    
    def a_ReportEmpty(c)
      @pushed_events << :etag
      @empty = true
      start_saving
      :stag
    end
    
    def a_ReportPI(c)
      start_saving unless @element_stack.empty?
      :pi
    end
    
    def a_ReportSTag(c)
      @in_doc = true
      start_saving
      :stag
    end
    
    def a_ReportText(c)
      stop_saving
      @text = @buf.ustr
      :text
    end
    
    def a_SaveExtra(c, s)
      @buf = @buf + s.explode
      false
    end

    def a_StuffChar(c)
      @input.save(c)
    end
    
    def a_SubsetExists(c)
      :subset_exists
    end


  end



  class Element

    attr_reader :strings, :namespaces

    # to-do; figure out what kind of accessor methods are interesting
    def initialize strings, reader
      @namespaces = { }
      t = strings[0].ustr
      @strings = QName.scan(strings, @namespaces, reader)
      unless @strings.kind_of? Array
        raise(SyntaxError, @strings)
      end
    end

    def type
      @strings[0]
    end

  end

end

class String
  def explode
    self.unpack('U*')
  end
end

class Array
  def ustr
    self.pack('U*')
  end
end
