=begin
    mo.rb - A simple class for operating GNU MO file.

    Copyright (C) 2003-2006  Masao Mutoh
    Copyright (C) 2002  Masahiro Sakai, Masao Mutoh
    Copyright (C) 2001  Masahiro Sakai

        Masahiro Sakai                  <s01397ms@sfc.keio.ac.jp>
        Masao Mutoh                     <mutoh@highway.ne.jp>

    You can redistribute this file and/or modify it under the same term
    of Ruby.  License of Ruby is included with Ruby distribution in
    the file "README".

    $Id: mo.rb,v 1.7 2006/06/11 15:36:20 mutoh Exp $
=end

require 'iconv'

class MOFile < Hash
  class InvalidFormat < RuntimeError; end;

  attr_reader :filename

  Header = Struct.new(:magic,
                      :revision,
                      :nstrings,
                      :orig_table_offset,
                      :translated_table_offset,
                      :hash_table_size,
                      :hash_table_offset)

  MAGIC_BIG_ENDIAN    = "\x95\x04\x12\xde"
  MAGIC_LITTLE_ENDIAN = "\xde\x12\x04\x95"

  def self.open(arg = nil, output_charset = nil)
    result = self.new(output_charset)
    result.load(arg)
  end

  def initialize(output_charset = nil)
    @filename = nil
    @last_modified = nil
    @little_endian = true
    @output_charset = output_charset
    super
  end

  def update!
    if FileTest.exist?(@filename)
      st = File.stat(@filename)
      load(@filename) unless (@last_modified == [st.ctime, st.mtime])
    else
      puts "#{@filename} was lost." if $DEBUG
      clear
    end
    self
  end

  def load(arg)
    case arg
    when String
      begin
	      st = File.stat(arg)
	      @last_modified = [st.ctime, st.mtime]
      rescue Exception
      end
      load_from_file(arg)
    when IO
      load_from_stream(arg)
    end
    @filename = arg
    self
  end

  def load_from_stream(io)
    magic = io.read(4)
    case magic
    when MAGIC_BIG_ENDIAN
      @little_endian = false
    when MAGIC_LITTLE_ENDIAN
      @little_endian = true
    else
      raise InvalidFormat.new("Unknown signature %s" % magic.dump)
    end

    header = Header.new(magic, *(io.read(4 * 6).unpack(@little_endian ? 'V6' : 'N6')))
    raise InvalidFormat.new(sprintf("file format revision %d isn't supported", header.revision)) if header.revision > 0

    io.pos = header.orig_table_offset
    orig_table_data = io.read((4 * 2) * header.nstrings).unpack(@little_endian ? 'V*' : 'N*')

    io.pos = header.translated_table_offset
    trans_table_data = io.read((4 * 2) * header.nstrings).unpack(@little_endian ? 'V*' : 'N*')

    original_strings = Array.new(header.nstrings)
    for i in 0...header.nstrings
      io.pos = orig_table_data[i * 2 + 1]
      original_strings[i] = io.read(orig_table_data[i * 2 + 0])
    end

    clear
    for i in 0...header.nstrings
      io.pos = trans_table_data[i * 2 + 1]
      str = io.read(trans_table_data[i * 2 + 0])

      if original_strings[i] == ""
        if str
          @charset = nil
          @nplurals = nil
          @plural = nil
          str.each_line{|line|
            if /^Content-Type:/i =~ line and /charset=((?:\w|-)+)/i =~ line
              @charset = $1
            elsif /^Plural-Forms:\s*nplurals\s*\=\s*(\d*);\s*plural\s*\=\s*([^;]*)\n?/ =~ line
              @nplurals = $1
              @plural = $2
            end
            break if @charset and @nplurals
          }
          @nplurals = "1" unless @nplurals
          @plural = "0" unless @plural
        end
      else
        if @output_charset
          begin
            str = Iconv.iconv(@output_charset, @charset, str).join if @charset
          rescue Iconv::Failure
            if $DEBUG
              $stderr.print "@charset = ", @charset, "\n"
              $stderr.print "@output_charset = ", @output_charset, "\n"
              $stderr.print "msgid = ", original_strings[i], "\n"
              $stderr.print "msgstr = ", str, "\n"
            end
          end
        end
      end
      self[original_strings[i]] = str
    end
    self
  end

  def load_from_file(filename)
    @filename = filename
    File.open(filename, 'rb'){|f| load_from_stream(f)}
  end

  def set_comment(msgid_or_sym, comment)
    #Do nothing
  end


  attr_accessor :little_endian, :path, :last_modified
  attr_reader :charset, :nplurals, :plural
end
