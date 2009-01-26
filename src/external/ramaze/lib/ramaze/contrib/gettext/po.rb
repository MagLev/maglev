#! /usr/bin/env ruby
=begin
  rgettext.rb - Generate a .pot file.

  Copyright (C) 2003-2006  Masao Mutoh
  Copyright (C) 2001,2002  Yasushi Shoji, Masao Mutoh

      Yasushi Shoji   <yashi at atmark-techno.com>
      Masao Mutoh     <mutoh at highway.ne.jp>

  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end


module GetText

  module RGetText #:nodoc:
    extend GetText

    MAX_LINE_LEN = 70 unless defined?(MAX_LINE_LEN)

    module_function

    def generate_pot_header # :nodoc:
      time = Time.now.strftime("%Y-%m-%d %H:%M")
      off = Time.now.utc_offset
      sign = off <= 0 ? '-' : '+'
      time += sprintf('%s%02d%02d', sign, *(off.abs / 60).divmod(60))

      <<EOS
# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER
# This file is distributed under the same license as the PACKAGE package.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid ""
msgstr ""
"Project-Id-Version: PACKAGE VERSION\\n"
"POT-Creation-Date: #{time}\\n"
"PO-Revision-Date: #{time}\\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n"
"Language-Team: LANGUAGE <LL@li.org>\\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=UTF-8\\n"
"Content-Transfer-Encoding: 8bit\\n"
EOS
    end

    def generate_pot(ary) # :nodoc:
      str = ""
      result = Array.new
      ary.each do |key|
        msgid = key.shift.dup
        curr_pos = MAX_LINE_LEN
        key.each do |e|
          if curr_pos + e.size > MAX_LINE_LEN
            str << "\n#:"
            curr_pos = 3
          else
            curr_pos += (e.size + 1)
          end
          str << " " << e
        end
        msgid.gsub!(/"/, '\"')
        msgid.gsub!(/\r/, '')

        str << "\nmsgid \"" << msgid << "\"\n"
        str << "msgstr \"\"\n"
      end
      str
    end

    def generate_translated_po(hash)
      str = generate_pot_header
      result = Array.new

      hash.keys.sort.each do |msgid|
        msgid = msgid.dup
        msgstr = hash[msgid]

        msgid.gsub!(/"/, '\"')
        msgid.gsub!(/\r/, '')

        if msgstr
          msgstr.gsub!(/"/, '\"')
          msgstr.gsub!(/\r/, '')
        end

        str << "\nmsgid \"" << msgid << "\"\n"
        str << "msgstr \"" << msgstr << "\"\n"
      end

      str
    end

    def generate(array)  # :nodoc:
      str = ''
      str << generate_pot_header
      str << generate_pot(array)
    end
  end
end

if $0 == __FILE__
  require 'yaml'
  puts GetText::RGetText.generate_translated_po(YAML.load_file(ARGV[0]))
end
