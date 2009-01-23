require 'rx/reader'

module RX

  class QName
    @@colon = 0x3a
    attr_reader :namespace, :local_part, :prefix

    def initialize(namespace, prefix, local_part)
      @namespace = namespace
      @prefix = prefix
      @local_part = local_part
    end

    def to_s
      prefix == '' ? @local_part : "#{@prefix}:#{@local_part}"
    end

    def QName::split(a)
      prefix = ''
      local = ''
      colon_at = a.index(@@colon)
      if colon_at
        prefix = a[0 ... colon_at].ustr # note three dots
        local = a[colon_at + 1 .. -1].ustr
      else
        local = a.ustr
      end
      return prefix, local
    end

    def QName.from_bytes(bytes, reader)
      pref, local = split bytes
      ns = reader.namespace_binding pref
      if ns || (pref == '')
        QName.new(ns, pref, local)
      else
        "Undefined namespace prefix #{pair[0]}" 
      end
    end

    # the argument is a list of strings, the first being the element type,
    #  subsequent pairs being attribute names and values.  This scans the list,
    #  detects any namespace declarations and installs them in the namespaces
    #  hash, removes such attributes, then turns the element type and attribute
    #  names into QNames, and returns the whole list
    #
    def QName.scan(list, namespaces, reader)
      working = []
      pref, local = split list[0]
      working << [pref, local]
      index = 1

      # find, remember, & remove namespace declarations
      while index < list.length
        pref, local = split list[index]
        if pref == 'xmlns'
          @namespaces[local] = list[index + 1]
        elsif pref == '' && local == 'xmlns'
          @namespaces[''] = list[index + 1]
        else
          working << [pref, local]
          working << list[index + 1]
        end
        index += 2
      end

      working[0] = try_new(working[0], reader)
      return working[0] unless working[0].kind_of? QName
      index = 1
      while index < working.length
        working[index] = try_new(working[index], reader)
        return working[index] unless working[index].kind_of? QName
        index += 2
      end

      return working
    end

    def ==(other)
      # note that we do *not* test prefix equality
      other.kind_of?(QName) &&
        @namespace == other.namespace &&
        @local_part == other.local_part
    end

    def QName.try_new(pair, reader)
      ns = reader.namespace_binding pair[0]
      if ns || (pair[0] == '')
        QName.new(ns, pair[0], pair[1])
      else
        "Undefined namespace prefix #{pair[0]}" 
      end
    end
  end

end
