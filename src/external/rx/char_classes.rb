require 'rx/support'

module RX

  class CharClass
    @@classes = {
      'S' => " \t\n\r",
      'NameStart' => 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
      'NameC' =>
      'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-.0123456789_:',
      'Hex' => '0123456789abcdefABCDEF',
      'PubID' =>
      " \n\rabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-\'()+,./:=?",
      'EncName' => 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ._-'
    }
    
    # from XML 1.0 "#x9 | #xA | #xD | [#x20-#xD7FF] "
    dot = [ 0x9, 0xa, 0xd ]
    0x20.upto(127) { |c| dot << c }
    @@classes['.'] = dot.pack('C*')

    def CharClass.bytes(classname)
      @@classes[classname]
    end

    def CharClass.is_in(c, class_name)
      @@points[class_name].index(c) ||
        @@ranges[class_name].any? { |r| r.member?(c) }
    end
  end
end
