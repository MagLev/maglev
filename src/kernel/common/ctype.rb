# depends on: module.rb

##
# Mixin containing byte classification methods.
#--
# isspace, islower, ... are not in MRI core library.  See specs in
# spec/shotgun/ctype_spec.rb

module CType
  # TODO: Add Unicode character support

  def isctrl
    self.size == 1 and (self == ?\n or self == ?\r or self == ?\t or self == ?\f or
    self == ?\v or self == ?\a or self == ?\e or self == ?\b)
  end

  def isspace
    self.size == 1 and (self == ?\s or self == ?\n or self == ?\t or self == ?\r or self == ?\f or self == ?\v)
  end

  def isupper
    self.size == 1 and (self >= ?A and self <= ?Z)
  end

  def islower
    self.size == 1 and (self >= ?a and self <= ?z)
  end

  def isdigit
    self.size == 1 and (self >= ?0 and self <= ?9)
  end

  def isalnum
    islower or isupper or isdigit
  end

  def toupper!
    (self.ord - ?\s.ord).chr
  end

  def toupper
    self.islower ? self.toupper! : self
  end

  def tolower!
    (self.ord + ?\s.ord).chr
  end

  def tolower
    self.isupper ? self.tolower! : self
  end

  def toprint
    if self == ?"
      "\\\""
    elsif self == ?\\
      "\\\\"
    elsif self == ?#
      "\\#"
    elsif isctrl
      case self
      when ?\n: "\\n"
      when ?\t: "\\t"
      when ?\a: "\\a"
      when ?\v: "\\v"
      when ?\f: "\\f"
      when ?\r: "\\r"
      when ?\e: "\\e"
      when ?\b: "\\b"
      end
    elsif self < 32 || self > 126
      str = "\\000"
      str.modify!

      c = self.to_s 8
      str.copy_from c, 0, c.size, 4-c.size
    else
      self.chr
    end
  end
end
