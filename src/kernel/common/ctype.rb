# depends on: module.rb

##
# Mixin containing byte classification methods.
#--
# isspace, islower, ... are not in MRI core library.  See specs in
# spec/shotgun/ctype_spec.rb

module CType
  def isctrl
    self.chr == ?\n or self.chr == ?\r or self.chr == ?\t or self.chr == ?\f or
    self.chr == ?\v or self.chr == ?\a or self.chr == ?\e or self.chr == ?\b
  end

  def isspace
    self.chr == ?\s or self.chr == ?\n or self.chr == ?\t or self.chr == ?\r or self.chr == ?\f or self.chr == ?\v
  end

  def isupper
    self.chr >= ?A and self.chr <= ?Z
  end

  def islower
    self.chr >= ?a and self.chr <= ?z
  end

  def isdigit
    self.chr >= ?0 and self.chr <= ?9
  end

  def isalnum
    islower or isupper or isdigit
  end

  def toupper!
    self - 32
  end

  def toupper
    self.islower ? self.toupper! : self
  end

  def tolower!
    self + 32
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
