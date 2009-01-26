#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

class String
  # A convenient way to do File.join
  # Usage:
  #   'a' / 'b' # => 'a/b'
  def / obj
    Ramaze.deprecated('String#/', 'File::join')
    File.join(self, obj.to_s)
  end
end

class Symbol
  # A convenient way to do File.join
  # Usage:
  #   :dir/:file # => 'dir/file'
  def / obj
    Ramaze.deprecated('Symbol#/', 'File::join(sym.to_s)')
    self.to_s / obj
  end
end
