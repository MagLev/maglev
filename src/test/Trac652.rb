module ActiveSupport
  module Autoload
    @@under_path = nil

    def autoload_under(path)
      @@under_path, old_path = path, @@under_path
      yield
    ensure
      @@under_path = old_path
    end
  end
end

class Cx
  @@under_path = 95
  def self.under(path)
    @@under_path, old_path = path, @@under_path
    unless old_path == 95; raise 'error'; end 
    @@under_path
  end
  def self.__under
    @@under_path
  end
end
rx = Cx.under(88)
unless rx == 88 ; raise 'error'; end
unless Cx.__under == 88 ; raise 'error'; end
true
