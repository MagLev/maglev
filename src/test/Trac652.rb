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
