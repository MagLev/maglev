class Sexp < Array
  def shift
    raise if self.empty?
    super
  end
end

s = Sexp.new
s << 42
raises "Shift broken" unless s.shift == 42
# Passes if it doesn't cause infinite recursion
