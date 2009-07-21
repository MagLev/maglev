# Test for trac 552
module M
  def m_meth
    "M"
  end
end

class C
  include M
end

