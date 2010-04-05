# Trac699.rb
class C
  autoload :PBM, "#{File.dirname(__FILE__)}/Trac699a.rb"
  def x
    p PBM::X
  end
end

C.new.x
