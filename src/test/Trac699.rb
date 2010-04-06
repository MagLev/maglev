# Trac699.rb
class C
  ax = autoload :PBM, "#{File.dirname(__FILE__)}/Trac699a.rb"
  #puts "ax #{ax.inspect}"
  def x
    #aa = C::PBM
    #puts "aa #{aa}"
    bb = PBM::X
    #puts "bb #{bb}"
    bb
  end
end

unless C.new.x == 10699 ; raise 'error'; end
#puts "OK"
true
