# Trac699.rb
class C
  ax = autoload :PBM, "#{File.dirname(__FILE__)}/Trac699a.rb"
  #puts "ax #{ax.inspect}"
  def x
    #aa = C::PBM
    #puts "aa #{aa}"

    #bx = PBM
    #ax = ::PBM
    cx = PBM::X

    #puts "bb #{bb}"
    cx
  end
end

unless C.new.x == 10699 ; raise 'error'; end
#puts "OK"
true
