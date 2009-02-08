#   test class creation related to singletons and Modules with empty namespace
o = [5]
cs = class << o
   class Enms ; end
end 


o.each do 
   class Espec; def ===(obj); obj == '2'; end; end
end

e = Espec.new
unless e.class.name == 'Espec' ; raise 'error' ; end
true
