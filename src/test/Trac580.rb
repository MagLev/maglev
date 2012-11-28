# coverage for Trac 580
xx_map = {
      "\C-[" => 22 ,
      "\C-\\" => 33 ,
      "\C-]" => 44
   }
unless xx_map.size == 3 ; raise 'error'; end
k = ' '
k[0] = 28.chr
unless xx_map[k] == 33 ; raise 'error' ; end

# Coverage for Trac565
ra = /abc/iomxiomx
rb = /abc/iomx
unless ra == rb ; raise 'error'; end

# coverage for lexing of octals
na = 0o1234
nb = 0O1234
unless na == 668 ; raise 'error'; end
unless nb == 668 ; raise 'error'; end

true
