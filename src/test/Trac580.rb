xx_map = {
      "\C-[" => 22 ,
      "\C-\\" => 33 ,
      "\C-]" => 44
   }
unless xx_map.size == 3 ; raise 'error'; end
k = ' '
k[0] = 28
unless xx_map[k] == 33 ; raise 'error' ; end
true
