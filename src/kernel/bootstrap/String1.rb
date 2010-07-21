class String
  primitive 'hash' , 'hash'  # So class Hash will operate

   MAGLEV_EXTRACT_BASE_TABLE = {"0b" => 2, "0d" => 10, "0o" => 8, "0x" => 16, "0" => 8 ,
                                "0B" => 2, "0D" => 10, "0O" => 8, "0X" => 16
                               }
   MAGLEV_EXTRACT_BASE_TABLE.freeze
end
String.__freeze_constants
