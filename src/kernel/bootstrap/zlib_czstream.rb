#  CZstream, Zlib::ZStream  defined in zlib_czstream1.rb

module Zlib

  def self.adler32(string="", initial=1)
    string = Maglev::Type.coerce_to(string, String, :to_s)
    unless initial._isFixnum
      # prim checks range of initial
      raise( initial._isNumeric ? RangeError : TypeError, 
		'adler32 initial crc must be a Fixnum >= 0')
    end
    ZStream.__adler32(string, initial);
  end

  def self.crc32(string="", initial=0)
    string = Maglev::Type.coerce_to(string, String, :to_s)
    unless initial._isFixnum
      # prim checks range of initial
      raise( initial._isNumeric ? RangeError : TypeError,
                'crc32 initial crc must be a Fixnum >= 0')
    end
    ZStream.__crc32(string, initial);
  end

  def self.crc_table
    ZStream.__crc_table
  end
end
