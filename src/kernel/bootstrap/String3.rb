class String

  # Return an array of three elements: [an_int, sign_string, a_string], where an_int is
  # base, unless self contains a base specifier (e.g., "0x", "0b", etc.),
  # in which case, an_int is the appropriate base.  a_string is self with
  # any base specifier removed.
  #
  # "0x10".__extract_base => [16, "", 10]
  # "-0b1010".__extract_base     => [2, "-", "1010"]
  # "-0b1010".__extract_base(16) => [2, "-", "1010"]
  # "-1010".__extract_base(16)   => [16, "-", "1010"]
  # MAGLEV_EXTRACT_BASE_TABLE defined in String2.rb

  def __extract_base(base)
    # for 1.8.7,  caller responsible for delete underscores and strip
    self =~ /^([+-]?)(0[bdox]?)?(.*)/i
    dtwo = $2
    base = MAGLEV_EXTRACT_BASE_TABLE[ dtwo ] unless dtwo._equal?(nil)
    [ base, $1, $3 ]  # result is [ baseFixnum , signString, magnitudeString ]
  end


end
