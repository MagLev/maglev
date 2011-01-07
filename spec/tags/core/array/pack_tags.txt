fails:Array#pack returns a tainted string when a pack argument is tainted
fails:Array#pack with format 'U' regards a integer as a Unicode codepoint and encodes into UTF-8 byte sequence
fails:Array#pack with format 'U' may accept a pack argument > max of Unicode codepoint
