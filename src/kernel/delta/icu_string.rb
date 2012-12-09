# After bootstrap, strings become unicode aware
class String

  # TODOs...
  # chars(&block)
  # __uppercaseAt(pos)
  # <=>

  # Non TODO
  # casecmp - uses __upercaseAt


  # def __at(index, len=nil)
  #   if index._isInteger

  #   elsif index._isRange
  #     arr = index.__beg_len(self.__size)
  #     if arr._equal?(nil)
  #       nil
  #     else
  #       self.__at(arr[0], arr[1])
  #     end
  #   elsif index._isRegexp
  #     # TODO
  #   else
  #     self.__at(Type.__coerce_to_Fixnum_to_int(index))
  #   end
  # end
  # alias [] __at
end
