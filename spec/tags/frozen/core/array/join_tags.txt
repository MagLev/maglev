fails:Array#join returns a string formed by concatenating each element.to_s separated by separator without trailing separator
fails:Array#join returns a string which would be infected with taint of the array, its elements or the separator when the array is not empty
fails:Array#join raises a TypeError if the separator cannot be coerced to a String by calling #to_str
