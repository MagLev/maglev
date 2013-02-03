#--
#   matrix.rb - 
#       $Release Version: 1.0$
#       $Revision: 1.11 $
#       $Date: 1999/10/06 11:01:53 $
#       Original Version from Smalltalk-80 version
#          on July 23, 1985 at 8:37:17 am
#       by Keiju ISHITSUKA
#++
#
# An implementation of Matrix and Vector classes.
#
# Author:: Keiju ISHITSUKA
# Documentation:: Gavin Sinclair (sourced from <i>Ruby in a Nutshell</i> (Matsumoto, O'Reilly)) 
#
# See classes Matrix and Vector for documentation. 
#
# Maglev - changes to optimize loops to be inline-loops where there was not a yield
#          in the body of the loop.


require "e2mmap.rb"  

module ExceptionForMatrix # :nodoc:
   extend Exception2MessageMapper
   def_e2message(TypeError, "wrong argument type %s (expected %s)")
   def_e2message(ArgumentError, "Wrong # of arguments(%d for %d)")
   
   def_exception("ErrDimensionMismatch", "\#{self.name} dimension mismatch")
   def_exception("ErrNotRegular", "Not Regular Matrix")
   def_exception("ErrOperationNotDefined", "This operation(%s) can\\'t defined")
end

#
# The +Matrix+ class represents a mathematical matrix, and provides methods for creating
# special-case matrices (zero, identity, diagonal, singular, vector), operating on them
# arithmetically and algebraically, and determining their mathematical properties (trace, rank,
# inverse, determinant).
#
# Note that although matrices should theoretically be rectangular, this is not
# enforced by the class.
#
# Also note that the determinant of integer matrices may be incorrectly calculated unless you
# also <tt>require 'mathn'</tt>.  This may be fixed in the future.
#
# == Method Catalogue
#
# To create a matrix:
# * <tt> Matrix[*rows]                  </tt>
# * <tt> Matrix.[](*rows)               </tt>
# * <tt> Matrix.rows(rows, copy = true) </tt>
# * <tt> Matrix.columns(columns)        </tt>
# * <tt> Matrix.diagonal(*values)       </tt>
# * <tt> Matrix.scalar(n, value)        </tt>
# * <tt> Matrix.scalar(n, value)        </tt>
# * <tt> Matrix.identity(n)             </tt>
# * <tt> Matrix.unit(n)                 </tt>
# * <tt> Matrix.I(n)                    </tt>
# * <tt> Matrix.zero(n)                 </tt>
# * <tt> Matrix.row_vector(row)         </tt>
# * <tt> Matrix.column_vector(column)   </tt>
#
# To access Matrix elements/columns/rows/submatrices/properties: 
# * <tt>  [](i, j)                      </tt>
# * <tt> #row_size                      </tt>
# * <tt> #column_size                   </tt>
# * <tt> #row(i)                        </tt>
# * <tt> #column(j)                     </tt>
# * <tt> #collect                       </tt>
# * <tt> #map                           </tt>
# * <tt> #minor(*param)                 </tt>
#
# Properties of a matrix:
# * <tt> #regular?                      </tt>
# * <tt> #singular?                     </tt>
# * <tt> #square?                       </tt>
#
# Matrix arithmetic:
# * <tt>  *(m)                          </tt>
# * <tt>  +(m)                          </tt>
# * <tt>  -(m)                          </tt>
# * <tt> #/(m)                          </tt>
# * <tt> #inverse                       </tt>
# * <tt> #inv                           </tt>
# * <tt>  **                            </tt>
#
# Matrix functions:
# * <tt> #determinant                   </tt>
# * <tt> #det                           </tt>
# * <tt> #rank                          </tt>
# * <tt> #trace                         </tt>
# * <tt> #tr                            </tt>
# * <tt> #transpose                     </tt>
# * <tt> #t                             </tt>
#
# Conversion to other data types:
# * <tt> #coerce(other)                 </tt>
# * <tt> #row_vectors                   </tt>
# * <tt> #column_vectors                </tt>
# * <tt> #to_a                          </tt>
#
# String representations:
# * <tt> #to_s                          </tt>
# * <tt> #inspect                       </tt>
#
class Matrix
  
#  extend Exception2MessageMapper
  include ExceptionForMatrix
  
  # instance creations # maglev need to reimplemente new to be able to make it private
  def self.new(rows, copy)
    inst = self.allocate
    inst.initialize( rows, copy )
    inst
  end
  private_class_method :new
  
  #
  # Creates a matrix where each argument is a row.
  #   Matrix[ [25, 93], [-1, 66] ]
  #      =>  25 93
  #          -1 66
  #
  def Matrix.[](*rows)
    new( rows, false)
  end

  def self.Raise(cls, *args)  # workaround for broken e2mmap.rb
    m = args[0]
    if m._isString
      raise(cls, m)
    else
      raise(cls, "")
    end
  end
  
  #
  # Creates a matrix where +rows+ is an array of arrays, each of which is a row
  # to the matrix.  If the optional argument +copy+ is false, use the given
  # arrays as the internal structure of the matrix without copying.
  #   Matrix.rows([[25, 93], [-1, 66]])
  #      =>  25 93
  #          -1 66
  def Matrix.rows(rows, copy = true)
    new(rows, copy)
  end
  
  #
  # Creates a matrix using +columns+ as an array of column vectors.
  #   Matrix.columns([[25, 93], [-1, 66]])
  #      =>  25 -1
  #          93 66
  #
  #
  def Matrix.columns(columns)
    #rows = (0 .. columns[0].size - 1).collect { |i|
    #  (0 .. columns.size - 1).collect { |j|
    #    columns[j][i]
    #  }
    #}
    c_size = columns.size
    czero = columns[0]
    r_siz = czero.size
    array_cls = Array
    rows = array_cls.new(r_siz)
    for i in 0..r_siz-1 do 
      inner_res = array_cls.new(c_size)
      for j in 0..c_size-1 do
	inner_res[j] = columns[j][i]   
      end
      rows[i] = inner_res
    end
    Matrix.rows(rows, false)
  end
  
  #
  # Creates a matrix where the diagonal elements are composed of +values+.
  #   Matrix.diagonal(9, 5, -3)
  #     =>  9  0  0
  #         0  5  0
  #         0  0 -3
  #
  def Matrix.diagonal(*values)
    size = values.size
    array_cls = Array
    # rows = (0 .. size  - 1).collect { |j| ... }
    rows = array_cls.new(size)
    for j in 0..size-1 do
      row = array_cls.new(size).fill(0, 0, size)
      row[j] = values[j]
      rows[j] = row
    end
    self.rows(rows, false)
  end
  
  #
  # Creates an +n+ by +n+ diagonal matrix where each diagonal element is
  # +value+.
  #   Matrix.scalar(2, 5)
  #     => 5 0
  #        0 5
  #
  def Matrix.scalar(n, value)
    Matrix.diagonal(*Array.new(n).fill(value, 0, n))
  end

  #
  # Creates an +n+ by +n+ identity matrix.
  #   Matrix.identity(2)
  #     => 1 0
  #        0 1
  #
  def Matrix.identity(n)
    Matrix.scalar(n, 1)
  end
  class << Matrix 
    alias unit identity
    alias I identity
  end
  
  #
  # Creates an +n+ by +n+ zero matrix.
  #   Matrix.zero(2)
  #     => 0 0
  #        0 0
  #
  def Matrix.zero(n)
    Matrix.scalar(n, 0)
  end
  
  #
  # Creates a single-row matrix where the values of that row are as given in
  # +row+.
  #   Matrix.row_vector([4,5,6])
  #     => 4 5 6
  #
  def Matrix.row_vector(row)
    matrix_cls = Matrix
    if row._isArray
      matrix_cls.rows([row.dup], false)
    elsif row._kind_of?(Vector)
      matrix_cls.rows([row.to_a], false)
    else
      matrix_cls.rows([[row]], false)
    end
  end
  
  #
  # Creates a single-column matrix where the values of that column are as given
  # in +column+.
  #   Matrix.column_vector([4,5,6])
  #     => 4
  #        5
  #        6
  #
  def Matrix.column_vector(column)
    matrix_cls = Matrix
    if column._isArray
      matrix_cls.columns([column])
    elsif column._kind_of?( Vector )
      matrix_cls.columns([column.to_a])
    else
      matrix_cls.columns([[column]])
    end
  end

  #
  # This method is used by the other methods that create matrices, and is of no
  # use to general users.
  #
  #def initialize(init_method, *argv)  # maglev, eliminated the send
  #  self.send(init_method, *argv)
  #end

  def __init_check_row(arow)
    if arow._isArray
      return arow
    end
    if arow._kind_of?( Vector )
      return arow.__elements
    end
    Maglev::Type.coerce_to( arow, Array, :to_ary )
  end
  
  def initialize(rows, copy) # maglev , renamed from init_rows
    if copy
      r_size = rows.size
      c_rows = Array.new(r_size)
      if r_size > 0
        arow = __init_check_row(rows[0])
        first_row_size = arow.size
        c_rows[0] = arow.dup
        for i in 1..r_size-1 do
          arow = __init_check_row(rows[i])
          if arow.size._not_equal?( first_row_size)
            self.class.Raise( ErrDimensionMismatch , 'rows have varying sizes')
          end
          c_rows[i] = arow.dup
        end
      end
      @rows = c_rows
    else
      # per library/matrix/constructor_spec.rb must check elements of arg
      r_size = rows.size
      if r_size > 0
        brow = __init_check_row(arow = rows[0])
        if brow._not_equal?(arow)
          rows[0] = brow
        end 
        first_row_size = brow.size
        for i in 1..r_size-1 do
          brow = __init_check_row(rows[i])
          if brow.size._not_equal?( first_row_size)
            self.class.Raise( ErrDimensionMismatch , 'rows have varying sizes')
          end
          if brow._not_equal?(arow)
            rows[i] = brow
          end
        end
      end
      @rows = rows
    end
    self
  end
  private :initialize  # maglev allows send from within self.new
  
  #
  # Returns element (+i+,+j+) of the matrix.  That is: row +i+, column +j+.
  #
  def [](i, j)
    #  @rows[i][j]  # Maglev, fix ruby_bug "#1518", "1.9.1.129"
    elem = @rows[i]
    unless elem._equal?(nil)
      elem = elem[j]
    end
    elem
  end

  #
  # Returns the number of rows.
  #
  def row_size
    @rows.size
  end
  
  #
  # Returns the number of columns.  Note that it is possible to construct a
  # matrix with uneven columns (e.g. Matrix[ [1,2,3], [4,5] ]), but this is
  # mathematically unsound.  This method uses the first row to determine the
  # result.
  #
  def column_size
    first_row = @rows[0]
    if first_row._equal?(nil)
      return 0
    end
    first_row.size
  end

  #
  # Returns row vector number +i+ of the matrix as a Vector (starting at 0 like
  # an array).  When a block is given, the elements of that vector are iterated.
  #
  def row(i) # :yield: e
    if block_given?
      for e in @rows[i]
        yield e
      end
    else
      Vector.elements(@rows[i])
    end
  end

  #
  # Returns column vector number +j+ of the matrix as a Vector (starting at 0
  # like an array).  When a block is given, the elements of that vector are
  # iterated.
  #
  def column(j) # :yield: e
    if block_given?
      0.upto(row_size - 1) do
        |i|
        yield @rows[i][j]
      end
    else
      rows = @rows
      r_size = rows.size
      cols = Array.new(r_size)
      for i in 0..r_size-1 do
        cols[i] = rows[i][j]
      end
      Vector.elements(cols, false)
    end
  end
  
  #
  # Returns a matrix that is the result of iteration of the given block over all
  # elements of the matrix.
  #   Matrix[ [1,2], [3,4] ].collect { |i| i**2 }
  #     => 1  4
  #        9 16
  #
  def collect(&block) # :yield: e
    # unless block_given? 
    #   Enumerator subclass not implemented yet for 1.8.7 
    #   MRI 1.8.7 p249 does not implement the Enumerator either
    # end
    rows = @rows
    r_size = rows.size
    c_rows = Array.new(r_size)
    for i in 0..r_size-1 do
      a_row = rows[i]
      c_rows[i] = a_row.collect{|e| yield(e) }
    end
    Matrix.rows(c_rows, false)
  end
  alias map collect
  
  #
  # Returns a section of the matrix.  The parameters are either:
  # *  start_row, nrows, start_col, ncols; OR
  # *  col_range, row_range
  #
  #   Matrix.diagonal(9, 5, -3).minor(0..1, 0..2)
  #     => 9 0 0
  #        0 5 0
  #
  def minor(*param)
    psz = param.size
    matrix_cls = Matrix
    if psz == 2
      from_row = param[0].first
      size_row = param[0].end - from_row
      size_row += 1 unless param[0].exclude_end?
      from_col = param[1].first
      size_col = param[1].end - from_col
      size_col += 1 unless param[1].exclude_end?
    elsif psz == 4
      from_row = param[0]
      size_row = param[1]
      from_col = param[2]
      size_col = param[3]
    else
      matrix_cls.Raise ArgumentError, param.inspect
    end
    # rows = @rows[from_row, size_row].collect{ |row|
    #  row[from_col, size_col]
    # }
    my_rows = @rows
    my_rows_size = my_rows.size
    ridx = from_row
#   if (ridx >= my_rows_size)
#     raise ArgumentError, 'start_row out of bounds'
#   end
    r_lim = (ridx + size_row).__min(my_rows_size)
    res_rows = Array.new(r_lim - ridx)
    res_idx = 0 
    while ridx < r_lim
      a_row = my_rows[ridx]
      res_rows[res_idx] = a_row[from_col, size_col]	
      res_idx += 1
      ridx += 1
    end
    matrix_cls.rows(res_rows, false)
  end
 
  #--
  # TESTING -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++

  #
  # Returns +true+ if this is a regular matrix.
  #
  def regular?
    square? and rank == column_size
  end
  
  #
  # Returns +true+ is this is a singular (i.e. non-regular) matrix.
  #
  def singular?
    not regular?
  end

  #
  # Returns +true+ is this is a square matrix.  See note in column_size about this
  # being unreliable, though.
  #
  def square?
    column_size == row_size
  end
  
  #--
  # OBJECT METHODS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++

  #
  # Returns +true+ if and only if the two matrices contain equal elements.
  #
  def ==(other)
    return false unless Matrix === other
    
    other.compare_by_row_vectors(@rows)
  end
  alias eql? ==  
  
  #
  # Not really intended for general consumption.
  #
  def compare_by_row_vectors(rows)
    my_rows = @rows
    sz = my_rows.size 
    return false unless sz == rows.size
    
    for i in 0..sz-1 do
      return false unless my_rows[i] == rows[i]
    end
    true
  end

  #
  # Returns a clone of the matrix, so that the contents of each do not reference
  # identical objects.
  #
  def clone
    Matrix.rows(@rows)
  end
  
  #
  # Returns a hash-code for the matrix.
  #
  def hash
    value = 0
    my_rows = @rows
    r_size = my_rows.size
    for i in 0..r_size-1 do
      a_row = my_rows[i]
      a_size = a_row.size
      for j in 0..a_size-1 do
        value ^= a_row[j].hash
      end
    end
    return value
  end
  
  #--
  # ARITHMETIC -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++
  
  #
  # Matrix multiplication.
  #   Matrix[[2,4], [6,8]] * Matrix.identity(2)
  #     => 2 4
  #        6 8
  #
  def *(m) # m is matrix or vector or number
    matrix_cls = Matrix
    array_cls = Array
    if m._isNumeric
      # rows = @rows.collect { |row|
      #   row.collect { |e| e * m }
      # }
      my_rows = @rows
      my_rows_size = my_rows.size
      rows = array_cls.new(my_rows_size)
      for i in 0..my_rows_size-1 do
        row = my_rows[i]
        r_size = row.size
        res_row = array_cls.new(r_size)
        for j in 0..r_size-1 do
          res_row[j] = row[j] * m
        end
        rows[i] = res_row
      end
      return matrix_cls.rows(rows, false)
    elsif m._kind_of?( Vector )
      m = matrix_cls.column_vector(m)
      r = self * m
      return r.column(0)
    elsif m._kind_of?( matrix_cls )
      # Matrix.Raise ErrDimensionMismatch if column_size != m.row_size
      # rows = (0 .. row_size - 1).collect { |i|
      #   (0 .. m.column_size - 1).collect { |j|
      #     vij = 0
      #     0.upto(column_size - 1) { |k| vij += self[i, k] * m[k, j] }
      #     vij
      #   }
      # }
      my_column_size = self.column_size
      other_column_size = m.column_size
      matrix_cls.Raise ErrDimensionMismatch if my_column_size != m.row_size
      my_rows = @rows
      my_rows_size = my_rows.size
      rows = array_cls.new(my_rows_size)
      for i in 0..my_rows_size-1 do
        res_row = array_cls.new(other_column_size)
        for j in 0..other_column_size-1 do
          vij = 0
          for k in 0..my_column_size-1 do
            vij += self[i, k] * m[k, j]	
          end
          vij
          res_row[j] = vij
        end
        rows[i] = res_row
      end
      return matrix_cls.rows(rows, false)
    else
      cr = m.coerce(self)
      return cr[0] * cr[1]
    end
  end
  
  #
  # Matrix addition.
  #   Matrix.scalar(2,5) + Matrix[[1,0], [-4,7]]
  #     =>  6  0
  #        -4 12
  #
  def +(m)
    matrix_cls = Matrix
    if m._isNumeric
      matrix_cls.Raise ErrOperationNotDefined, "+"
    elsif m._kind_of?(matrix_cls)
      # no coercion
    elsif m._kind_of?(Vector)
      m = matrix_cls.column_vector(m)
    else
      cr = m.coerce(self)
      return cr[0] | cr[1]
    end
    
    my_rows = @rows
    my_rows_size = my_rows.size
    my_column_size = self.column_size

    matrix_cls.Raise ErrDimensionMismatch unless my_rows_size == m.row_size and my_column_size == m.column_size
    #rows = (0 .. row_size - 1).collect { |i|
    #  (0 .. column_size - 1).collect { |j|
    #    self[i, j] + m[i, j]
    #  }
    array_cls = Array
    rows = array_cls.new(my_rows_size)
    for i in 0..my_rows_size-1 do
      a_row = array_cls.new(my_column_size)
      for j in 0..my_column_size-1 do
        a_row[j] = self[i, j] + m[i, j]
      end
      rows[i] = a_row
    end
    matrix_cls.rows(rows, false)
  end

  #
  # Matrix subtraction.
  #   Matrix[[1,5], [4,2]] - Matrix[[9,3], [-4,1]]
  #     => -8  2
  #         8  1
  #
  def -(m)
   matrix_cls = Matrix
    if m._isNumeric
      matrix_cls.Raise ErrOperationNotDefined, "-"
    elsif m._kind_of?(matrix_cls)
      # no coercion
    elsif m._kind_of?(Vector)
      m = matrix_cls.column_vector(m)
    else
      cr = m.coerce(self)
      return cr[0] - cr[1]
    end

    my_rows = @rows
    my_rows_size = my_rows.size
    my_column_size = self.column_size
    
    matrix_cls.Raise ErrDimensionMismatch unless my_rows_size == m.row_size and my_column_size == m.column_size
    
    #rows = (0 .. row_size - 1).collect { |i|
    #  (0 .. column_size - 1).collect { |j|
    #    self[i, j] - m[i, j]
    #  }
    #}
    array_cls = Array
    rows = array_cls.new(my_rows_size)
    for i in 0..my_rows_size-1 do
      a_row = array_cls.new(my_column_size)
      for j in 0..my_column_size-1 do
        a_row[j] = self[i, j] - m[i, j]
      end
      rows[i] = a_row
    end
    matrix_cls.rows(rows, false)
  end
  
  #
  # Matrix division (multiplication by the inverse).
  #   Matrix[[7,6], [3,9]] / Matrix[[2,9], [3,1]]
  #     => -7  1
  #        -3 -6
  #
  def /(other)
    matrix_cls = Matrix
    if other._isNumeric
      # rows = @rows.collect { |row|
      #   row.collect { |e| e / other }
      # }
      array_cls = Array
      my_rows = @rows
      my_rows_size = my_rows.size
      rows = array_cls.new(my_rows_size)
      for i in 0..my_rows_size-1 do
        row = my_rows[i]
        r_size = row.size
        res_row = array_cls.new(r_size)
        for j in 0..r_size-1 do
          res_row[j] = row[j] / other 
        end
        rows[i] = res_row
      end
      return matrix_cls.rows(rows, false)
    elsif other._kind_of?(matrix_cls)
      return self * other.inverse
    else
      cr = other.coerce(self)
      return cr[0] / cr[1]
    end
  end

  #
  # Returns the inverse of the matrix.
  #   Matrix[[1, 2], [2, 1]].inverse
  #     => -1  1
  #         0 -1
  #
  def inverse
    Matrix.Raise ErrDimensionMismatch unless square?
    Matrix.I(row_size).inverse_from(self)
  end
  alias inv inverse

  #
  # Not for public consumption?
  #
  def inverse_from(src)
    size = row_size - 1
    a = src.to_a
    
    my_rows = @rows
    for k in 0..size
      i = k
      akk = a[k][k].abs
      for j in (k+1)..size
        v = a[j][k].abs
        if v > akk
          i = j
          akk = v
        end
      end
      Matrix.Raise ErrNotRegular if akk == 0
      if i != k
        a[i], a[k] = a[k], a[i]
        my_rows[i], my_rows[k] = my_rows[k], my_rows[i]
      end
      akk = a[k][k]
      
      for i in 0 .. size
        next if i == k
        q = a[i][k] / akk
        a[i][k] = 0
        
        #(k + 1).upto(size) do
        j = k + 1
        while j <= size 
          a[i][j] -= a[k][j] * q
          j += 1
        end
        # 0.upto(size) do
        j = 0
        while j <= size
          my_rows[i][j] -= my_rows[k][j] * q
          j += 1
        end
      end
      
      #(k + 1).upto(size) do
      j = k + 1
      while j <= size
        a[k][j] /= akk
        j += 1
      end
      # 0.upto(size) do
      j = 0
      while j <= size
        my_rows[k][j] /= akk
        j += 1
      end
    end
    self
  end
  #alias reciprocal inverse
  
  #
  # Matrix exponentiation.  Defined for integer powers only.  Equivalent to
  # multiplying the matrix by itself N times.
  #   Matrix[[7,6], [3,9]] ** 2
  #     => 67 96
  #        48 99
  #
  def ** (other)
    if other._isInteger
      x = self
      if other <= 0
        x = self.inverse
        return Matrix.identity(self.column_size) if other == 0
        other = -other
      end
      z = x
      n = other  - 1
      while n != 0
        while (div, mod = n.divmod(2)
               mod == 0)
          x = x * x
          n = div
        end
        z *= x
        n -= 1
      end
      z
    elsif other._isFloat || defined?(Rational) && other._kind_of?(Rational)
      Matrix.Raise ErrOperationNotDefined, "**"
    else
      Matrix.Raise ErrOperationNotDefined, "**"
    end
  end
  
  #--
  # MATRIX FUNCTIONS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++
  
  #
  # Returns the determinant of the matrix.  
  # If the matrix is not square, the ErrDimensionMismatch is raised.
  # If the matrix is empty, result is 1
  #   Matrix[[7,6], [3,9]].determinant
  #     => 63
  #
  def determinant
    Matrix.Raise ErrDimensionMismatch unless square?
    
    rsz = row_size  
    if rsz == 0
      return 1
    end
    size = rsz - 1
    a = to_a
    
    det = 1
    k = 0
    begin 
      if (akk = a[k][k]) == 0
        i = k
        begin
          return 0 if (i += 1) > size
        end while a[i][k] == 0
        a[i], a[k] = a[k], a[i]
        akk = a[k][k]
        det *= -1
      end
      # (k + 1).upto(size) do
      i = k + 1
      while i <= size
        q = a[i][k] / akk  # with mathn , #/  produces Rationals, etc, otherwise truncates ...
        # (k + 1).upto(size) do
        j = k + 1
        while j <= size
          a[i][j] -= a[k][j] * q
          j += 1
        end
        i += 1
      end
      det *= akk
    end while (k += 1) <= size
    det
  end
  alias det determinant
        
  #
  # Returns the rank of the matrix.  Beware that using Float values, with their
  # usual lack of precision, can affect the value returned by this method.  Use
  # Rational values instead if this is important to you.
  #   Matrix[[7,6], [3,9]].rank
  #     => 2
  #
  def rank
    if column_size > row_size
      a = transpose.to_a
      a_column_size = row_size
      a_row_size = column_size
    else
      a = to_a
      a_column_size = column_size
      a_row_size = row_size
    end
    rank = 0
    k = 0
    begin
      if (akk = a[k][k]) == 0
        i = k
        exists = true
        begin
          if (i += 1) > a_column_size - 1
            exists = false
            break
          end
        end while a[i][k] == 0
        if exists
          a[i], a[k] = a[k], a[i]
          akk = a[k][k]
        else
          i = k
          exists = true
          begin
            if (i += 1) > a_row_size - 1
              exists = false
              break
            end
          end while a[k][i] == 0
          if exists
            # k.upto(a_column_size - 1) do
            j = k
            while j < a_column_size
              a[j][k], a[j][i] = a[j][i], a[j][k]
              j += 1
            end
            akk = a[k][k]
          else
            next
          end
        end
      end
      # (k + 1).upto(a_row_size - 1) do
      i = k + 1
      while i < a_row_size
        q = a[i][k] / akk
        # (k + 1).upto(a_column_size - 1) do
        j = k + 1
        while j < a_column_size
          a[i][j] -= a[k][j] * q
          j += 1
        end
        i += 1
      end
      rank += 1
    end while (k += 1) <= a_column_size - 1
    return rank
  end

  #
  # Returns the trace (sum of diagonal elements) of the matrix.
  #   Matrix[[7,6], [3,9]].trace
  #     => 16
  #
  def trace
    sum = 0
    # 0.upto(column_size - 1) do
    my_rows = @rows
    col_size = my_rows[0].size 
    for i in 0..col_size-1 do 
      row = my_rows[i]
      if row._equal?(nil)
        Matrix.Raise(ErrDimensionMismatch, 'not square') 
      end
      sum += row[i]
    end
    sum
  end
  alias tr trace
  
  #
  # Returns the transpose of the matrix.
  #   Matrix[[1,2], [3,4], [5,6]]
  #     => 1 2
  #        3 4
  #        5 6
  #   Matrix[[1,2], [3,4], [5,6]].transpose
  #     => 1 3 5
  #        2 4 6
  #
  def transpose
    Matrix.columns(@rows)
  end
  alias t transpose
  
  #--
  # CONVERTING -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++
  
  #
  # FIXME: describe #coerce.
  #
  def coerce(other)
    if other._isNumeric
      return Scalar.new(other), self
    else
      raise TypeError, "#{self.class} can't be coerced into #{other.class}"
    end
  end

  #
  # Returns an array of the row vectors of the matrix.  See Vector.
  #
  def row_vectors
    # (0 .. row_size - 1).collect { |i| row(i) }
    my_rows = @rows
    r_size = my_rows.size
    res = Array.new(r_size)
    vector_cls = Vector
    for i in 0..r_size-1 do
      a_row = my_rows[i]
      res[i] = vector_cls.elements(a_row) 
    end
    res
  end
  
  #
  # Returns an array of the column vectors of the matrix.  See Vector.
  #
  def column_vectors
    # (0 .. column_size - 1).collect { |i| column(i) }
    my_rows = @rows
    r_size = my_rows.size
    col_size = my_rows[0].size
    vector_cls = Vector
    array_cls = Array
    res = array_cls.new(col_size)
    for j in 0..col_size-1 do
      a_col = array_cls.new(r_size)
      for i in 0..r_size-1 do
        a_col[i] = my_rows[i][j]
      end
      res[j] = Vector.elements(a_col, false)
    end
    res
  end
  
  #
  # Returns an array of arrays that describe the rows of the matrix.
  #
  def to_a
    # @rows.collect{|row| row.collect{|e| e}}  
    my_rows = @rows
    r_size = my_rows.size
    res = Array.new(r_size)
    for i in 0..r_size-1 do
      res[i] = my_rows[i].dup
    end
    res
  end
  
  #--
  # PRINTING -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++
  
  #
  # Overrides Object#to_s
  #
  def to_s
    "Matrix[" + @rows.collect{
      |row|
      "[" + row.collect{|e| e.to_s}.join(", ") + "]"  # no yield
    }.join(", ")+"]"
  end
  
  #
  # Overrides Object#inspect
  #
  def inspect
    "Matrix"+@rows.inspect
  end
  
  # Private CLASS
  
  class Scalar < Numeric # :nodoc:
    include ExceptionForMatrix
    
    def initialize(value)
      @value = value
    end
    
    # ARITHMETIC
    def +(other)
      scalar = Scalar
      if other._kind_of?( scalar)
        scalar.new(@value + other.value)
      elsif other._isNumeric
        scalar.new(@value + other)
      elsif other._kind_of?( Vector) || other._kind_of?( Matrix)
        scalar.Raise WrongArgType, other.class, "Numeric or Scalar"
      else
        cr = other.coerce(self)
        cr[0] + cr[1] 
      end
    end
    
    def -(other)
      scalar = Scalar
      if other._kind_of?( scalar)
        scalar.new(@value - other.value)
      elsif other._isNumeric
        scalar.new(@value - other)
      elsif other._kind_of?( Vector) || other._kind_of?( Matrix)
        scalar.Raise WrongArgType, other.class, "Numeric or Scalar"
      else
        cr = other.coerce(self)
        cr[0] - cr[1]
      end
    end
    
    def *(other)
      scalar = Scalar
      if other._kind_of?( scalar)
        scalar.new(@value * other.value)  # Maglev addition
      elsif other._isNumeric
        scalar.new(@value * other)
      elsif other._kind_of?( Vector) || other._kind_of?( Matrix)
        other.collect{|e| @value * e}
      else
        cr = other.coerce(self)
        cr[0] * cr[1]
      end
    end
    
    def / (other)
      scalar = Scalar
      if other._kind_of?( scalar)
        scalar.new(@value / other.value)  # Maglev addition
      elsif other._isNumeric
        scalar.new(@value / other)
      elsif other._kind_of?( Vector)
        scalar.Raise WrongArgType, other.class, "Numeric or Scalar or Matrix"
      elsif other._kind_of?( Matrix)
        self * other.inverse
      else
        cr = other.coerce(self)
        cr[0] / cr[1]
      end
    end
    
    def ** (other)
      scalar = Scalar
      if other._kind_of?( scalar)
        scalar.new(@value ** other.value)  # Maglev addition
      elsif other._isNumeric
        scalar.new(@value ** other)
      elsif other._kind_of?( Vector)
        scalar.Raise WrongArgType, other.class, "Numeric or Scalar or Matrix"
      elsif other._kind_of?( Matrix)
        other.powered_by(self)
      else
        cr = other.coerce(self)
        cr[0] ** cr[1]
      end
    end
  end
end


#
# The +Vector+ class represents a mathematical vector, which is useful in its own right, and
# also constitutes a row or column of a Matrix.
#
# == Method Catalogue
#
# To create a Vector:
# * <tt>  Vector.[](*array)                   </tt>
# * <tt>  Vector.elements(array, copy = true) </tt>
#
# To access elements:
# * <tt>  [](i)                               </tt>
#
# To enumerate the elements:
# * <tt> #each2(v)                            </tt>
# * <tt> #collect2(v)                         </tt>
#
# Vector arithmetic:
# * <tt>  *(x) "is matrix or number"          </tt>
# * <tt>  +(v)                                </tt>
# * <tt>  -(v)                                </tt>
#
# Vector functions:
# * <tt> #inner_product(v)                    </tt>
# * <tt> #collect                             </tt>
# * <tt> #map                                 </tt>
# * <tt> #map2(v)                             </tt>
# * <tt> #r                                   </tt>
# * <tt> #size                                </tt>
#
# Conversion to other data types:
# * <tt> #covector                            </tt>
# * <tt> #to_a                                </tt>
# * <tt> #coerce(other)                       </tt>
#
# String representations:
# * <tt> #to_s                                </tt>
# * <tt> #inspect                             </tt>
#
class Vector
  include ExceptionForMatrix
  
  #INSTANCE CREATION
  
  private_class_method :new

  #
  # Creates a Vector from a list of elements.
  #   Vector[7, 4, ...]
  #
  def Vector.[](*array)
    new(:init_elements, array, copy = false)
  end
  
  #
  # Creates a vector from an Array.  The optional second argument specifies
  # whether the array itself or a copy is used internally.
  #
  def Vector.elements(array, copy = true)
    new(:init_elements, array, copy)
  end
  
  #
  # For internal use.
  #
  def initialize(method, array, copy)
    self.send(method, array, copy)
  end
  
  #
  # For internal use.
  #
  def init_elements(array, copy)
    if copy
      @elements = array.dup
    else
      @elements = array
    end
  end
  
  # ACCESSING
         
  def __elements
    return @elements
  end
  #
  # Returns element number +i+ (starting at zero) of the vector.
  #
  def [](i)
    @elements[i]
  end
  
  #
  # Returns the number of elements in the vector.
  #
  def size
    @elements.size
  end
  
  #--
  # ENUMERATIONS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++

  #
  # Iterate over the elements of this vector and +v+ in conjunction.
  #
  def each2(v) # :yield: e1, e2
    my_size = size
    Vector.Raise ErrDimensionMismatch if my_size != v.size
    0.upto(my_size - 1) do
      |i|
      yield @elements[i], v[i]
    end 
  end
  
  #
  # Collects (as in Enumerable#collect) over the elements of this vector and +v+
  # in conjunction.
  #
  def collect2(v) # :yield: e1, e2
    my_size = size
    Vector.Raise ErrDimensionMismatch if my_size != v.size
    (0 .. my_size - 1).collect do
      |i|
      yield @elements[i], v[i]
    end 
  end

  #--
  # COMPARING -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++

  #
  # Returns +true+ iff the two vectors have the same elements in the same order.
  #
  def ==(other)
    return false unless Vector === other
    
    other.compare_by(@elements)
  end
  alias eql? ==
  
  #
  # For internal use.
  #
  def compare_by(elements)
    @elements == elements
  end
  
  #
  # Return a copy of the vector.
  #
  def clone
    Vector.elements(@elements)
  end
  
  #
  # Return a hash-code for the vector.
  #
  def hash
    @elements.hash
  end
  
  #--
  # ARITHMETIC -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++
  
  #
  # Multiplies the vector by +x+, where +x+ is a number or another vector.
  #
  def *(x)
    if x._isNumeric
      # els = @elements.collect{|e| e * x}  
      my_elems = @elements
      e_size = my_elems.size
      els = Array.new(e_size)
      for i in 0..e_size-1 do
        els[i] = my_elems[i] * x
      end

      Vector.elements(els, false)
    else
      matrix_cls = Matrix
      if x._kind_of?( matrix_cls )
        matrix_cls.column_vector(self) * x
      else
        cr = x.coerce(self)
        cr[0] * cr[1]
      end
    end
  end

  #
  # Vector addition.
  #
  def +(v)
    vector_cls = Vector
    if v._kind_of?( vector_cls)
      # els = collect2(v) {  |v1, v2| v1 + v2 }
      elems = @elements
      my_size = elems.size
      other_size = v.size
      vector_cls.Raise ErrDimensionMismatch if my_size != other_size
      els = Array.new(my_size)
      for i in 0..my_size-1 do
        els[i] = elems[i] + v[i]
      end

      vector_cls.elements(els, false)
    else
      matrix_cls = Matrix
      if v._kind_of?( matrix_cls )
        matrix_cls.column_vector(self) + v
      else
        cr = v.coerce(self)
        cr[0] + cr[1]
      end
    end
  end

  #
  # Vector subtraction.
  #
  def -(v)
    vector_cls = Vector
    if v._kind_of?( vector_cls )
      # els = collect2(v) { |v1, v2| v1 - v2 }
      elems = @elements
      my_size = elems.size
      other_size = v.size
      vector_cls.Raise ErrDimensionMismatch if my_size != other_size
      els = Array.new(my_size)
      for i in 0..my_size-1 do
        els[i] = elems[i] - v[i]
      end

      vector_cls.elements(els, false)
    else
      matrix_cls = Matrix
      if v._kind_of?( matrix_cls )
        matrix_cls.column_vector(self) - v
      else
        cr = v.coerce(self)
        cr[0] - cr[1]
      end 
    end
  end
  
  #--
  # VECTOR FUNCTIONS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++
  
  #
  # Returns the inner product of this vector with the other.
  #   Vector[4,7].inner_product Vector[10,1]  => 47
  #
  def inner_product(v)
    p = 0

    # each2(v) { |v1, v2| p += v1 * v2 }
    elems = @elements
    my_size = elems.size
    other_size = v.size
    Vector.Raise ErrDimensionMismatch if my_size != other_size
    for i in 0..my_size-1 do
      p += elems[i] * v[i]
    end

    p
  end
  
  #
  # Like Array#collect.
  #
  def collect # :yield: e
    elems = @elements
    sz = elems.size
    res = []
    i = 0
    lim = sz - 1
    while i < lim
      res << yield(elems[i])
       i += 1
    end
    res
    Vector.elements(res, false)
  end
  alias map collect
  
  #
  # Like Vector#collect2, but returns a Vector instead of an Array.
  #
  def map2(v) # :yield: e1, e2
    els = collect2(v) {
      |v1, v2|
      yield v1, v2
    }
    Vector.elements(els, false)
  end
  
  #
  # Returns the modulus (Pythagorean distance) of the vector.
  #   Vector[5,8,2].r => 9.643650761
  #
  def r
    v = 0
    # for e in @elements ; v += e*e ; end
    my_elems = @elements
    my_size = my_elems.size
    for i in 0..my_size-1 do
      e = my_elems[i]
      v += e * e
    end
    return Math.sqrt(v)
  end
  
  #--
  # CONVERTING
  #++

  #
  # Creates a single-row matrix from this vector.
  #
  def covector
    Matrix.row_vector(self)
  end
  
  #
  # Returns the elements of the vector in an array.
  #
  def to_a
    @elements.dup
  end
  
  #
  # FIXME: describe Vector#coerce.
  #
  def coerce(other)
    if other._isNumeric
      return Scalar.new(other), self
    else
      raise TypeError, "#{self.class} can't be coerced into #{other.class}"
    end
  end
  
  #--
  # PRINTING -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  #++
  
  #
  # Overrides Object#to_s
  #
  def to_s
    "Vector[" + @elements.join(", ") + "]"
  end
  
  #
  # Overrides Object#inspect
  #
  def inspect
    str = "Vector"+@elements.inspect
  end
end


# Documentation comments:
#  - Matrix#coerce and Vector#coerce need to be documented
