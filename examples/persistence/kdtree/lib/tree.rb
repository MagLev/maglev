module Collections
  class AVLTree
    def initialize(&block)
      @compare = block_given? ? block : proc {|a,b| a <=> b }
      @head = nil
    end

    def insert(datum)
      if @head.nil?
        @head = AVLTreeNode.new datum
      else
        @head.insert(datum, compare)
      end
    end
  end

  class AVLTreeNode
    attr_reader :balance
    def initialize(datum, left=nil, right=nil)
      raise ArgumentError, "No datum" if datum.nil?
      @datum = datum
      @left = left
      @right = right
      @balance = 0
    end

    def insert(datum, compare)
      if compare.call(@datum, datum)
        if @right.nil?
          @right = new(datum)
          @balance -= 1
        else
          @right.insert(datum, compare)
        end
      else
        if @left.nil?
          @left = new(datum)
          @balance += 1
        else
          @left.insert(datum, compare)
        end
      end
      rebalance
    end

    def rebalance
      if @balance > 1 || @balance < -1
        # rebalance
      end
      @balance
    end
  end
end
