class Hat
	def initialize
		@contents = []
	end
	
	def put(item)
		@contents << item
		nil
	end
	
	def contents
		@contents
	end
	
	def size
		@contents.size
	end
end

$hat = Hat.new
