seq = 'tgcaacgacatttgaccaacttgaccattcctgcttgtagcgt'
seq.gsub!('B','(c|g|t)')
puts seq.length