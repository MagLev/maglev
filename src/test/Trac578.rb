# From http://lojic.com/blog/2008/09/04/how-to-write-a-spelling-corrector-in-ruby/
# Copyright (c) Brian Adkins.

def words(text)
  text.downcase.scan(/[a-z]+/)
end

def train(features)
  model = Hash.new(1)
  features.each {|f| model[f] += 1 }
  return model
end

LETTERS = ("a".."z").to_a.join

def edits1(word)
  n = word.length
  deletion = (0...n).collect {|i| word[0...i]+word[i+1..-1] }
  transposition = (0...n-1).collect {|i| word[0...i]+word[i+1,1]+word[i,1]+word[i+2..-1] }
  alteration = []
  n.times {|i| LETTERS.each_byte {|l| alteration << word[0...i]+l.chr+word[i+1..-1] } }
  insertion = []
  (n+1).times {|i| LETTERS.each_byte {|l| insertion << word[0...i]+l.chr+word[i..-1] } }
  result = deletion + transposition + alteration + insertion
  result.empty? ? nil : result
end

def known_edits2(word)
  result = []
  edits1(word).each {|e1| edits1(e1).each {|e2| result << e2 if $nwords.has_key?(e2) }}
  result.empty? ? nil : result
end

def known(words)
  result = words.find_all {|w| $nwords.has_key?(w) }
  result.empty? ? nil : result
end

def correct(word)
  (known([word]) or known(edits1(word)) or known_edits2(word) or
    [word]).max {|a,b| $nwords[a] <=> $nwords[b] }
end

5.times do
  # $nwords is used to avoid constant reassignment warnings
  $nwords = train(words(File.new('holmes.txt').read))
  if correct("speling") == "spelling"
    puts "Spelling successfully corrected."
  else
    # raise is used so that the error appears in the reports
    raise "Failed to correct spelling."
  end
end
