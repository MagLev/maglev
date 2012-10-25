# brackets around Module.new {}, or removing either line doesn't produce a parser error
a = []
extend Module.new {
  a[0] = 1
  [].each do end
}
# passed if no parser error produced
