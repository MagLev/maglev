fails:Kernel#eval evaluates such that consts are scoped to the class of the eval
fails:Kernel#eval updates a local in a scope above a surrounding block scope
fails:Kernel#eval accepts a Proc object as a binding
fails:Kernel#eval does not make Proc locals visible to evaluated code
fails:Kernel#eval allows a binding to be captured inside an eval
fails:Kernel#eval allows Proc and binding to be nested in horrible ways
fails:Kernel#eval allows creating a new class in a binding
fails:Kernel#eval includes file and line information in syntax error
fails:Kernel#eval uses the filename of the binding if none is provided
