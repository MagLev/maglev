flags = File::CREAT | File::TRUNC | File::WRONLY
f = File.new("sssss", flags)

# Clean up file
File.delete("sssss")

