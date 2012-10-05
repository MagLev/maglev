raise "Second argument to Kernel.system was dropped" unless system("make", "-v") || system("gmake", "-v")
