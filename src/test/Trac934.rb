puts "Exit A"
true        # overridden in at_exit handler
at_exit {
  puts "Exit B"
  exit_code = nil
  at_exit {  
    puts "Exit C"
    exit false if exit_code && exit_code != 0 
  }
  exit_code = 123
}

