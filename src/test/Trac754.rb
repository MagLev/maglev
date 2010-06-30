class C
  def try(method, *args, &block)
    send(method, *args, &block)
  end
  remove_method :try
  alias_method :try, :__send__

  def mysend(method, *args, &block)
    puts "mysend"
  end

  $aa = 0
  begin
    alias_method :__send__ , :mysend
  rescue NameError
    $aa = 8
  end
  unless $aa == 8 ; raise 'fail'; end
end
true
