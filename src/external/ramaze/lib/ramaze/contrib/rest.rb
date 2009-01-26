Ramaze::Rewrite['REST dispatch'] = lambda do |path, request|
  path << '/' unless path[-1] == '/'

  method = if request.request_method == 'POST' and request.params.has_key?('method')
             request.params['method'].upcase
           else
             request.request_method
           end
  
  case method
  when 'GET' then path << 'show/'
  when 'POST' then path << 'create/'
  when 'PUT' then path << 'update/'
  when 'DELETE' then path << 'destroy/'
  else path
  end
end
