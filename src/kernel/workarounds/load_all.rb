# Meta-file, to load all current workarounds in this directory.

Dir[File.expand_path("../*.rb", __FILE__)].each do |f|
  warn "Loading workaround #{File.basename(f)}"
  require f
end

