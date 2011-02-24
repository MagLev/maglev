def create_makefile(target, srcprefix = nil)
  for sfx, i in [["-default", [["lib/**/*.rb", "$(RUBYLIBDIR)", "lib"]]], ["", $INSTALLFILES]]
    files = install_files(mfile, i, nil, srcprefix) or next
    for dir, *files in files
      unless dirs.include?(dir)
        dirs << dir
        mfile.print "pre-install-rb#{sfx}: #{dir}\n"
      end
      files.each do |f|
        dest = "#{dir}/#{File.basename(f)}"
        mfile.print("install-rb#{sfx}: #{dest}\n")
        mfile.print("#{dest}: #{f} #{dir}\n\t$(#{$extout ? 'COPY' : 'INSTALL_DATA'}) ")
        sep = config_string('BUILD_FILE_SEPARATOR')
      end
    end
  end
ensure
  mfile.close if mfile
end
