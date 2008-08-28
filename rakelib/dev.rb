# Some other ideas: Cache images and reload them (e.g., after you've set
# them up with something).

def rm_current_gemstone
  cd MAGLEV_HOME do
    # The raw products tend to have the directories write protected, so
    # in order to delete any stray ones, we need to first chmod them so
    # we can delete them
    if File.directory?("product")
      puts "=== Removing #{MAGLEV_HOME}/product."
      chmod_R  0700, "product"
      rm_rf "product"
    end

    # Save one previous version of the product
    prev_gemstone = "gemstone.old"
    if File.directory?(prev_gemstone)
      puts "=== Removing previous gemstone: #{MAGLEV_HOME}/#{prev_gemstone}."
      rm_rf prev_gemstone
    else
      puts "=== No previous gemstone directory."
    end

    if File.directory?(GEMSTONE)
      puts "=== Saving current gemstone to #{prev_gemstone}."
      mv("gemstone", prev_gemstone)
    else
      puts "=== No current gemstone directory."
    end
  end
end

def untar_product_to_gemstone(tgz)
  product_tgz = File.expand_path(tgz)
  raise ArgumentError, "'#{product_tgz}' is not a file" unless
    File.exists?(product_tgz)

  cd MAGLEV_HOME do
    puts "=== untar #{product_tgz} to #{MAGLEV_HOME}/gemstone"
    # Have to escape to sh for untaring and passing "u+w" to chmod
    sh %{
      tar zxf ../latest-product.tgz
      chmod -R u+w product
      mv product gemstone
      echo "=== Unpacked build: version.txt is:"
      cat gemstone/version.txt
      }, :verbose => false
  end
end

def ensure_std_directories
  cd MAGLEV_HOME do
    puts "=== Ensure standard directories exist (data, locks, log)."
    %w(data locks log).each { |dir| Dir.mkdir dir unless File.directory? dir }
  end
end


def copy_extent
  puts "=== Copy new ruby extent into data"
  cd MAGLEV_HOME do

    # TODO: Just add these to a clobber target and nuke them...
    old_files = FileList.new("data/tranlog*", "data/extent0.ruby.dbf")
    if ! old_files.empty?
      puts "=== Removing old old_files #{old_files.join(' ')}"
      rm_f old_files
    end
    raise "No data directory" unless File.directory? 'data'
    puts "=== Copy new ruby extent."
    install 'gemstone/bin/extent0.ruby.dbf', 'data', 0644
  end
end

def load_mcz(file_name)
  warn "=== Not loading mcz files...requires user name and password for monticello repository..."
end

def run_topaz(snippet)
  sh %{ #{TOPAZ_CMD} <<EOF
#{snippet}
EOF
  }
end
