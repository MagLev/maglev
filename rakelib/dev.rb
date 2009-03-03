# Ruby support for the rake tasks in dev.rake.  Support for managing the
# development environment.

# Remove the currently installed development image.
# Saves $MAGLEV_HOME/gemstone as $MAGLEV_HOME.gemstone.old.  Deletes
# $MAGLEV_HOME/product, if it exists.
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

# Unpack the given +tgz+ file into place as $MAGLEV_HOME/gemstone Assumes
# the .tgz file unpacks a directory named +product+, renames it to
# +gemstone+.
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
      cat version.txt
      }
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
    cp 'gemstone/bin/extent0.ruby.dbf', 'data'
    chmod 0644, 'data/extent0.ruby.dbf'
  end
end

# ######################################################################
#                           TOPAZ COMMAND STRINGS
# ######################################################################
# The following +tc_*+ methods generate topaz command strings based on the
# parameters passed to them.

# Returns a topaz command string that reloads the primitives
# (src/kernel/kernel.rb) and commits the DB.
def tc_reload_prims
  <<-END.margin
    |omit resultcheck
    |run
    |RubyContext reset ; load "includes save, commit"
    |%
    |exit
  END
end

def tc_ensure_prims
  <<-END.margin
    |omit resultcheck
    |run
    |RubyContext ensurePrimsLoaded .
    |%
    |exit
  END
end


# Returns a topaz command string that runs the set of passing vm tests
# in src/test/vmunit.conf
def tc_run_vmunit
  <<-END.margin
    |omit resultcheck
    |run
    |RubyContext _runVmUnit
    |%
    |exit
  END
end

# Returns a topaz command string that runs the benchmarks
def tc_run_benchmarks
  <<-END.margin
    |inp #{"rakelib/allbench.inp"}
  END
end

# Returns a topaz command string that loads ../latest.mcz and commits the DB.
def tc_load_mcz
  <<-END.margin
    |output push loadmcz.out
    |display resultcheck
    |run
    || fileRepo aName ver |
    |fileRepo := MCDirectoryRepository new directory: (FileDirectory on: '../').
    |aName := 'latest.mcz'.
    |
    |ver := fileRepo loadVersionFromFileNamed: aName .
    |ver class == MCVersion ifFalse:[ aName error:'not found in repos' ].
    |GsFile gciLogServer: ver printString .
    |ver load .
    |GsFile gciLogServer: 'load done'.
    |^ true
    |%
    |commit
    |exit
  END
end

