function gs_sh()
{
  typeset _command
  _command="$1"
  shift
  "$GEMSTONE/bin/${_command}" "$@" || return $?
}

function run_topaz()
{
  typeset IFS
  IFS=$'\n'
  printf "$*\n" | gs_sh topaz || return $?
}

function run_on_stone()
{
  typeset _username _password
  _username="DataCurator"
  _password="swordfish"

  typeset -a _commands
  _commands=(
    "output append ${MAGLEV_HOME}/log/maglev/topaz.log"
    "set u ${username} p ${password} gemstone ${STONENAME}"
    "login"
    "limit oops 100"
    "limit bytes 1000"
    "display oops"
    "iferr 1 stack"
    "iferr 2 exit 3"
    "$@" # inject given commands
    "output pop"
    "exit"
  )
  run_topaz "$@"
}

# Given the name of a ERB template, copy it to the destination dir,
# running it through erb.
function cp_template()
{
  erb_file="$1"
  dest_file="$2"
  shift 2
  [[ -f "$erb_file" ]] || {
    echo "template $erb_file missing"
    exit 1
  }
  sed \
    -e "s#<%= extent_filename %>#$GEMSTONE_DATADIR/extent/extent0.dbf#" \
    -e "s#<%= scratch_directory %>#$GEMSTONE_DATADIR/scratch.#" \
    -e "s#<%= tranlog_directories.join(\",\") %>#$GEMSTONE_DATADIR/tranlog,$GEMSTONE_DATADIR/tranlog#" \
    -e "s#<%= FILEIN_DIR %>#$FILEIN_DIR#" \
    -e "s#<%= KEYFILE %>#$MAGLEV_HOME/etc/maglev.demo.key#" \
    < erb_file > dest_file
}

function maglev_stone_create()
{
  echo "[Info] Creating new default '${STONENAME}' repository"
  mkdir -p "${MAGLEV_HOME}/etc/conf.d/"
  mkdir_p "${MAGLEV_HOME}/backups"
  cp_template "${MAGLEV_HOME}/rakelib/contrib/ottobehrens/stone.conf.template" "${MAGLEV_HOME}/etc/conf.d/${STONENAME}.conf"
  mkdir -p "${GEMSTONE_DATADIR}/extent"
  mkdir -p "${MAGLEV_HOME}/log/${STONENAME}"
  mkdir -p "${GEMSTONE_DATADIR}/tranlog"
  cp "${MAGLEV_HOME}/bin/extent0.ruby.dbf") "${GEMSTONE_DATADIR}/extent0.ruby.dbf"
  chmod 0660 "${GEMSTONE_DATADIR}/extent0.ruby.dbf"
}

function build_maglev_filein_env_setup()
{
  # upgradeDir is also needed by the filein topaz scripts.  When a
  # customers does a 'upgrade' it will be set to $GEMSTONE/upgrade.  For
  # a filein it will be set to the imageDir.
  #
  # The non-traditional camel case env variable names come from the
  # Smalltalk build.  We do not change them because we want to remain as
  # compatible as possible with the original SVN source.
  export FILEIN_DIR="$MAGLEV_HOME/fileintmp"
  export BUILD_DIR="$MAGLEV_HOME/build"
  export upgradeDir="$GEMSTONE/upgrade"
  export imageDir="$MAGLEV_HOME/src/smalltalk"
  export dbfDir="$FILEIN_DIR"
  export imageRubyDir="$MAGLEV_HOME/src/smalltalk/ruby"
  export GS_DEBUG_COMPILE_TRACE=1
  export STONENAME="fileinrubystone"
  export GEMSTONE_SYS_CONF="$FILEIN_DIR/filein.ruby.conf"
  export GEMSTONE_EXE_CONF="$FILEIN_DIR/fileingem.ruby.conf"
}

function build_maglev_new_extent()
(
# ( = subprocess for new env variables scope
  build_maglev_filein_env_setup
  mkdir -p "${FILEIN_DIR}"
  cd "${FILEIN_DIR}"
  cp "$GEMSTONE/bin/extent0.dbf" "$FILEIN_DIR/extent0.ruby.dbf"
  chmod 0770 "$FILEIN_DIR/extent0.ruby.dbf"
  cp_template "${BUILD_DIR}/filein.ruby.conf.erb" "$GEMSTONE_SYS_CONF"
  cp "${BUILD_DIR}/fileingem.ruby.conf" "$GEMSTONE_EXE_CONF"
)

function build_maglev_start_stone()
{
  {
    gs_sh startstone ${STONENAME} -l "$FILEIN_DIR/stone.log" -e "$FILEIN_DIR/filein.ruby.conf" -z "$FILEIN_DIR/filein.ruby.conf"
    gs_sh waitstone ${STONENAME}
  } > "$FILEIN_DIR/startstone.log" 2>&1
}

function build_maglev_stop_stone()
{
  {
    gs_sh stopstone ${STONENAME} DataCurator swordfish
  } > "$FILEIN_DIR/stopstone.log" 2>&1
}

function build_maglev_fileinruby()
{
  typeset -a _commands
  typeset outfile
  outfile="${FILEIN_DIR}/fileinruby.out"
  _commands=(
    "output push ${outfile} only"
    "set gemstone ${STONE_NAME}"
    "input $imageDir/fileinruby.topaz"
    "output pop"
    "exit"
  )
  run_topaz "${_commands[@]}"
}

function build_maglev_load_file_tree_dir()
{
  typeset -a _commands
  typeset outfile
  outfile= "${FILEIN_DIR}/loadfiletree.out"
  _commands=(
    "output push ${outfile} only"
    "iferr 1 exit 3"
    "set gemstone ${STONE_NAME} user DataCurator pass swordfish"
    "login"
    "run"
    "|repos|"
    "repos := MCFileTreeRepository new directory: (FileDirectory on: '${MAGLEV_HOME}/src/packages')."
    "Gofer new"
    "    package: 'Maglev';"
    "    repository: repos;"
    "    load"
    "%"
    "expectvalue true"
    "commit"
    "logout"
    "exit 0"
  )
  run_topaz "${_commands[@]}"
}

function build_maglev_filein()
(
# ( = subprocess for new env variables scope
  build_maglev_filein_env_setup
  cd "${FILEIN_DIR}"
  build_maglev_start_stone && build_maglev_fileinruby
  build_maglev_stop_stone
)

function build_maglev_packages()
(
# ( = subprocess for new env variables scope
  build_maglev_filein_env_setup
  cd "${FILEIN_DIR}"
  build_maglev_start_stone && build_maglev_load_file_tree_dir
  build_maglev_stop_stone
)

function build_maglev()
{
  if
    build_maglev_new_extent &&
    build_maglev_filein &&
    build_maglev_packages
  then
    echo "[Info] Build Succeeded"
    if [[ -f "${MAGLEV_HOME}/bin/extent0.ruby.dbf" ]]
    then
      echo "[Info] Saving previous extent as ${MAGLEV_HOME}/bin/extent0.ruby.dbf.save"
      mv "${MAGLEV_HOME}/bin/extent0.ruby.dbf" "${MAGLEV_HOME}/bin/extent0.ruby.dbf.save"
    fi

    echo "[Info] Copying new extent to ${MAGLEV_HOME}/bin/extent0.ruby.dbf"
    cp "${MAGLEV_HOME}/fileintmp/extent0.ruby.dbf" "${MAGLEV_HOME}/bin/extent0.ruby.dbf"
    chmod 0444 "${MAGLEV_HOME}/bin/extent0.ruby.dbf"
  else
    echo "[Error] Build failed see ${BUILD_LOG}"
    return 1
  fi &&
  maglev_stone_create ||
  return $?
}
