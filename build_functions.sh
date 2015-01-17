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
  printf "$*\n" | gs_sh topaz -l -I "${MAGLEV_HOME}/etc/.topazini" || return $?
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
    -e "s#<%= extent_filename %>#$GEMSTONE_DATADIR/extent/extent0.dbf#g" \
    -e "s#<%= scratch_directory %>#$GEMSTONE_DATADIR/scratch.#g" \
    -e "s#<%= tranlog_directories.join(\",\") %>#$GEMSTONE_DATADIR/tranlog,$GEMSTONE_DATADIR/tranlog#g" \
    -e "s#<%= FILEIN_DIR %>#$FILEIN_DIR#g" \
    -e "s#<%= KEYFILE %>#$MAGLEV_HOME/etc/maglev.demo.key#g" \
    < "$erb_file" > "$dest_file"
}

function build_maglev_stone_create()
{
  echo "[Info] Creating new default '${STONENAME}' repository"
  mkdir -p "${MAGLEV_HOME}/etc/conf.d/"
  mkdir_p "${MAGLEV_HOME}/backups"
  cp_template "${MAGLEV_HOME}/rakelib/contrib/ottobehrens/stone.conf.template" "${MAGLEV_HOME}/etc/conf.d/${STONENAME}.conf"
  mkdir -p "${GEMSTONE_DATADIR}/extent"
  mkdir -p "${MAGLEV_HOME}/log/${STONENAME}"
  mkdir -p "${GEMSTONE_DATADIR}/tranlog"
  cp -f "${MAGLEV_HOME}/bin/extent0.ruby.dbf" "${GEMSTONE_DATADIR}/extent0.ruby.dbf"
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
  \mkdir -p "${FILEIN_DIR}"
  \cd "${FILEIN_DIR}"
}

function build_maglev_new_extent()
(
# ( = subprocess for new env variables scope
  build_maglev_filein_env_setup
  cp -f "$GEMSTONE/bin/extent0.dbf" "$FILEIN_DIR/extent0.ruby.dbf"
  chmod 0770 "$FILEIN_DIR/extent0.ruby.dbf"
  cp_template "${BUILD_DIR}/filein.ruby.conf.erb" "$GEMSTONE_SYS_CONF"
  cp -f "${BUILD_DIR}/fileingem.ruby.conf" "$GEMSTONE_EXE_CONF"
)

function build_maglev_in_stone()
{
  typeset __return __max_log
  __max_log=50
  {
    gs_sh startstone ${STONENAME} -l "$FILEIN_DIR/stone.log" -e "$FILEIN_DIR/filein.ruby.conf" -z "$FILEIN_DIR/filein.ruby.conf" &&
    gs_sh waitstone ${STONENAME}
  } > "$FILEIN_DIR/startstone_$1.log" 2>&1 ||
  {
    typeset _return=$?
    echo "[ERROR] failed starting gemstone, last ${__max_log} lines of '$FILEIN_DIR/startstone_$1.log':"
    tail -n ${__max_log} "$FILEIN_DIR/startstone_$1.log"
    return ${_return}
  }
  {
    "$@"
  } > "$FILEIN_DIR/runstone_$1.log" 2>&1 ||
  {
    __return=$?
    echo "[ERROR] failed running '$*', last ${__max_log} lines of '$FILEIN_DIR/runstone_$1.log':"
    tail -n ${__max_log} "$FILEIN_DIR/runstone_$1.log"
    echo "return_status=${_return}" >> "$FILEIN_DIR/runstone_$1.log"
  }
  gs_sh stopstone ${STONENAME} DataCurator swordfish > "$FILEIN_DIR/stopstone_$1.log" 2>&1 ||
  {
    typeset _return=$?
    [[ -n "__return" ]] || __return=${_return}
    echo "[ERROR] failed stopping gemstone, last ${__max_log} lines of '$FILEIN_DIR/stopstone_$1.log':"
    tail -n ${__max_log} "$FILEIN_DIR/stopstone_$1.log"
  }
  return ${__return:-0}
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

function build_maglev_filein()
(
# ( = subprocess for new env variables scope
  build_maglev_filein_env_setup
  build_maglev_in_stone build_maglev_fileinruby
)

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

function build_maglev_packages()
(
# ( = subprocess for new env variables scope
  build_maglev_filein_env_setup
  build_maglev_in_stone build_maglev_load_file_tree_dir
)

function build_maglev_install_extent()
{
  echo "[Info] Build Succeeded"
  if [[ -f "${MAGLEV_HOME}/bin/extent0.ruby.dbf" ]]
  then
    echo "[Info] Saving previous extent as ${MAGLEV_HOME}/bin/extent0.ruby.dbf.save"
    mv "${MAGLEV_HOME}/bin/extent0.ruby.dbf" "${MAGLEV_HOME}/bin/extent0.ruby.dbf.save"
  fi

  echo "[Info] Copying new extent to ${MAGLEV_HOME}/bin/extent0.ruby.dbf"
  cp -f "${MAGLEV_HOME}/fileintmp/extent0.ruby.dbf" "${MAGLEV_HOME}/bin/extent0.ruby.dbf"
  chmod 0444 "${MAGLEV_HOME}/bin/extent0.ruby.dbf"
}

function build_maglev()
{
  typeset _type _return
  for _type in new_extent filein packages install_extent stone_create
  do
    echo "[INFO] maglev build ${_type//_/ }"
    build_maglev_${_type} || {
      _return=$?
      echo "[Error] Build failed for build_maglev_${_type} with return code ${_return}."
      return ${_return}
    }
  done
}
