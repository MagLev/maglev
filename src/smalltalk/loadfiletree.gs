iferr 1 exit 3
set user DataCurator pass swordfish
login
run
|repos|
repos := MCFileTreeRepository new directory: (FileDirectory on: '$MAGLEV_HOME/src/packages').

Gofer new
      package: 'Maglev';
      repository: repos;
      load
%
expectvalue true
commit
