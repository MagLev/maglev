set user DataCurator pass swordfish
login
expectvalue %GoferLoad
run
|repos|
repos := MCFileTreeRepository new directory: 
(FileDirectory on: (System gemEnvironmentVariable:'MAGLEV_HOME') , '/src/packages').
Gofer new
      package: 'Maglev';
      repository: repos;
      load
%
expectvalue true
commit
logout
