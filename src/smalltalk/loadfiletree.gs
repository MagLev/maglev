run
|repos|
repos := MCFileTreeRepository new directory: (FileDirectory on: '/home/pbm/GemStone/checkouts/maglev/src/packages').

Gofer new
    package: 'Maglev';
    repository: repos;
    load
%

