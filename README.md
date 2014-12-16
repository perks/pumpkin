pumpkin
=======

A patchwork functional programming language

Git Flow
======
1) Make sure you are using `git pull --rebase` to pull changes, fixing merge
conflicts before you push. To set this up automatically, do ```git config
branch.autosetuprebase always```

2) No branch/pull requests, since we are doing rebase on master, master should
always be up to date and free of merge conflits

Task Sign Off
======

Task                | Parser  | Analyzer | CodeGen | Comment |
------------------- | ------- | -------- | ------- | ------- |
Comments            | quinton | NA       | NA      | |
Numerical           | quinton | | chris |:heavy_check_mark: |
Boolean             | quinton | | chris | :heavy_check_mark:|
Unit                | quinton | | chris | :heavy_check_mark:|
Char                | quinton | | chris | :heavy_check_mark:|
Strings             | quinton | | chris |:heavy_check_mark: |
Lists               | quinton | | | :heavy_check_mark: |
Tupals              | quinton | | | waiting on frontend, will use object prototypes |
Map                 | quinton | | | will use standard js dicts|
Variables           | quinton | | chris | :heavy_check_mark: |
Control Flow        | quinton | | chris | |
Functions           | quinton | | chris | |
Piping              | quinton | | | |
Composition         | quinton | | | |
Scoping/Indentation | quinton | | chris | |
