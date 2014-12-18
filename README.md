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

Testing
======
In the tests folder:
1) Run 'runtests.sh' to automate testing and output to log. This script compiles all files to javascript, runs them with node, and compares the output with an expected output.

2) Compile tests to javascript with 'js.sh'

3) Run Node on all js files with 'node.sh'

Dependencies
======
You need Node to run tests.
