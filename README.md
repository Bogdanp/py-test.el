# py-test

A simple test runner for Python code.

# Setup

Clone the repo.

```sh
git clone https://github.com/Bogdanp/py-test.el ~/py-test.el
```

Add `py-test` to your `.emacs`.

```lisp
(add-to-list 'load-path (expand-file-name "~/py-test.el"))
(require 'py-test)
```

Define a project.

```lisp
(py-test/define-project
 :name "My Project"
 :basedir (expand-file-name "~/sandbox/my-project-home/")
 :runner (expand-file-name "~/sandbox/my-project-home/tests/runner.py")
 :cwd (expand-file-name "~/sandbox/my-project-home/tests/"))
```

You're done! You can now use `M-x py-test/run-file RET` and
`M-x py-test/run-folder RET` to run your tests. Read the manual for
`compilation-mode` to find out how to navigate through failed tests.
