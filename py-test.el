;;; py-test.el --- A test runner for Python code.

;; Copyright (C) 2014 Bogdan Paul Popa

;; Author: Bogdan Paul Popa <popa.bogdanp@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((dash "2.9.0") (f "0.17") (emacs "24"))
;; Keywords: python testing py.test
;; URL: https://github.com/Bogdanp/py-test.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
;; KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
;; WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Py-test gives you the ability to define testing projects and, based
;; on those projects, run a single test that's defined in the current
;; buffer, all of the tests that you have defined in the current buffer,
;; or all of the tests that you have defined in the current buffers's
;; parent directory.

;;; Installation

;; From `MELPA':
;;
;; M-x package-install RET py-test RET

;; Manually:


;;; Usage

;; Define a project.
;;
;; (py-test/define-project
;;  :name "My Project"
;;  :python-command "python"
;;  :base-directory (expand-file-name "~/sandbox/my-project-home/")
;;  :test-runner (expand-file-name "~/sandbox/my-project-home/tests/runner.py")
;;  :working-directory (expand-file-name "~/sandbox/my-project-home/tests/"))

;; Open a file belonging to that project:
;;
;; (find-file "~/sandbox/my-project-home/tests/subfolder/test_something.py")

;; Run all of the tests that were defined in that file:
;;
;; M-x py-test/run-file RET

;; Run all of the tests that were defined in that file's parent directory:
;;
;; M-x py-test/run-folder RET

;; Jump to a single test function, method or class and run just that:
;;
;; M-x py-test/run-test-at-point RET

;;; Code:

(require 'dash)
(require 'f)

(defvar py-test/*default-buffer-name* "*py-test*"
  "The default name to use when creating a new compilation buffer.")

(defvar py-test/*default-test-runner* "py.test"
  "The test runner to use when one isn't provided by the project.")

(defvar py-test/*test-path-separator* "::"
  "The separator to use when generating paths to individual tests. In
py.test this is \"::\".")

(defvar py-test/*projects* nil
  "The list of projects.

This is a property list with the following properties:

`name'
  The project's name.

`base-directory'
  The project's base directory.

`python-command'
  The Python command to use when running the runner. May be nil if the
  test-runner is executable.

`test-runner'
  The path to the test runner to use. This can be nil, in which case
  `py-test/*default-test-runner*' will be used.

`test-runner-arguments'
  A list of command-line arguments that should always get passed to the
  runner.

`working-directory'
  The directory in which to run the tests. This can be nil, in which
  case the current buffer's CWD will be used.

`compilation-buffer-name'
  The name of the buffer to use when running `compile'. Defaults to
  `py-test/*default-buffer-name*'.")

(defun py-test/define-project (&rest args)
  "Define a new project with ARGS.

If the project already exists, update it."
  (let* ((project-name (plist-get args :name))
         (finder (lambda (project)
                   (string= (plist-get project :name) project-name)))
         (project (-first finder py-test/*projects*)))

    (when project
      ;; Remove the project so it can be re-added.
      (setq py-test/*projects* (-reject finder py-test/*projects*)))

    (push args py-test/*projects*)))

(defun py-test/project-for-filename (filename)
  "Find the first project whose base-directory is a parent of FILENAME."
  (let ((finder
         (lambda (project)
           (string-match (concat "^" (plist-get project :base-directory))
                         filename))))
    (-first finder py-test/*projects*)))

(defun py-test/find-outer-test-class ()
  "Searches backward for the first class definition of the form 'class.*T.*('."
  (save-excursion
    (re-search-backward "^ *class +\\(Test[^(]*\\)" nil t)
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun py-test/find-outer-test ()
  "Searches backward for the current test."
  (save-excursion
    (re-search-backward "^\\( *\\)\\(class\\|def\\) +\\([Tt]est[^(]*\\)" nil t)
    (let* ((indentation (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
           (abstraction (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
           (name (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
           (is-method (> (length indentation) 0)))
      (if is-method
          (list (py-test/find-outer-test-class) name)
        (list name)))))

(defun py-test/run-project (project &rest args)
  "'Compiles' the runner for PROJECT with ARGS."
  (let* ((project-python-command (plist-get project :python-command))
         (project-test-runner (plist-get project :test-runner))
         (project-test-runner-arguments (plist-get project :test-runner-arguments))
         (project-working-directory (plist-get project :working-directory))
         (project-compilation-buffer-name (plist-get project :compilation-buffer-name))

         (python-command (or project-python-command ""))
         (test-runner (or project-test-runner py-test/*default-test-runner*))
         (command (list python-command test-runner))
         (default-directory (or project-working-directory default-directory))

         (compilation-buffer-name-function
          (lambda (_)
            (or project-compilation-buffer-name
                py-test/*default-buffer-name*))))

    (compile (string-join (append command project-test-runner-arguments args) " "))))


;;;###autoload
(defun py-test/run-folder ()
  "Run all the tests in the current folder."
  (interactive)
  (let* ((filename (buffer-file-name))
         (project (py-test/project-for-filename filename))
         (directory (f-dirname filename)))
    (py-test/run-project project directory)))

;;;###autoload
(defun py-test/run-file ()
  "Run all the tests in the current file."
  (interactive)
  (let* ((filename (buffer-file-name))
         (project (py-test/project-for-filename filename)))
    (py-test/run-project project filename)))

;;;###autoload
(defun py-test/run-test-at-point ()
  "Run the test at point."
  (interactive)
  (let* ((filename (buffer-file-name))
         (project (py-test/project-for-filename filename))
         (test-path (string-join (cons filename (py-test/find-outer-test))
                                 py-test/*test-path-separator*)))
    (py-test/run-project project test-path)))


(provide 'py-test)

;;; py-test.el ends here.
