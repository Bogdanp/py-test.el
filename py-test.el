;;; py-test.el --- A test runner for Python code.

;; Copyright (C) 2014 Bogdan Paul Popa

;; Author: Bogdan Paul Popa <popa.bogdanp@gmail.com>
;; Version: 0.1.0
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

;;; Code:

(require 'dash)
(require 'f)

(defvar py-test/*projects* nil
  "The list of projects.

This is a property list with the following properties:

`name`
  The project's name.

`base-directory`
  The project's base directory.

`python-command`
  The Python command to use when running the runner. May be nil if the
  test-runner is executable.

`test-runner`
  The path to the test runner to use. This can be nil, in which case
  py.test will be used.

`working-directory`
  The directory in which to run the tests. This can be nil, in which
  case the current buffer's CWD will be used.")

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

(defun py-test/run-project (project &rest args)
  "'Compiles' the runner for PROJECT with ARGS."
  (let* ((project-python-command (plist-get project :python-command))
         (project-test-runner (plist-get project :test-runner))
         (project-working-directory (plist-get project :working-directory))

         (python-command (or project-python-command ""))
         (test-runner (or project-test-runner "py.test"))
         (command (list python-command test-runner))
         (default-directory (or project-working-directory default-directory)))

    (compile (string-join (append command args) " "))))

(defun py-test/run-folder ()
  "Run all the tests in the current folder."
  (interactive)
  (let* ((filename (buffer-file-name))
         (project (py-test/project-for-filename filename))
         (directory (f-dirname filename)))
    (py-test/run-project project directory)))

(defun py-test/run-file ()
  "Run all the tests in the current file."
  (interactive)
  (let* ((filename (buffer-file-name))
         (project (py-test/project-for-filename filename)))
    (py-test/run-project project filename)))


(provide 'py-test)

;;; py-test.el ends here.
