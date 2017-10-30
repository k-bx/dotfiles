(require 'projectile)

(defun python-get-runcmd-for-current-unit-test ()
  "Runs current python unit-test."
  (interactive)

  (defun get-test-name ()
    (setq before-everything (point))

    (setq def-start (search-backward "def "))
    (forward-char (length "def "))
    (setq name-start (point))
    (setq name-end (- (search-forward "(") 1))
    (setq rv (buffer-substring name-start name-end))

    (goto-char before-everything)

    rv)

  (defun get-class-name ()
    (setq before-everything (point))

    (setq def-start (search-backward "class "))
    (forward-char (length "class "))
    (setq name-start (point))
    (setq name-end (- (search-forward "(") 1))
    (setq rv (buffer-substring name-start name-end))

    (goto-char before-everything)

    rv)


  (defun get-test-path ()
    (setq without-dotpy (substring buffer-file-name 0 (- (length ".py"))))
    (setq splitted-path (split-string without-dotpy "/"))
    (setq magic-headoff (nthcdr python-test-cutoff-depth splitted-path))
    (setq joined (mapconcat 'identity magic-headoff "."))
    joined)

  (setq nose-path-to-test (concatenate 'string (get-test-path) ":" (get-class-name) "." (get-test-name)))
  (setq whole-command (concatenate 'string
                                   python-test-basecmd
                                   " "
                                   nose-path-to-test))
  (kill-new whole-command)

  whole-command)

(provide 'python-get-runcmd-for-current-unit-test)
