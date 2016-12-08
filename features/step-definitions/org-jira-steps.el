;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(load "espuds")

(Given "^\\(?:I am in buffer\\|I switch to buffer\\) \"\\(.+\\)\"$"
  "Switches to BUFFER."
  (lambda (buffer)
    (switch-to-buffer buffer)))
