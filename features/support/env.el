(require 'f)

(defvar org-jira-support-path
  (f-dirname load-file-name))

(defvar org-jira-features-path
  (f-parent org-jira-support-path))

(defvar org-jira-root-path
  (f-parent org-jira-features-path))

(add-to-list 'load-path org-jira-root-path)

(require 'org-jira)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
