;;; org-jira.el --- Syncing between Jira and Org-mode.

;; Authors:
;; Bertrand Lallau <bertrand.lallau@gmail.com>
;; Bao Haojun <baohaojun@gmail.com>
;;
;; Version: 0.1
;; Homepage: https://github.com/blallau/org-jira

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;;
;; This provides an extension to org-mode for syncing issues with JIRA
;; issue servers.
;;
;;; Code:

(eval-when-compile (load-library "cl-extra"))
(require 'org)
(require 'jiralib)
(require 'cl-lib)
(require 'cl-macs)

(defgroup org-jira nil
  "Customisation group for org-jira."
  :tag "Org JIRA"
  :group 'org)

(defvar org-jira-working-dir "~/.org-jira"
  "Folder under which to store org-jira working files.")

(defcustom org-jira-default-jql
  "assignee = currentUser() and resolution = unresolved ORDER BY
  priority DESC, created ASC"
  "Default jql for querying your Jira tickets."
  :group 'org-jira
  :type 'string)

(defcustom org-jira-done-states
  '("Closed" "Resolved" "Done")
  "Jira states that should be considered as DONE for `org-mode'."
  :group 'org-jira
  :type '(repeat (string :tag "Jira state name:")))

(defvar jira-users (list (cons "Full Name" "username"))
  "Jira has not api for discovering all users, so we should provide it somewhere else.")

(defcustom org-jira-default-issue-type "Bug"
  "Default issuetype for new issues"
  :group 'org-jira)
(defcustom org-jira-default-priority "Major"
  "Default issuetype for new issues"
  :group 'org-jira)
(defcustom org-jira-use-status-as-todo nil
  "Use the JIRA status as the TODO tag value."
  :group 'org-jira)
(defcustom org-jira-default-assignee nil
  "Default assignee for new tickets"
  :group 'org-jira)

(defvar org-jira-mode-hook nil
  "Hook to run upon entry into mode.")

(defvar org-jira-issue-id-history '()
  "Prompt history for issue id.")

(defvar org-jira-tag "JIRA"
  "Org mode tag for jira issues")

(defmacro ensure-on-issue (&rest body)
  "Make sure we are on an issue heading, before executing BODY."
  `(save-excursion
     (save-restriction
       (while (org-up-heading-safe)) ; go to the top heading
       (let ((org-jira-id (org-jira-id)))
         (unless (and org-jira-id (string-match (jiralib-get-issue-regexp) org-jira-id))
           (error "Not on a issue region!")))
       ,@body)))

(defmacro ensure-on-issue-id (issue-id &rest body)
  "Make sure we are on an issue heading with id ISSUE-ID, before executing BODY."
  (declare (indent 1))
  `(save-excursion
     (save-restriction
       (widen)
       ;;(outline-show-all)
       (goto-char (point-min))
       (let (p)
         (setq p (org-find-entry-with-id ,issue-id))
         (unless p
           (error "Issue %s not found!" ,issue-id))
         (goto-char p)
         (org-narrow-to-subtree)
         ,@body))))

(defmacro ensure-on-todo (&rest body)
  "Make sure we are on an todo heading, before executing BODY."
  `(save-excursion
     (save-restriction
       (let ((continue t)
             (on-todo nil))
         (while continue
           (when (org-get-todo-state)
             (setq continue nil on-todo t))
           (unless (and continue (org-up-heading-safe))
             (setq continue nil)))
         (if (not on-todo)
             (error "TODO not found")
           (org-narrow-to-subtree)
           ,@body)))))

(defmacro ensure-on-comment (&rest body)
  "Make sure we are on a comment heading, before executing BODY."
  `(save-excursion
     (org-back-to-heading)
     (forward-thing 'whitespace)
     (unless (looking-at "Comment:")
       (error "Not on a comment region!"))
     (save-restriction
       (org-narrow-to-subtree)
       ,@body)))

(defmacro ensure-on-worklog (&rest body)
  "Make sure we are on a worklog heading, before executing BODY."
  `(save-excursion
     (org-back-to-heading)
     (forward-thing 'whitespace)
     (unless (looking-at "Worklog:")
       (error "Not on a worklog region!"))
     (save-restriction
       (org-narrow-to-subtree)
       ,@body)))

(defun hash (dlist dhash)
  "Convert a list to hash and return that hash."
  (progn
    (new-list-to-hash dlist dhash)
    dhash))

(defun new-list-to-hash (dlist dhash)
  "Given a nested list, convert it into a hash-table."
  (mapc (lambda (x)
          (puthash (car x)
                   (if (and (equal 'cons (type-of (cdr x)))
                            (not (equal 'string (type-of (car (cdr x))))))
                       (hash (car (cdr x)) (make-hash-table :test 'equal))
                     (cdr x))
                   dhash))
        dlist))

(defvar org-jira-entry-mode-map
  (let ((org-jira-map (make-sparse-keymap)))
    (define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
    (define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
    (define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
    (define-key org-jira-map (kbd "C-c ia") 'org-jira-get-issue-in-current-buffer)
    (define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
    (define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-from-filter-headonly)
    (define-key org-jira-map (kbd "C-c iF") 'org-jira-get-issues-from-filter)
    (define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
    (define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
    (define-key org-jira-map (kbd "C-c ir") 'org-jira-refresh-issue)
    (define-key org-jira-map (kbd "C-c ic") 'org-jira-create-issue)
    (define-key org-jira-map (kbd "C-c ik") 'org-jira-copy-current-issue-key)
    (define-key org-jira-map (kbd "C-c sc") 'org-jira-create-subtask)
    (define-key org-jira-map (kbd "C-c sg") 'org-jira-get-subtasks)
    (define-key org-jira-map (kbd "C-c cu") 'org-jira-update-comment)
    (define-key org-jira-map (kbd "C-c wu") 'org-jira-update-worklog)
    (define-key org-jira-map (kbd "C-c tj") 'org-jira-todo-to-jira)
    (define-key org-jira-map (kbd "C-c wa") 'org-jira-add-blank-worklog)
    (define-key org-jira-map (kbd "C-c ii") 'org-jira-clock-in)
    (define-key org-jira-map (kbd "C-c id") 'org-jira-done)
    org-jira-map))


;;;###autoload
(define-minor-mode org-jira-mode
  "Toggle org-jira mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org-jira-entry-mode-map}

Entry to this mode calls the value of `org-jira-mode-hook'."

  :init-value nil
  :lighter " jira"
  :group 'org-jira
  :keymap org-jira-entry-mode-map

  (if org-jira-mode
      (run-mode-hooks 'org-jira-mode-hook)))

(defun org-jira-get-project-name (proj)
  (org-jira-find-value proj 'name))

(defun org-jira-find-value (l &rest keys)
  (let* (key exists)
    (while (and keys (listp l))
      (setq key (car keys))
      (setq exists nil)
      (catch 'exitmap
        (mapc (lambda (item)
                (when (equal key (car item))
                  (setq exists t)
                  (throw 'exitmap item)))
              (if (and (listp l)
                       (listp (car l)))
                  l
                nil)))
      (setq keys (cdr keys))
      (if exists
          (setq l (cdr (assoc key l)))
        (setq l (or (cdr (assoc key l)) l))))
    (if exists
        l
      nil)))

(defun org-jira-get-project-lead (proj)
  (org-jira-find-value proj 'lead 'name))

;;;###autoload
(defun org-jira-get-projects ()
  "Get list of projects."
  (interactive)
  (let ((projects-file (expand-file-name "projects-list.org" org-jira-working-dir)))
    (or (find-buffer-visiting projects-file)
        (find-file projects-file))
    (org-jira-mode t)
    (save-excursion
      (let* ((oj-projs (jiralib-get-projects-details)))
        (mapc (lambda (proj)
                (let* ((proj-key (org-jira-find-value proj 'key))
                       (proj-headline (format "Project: [[file:%s.org][%s]]" proj-key proj-key)))
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    ;;(outline-show-all)
                    (let ((p (org-find-exact-headline-in-buffer proj-headline)))
                      (if (and p (>= p (point-min))
                               (<= p (point-max)))
                          (progn
                            (goto-char p)
                            (org-narrow-to-subtree)
                            (end-of-line))
                        (goto-char (point-max))
                        (insert "* ")
                        (insert proj-headline)
                        (newline)
                        (org-narrow-to-subtree)))
                    (org-entry-put (point) "name" (org-jira-get-project-name proj))
                    (org-entry-put (point) "key" (org-jira-find-value proj 'key))
                    (org-entry-put (point) "lead" (org-jira-get-project-lead proj))
                    (org-entry-put (point) "ID" (org-jira-find-value proj 'id))
                    (org-entry-put (point) "url" ( (org-jira-find-value proj 'key))))))
              oj-projs)))))

(defun org-jira-get-issue-components (issue)
  "Return the components the ISSUE belongs to."
  (mapconcat (lambda (comp) (org-jira-find-value comp 'name)) (org-jira-find-value issue 'fields 'components) ", "))

(defun org-jira-transform-time-format (jira-time-str)
  "Convert JIRA-TIME-STR to format \"%Y-%m-%d %T\".

Example: \"2012-01-09T08:59:15.000Z\" becomes \"2012-01-09
16:59:15\", with the current timezone being +0800."
  (condition-case ()
      (format-time-string "%Y-%m-%d %T"
                          (apply
                           'encode-time
                           (parse-time-string (replace-regexp-in-string "T\\|\\.000" " " jira-time-str))))
    (error jira-time-str)))

(defun org-jira--fix-encode-time-args (arg)
  "Fix ARG for 3 nil values at the head."
  (loop
   for n from 0 to 2 by 1 do
   (when (not (nth n arg))
     (setcar (nthcdr n arg) 0)))
  arg)

(defun org-jira-time-format-to-jira (org-time-str)
  "Convert ORG-TIME-STR back to jira time format."
  (condition-case ()
      (format-time-string "%Y-%m-%dT%T.000Z"
                          (apply 'encode-time
                                 (org-jira--fix-encode-time-args (parse-time-string org-time-str))) t)
    (error org-time-str)))

(defun org-jira-get-comment-val (key comment)
  "Return the value associated with KEY of COMMENT."
  (org-jira-get-issue-val key comment))

(defun org-jira-get-worklog-val (key WORKLOG)
  "Return the value associated with KEY of WORKLOG."
  (org-jira-get-comment-val key WORKLOG))

(defun org-jira-get-issue-val (key issue)
  "Return the value associated with key KEY of issue ISSUE."
  (let ((tmp  (or (org-jira-find-value issue 'fields key 'key) "")))
    (unless (stringp tmp)
      (setq tmp (or (org-jira-find-value issue key) "")))
    (unless (stringp tmp)
      (setq tmp ""))
    (cond ((eq key 'components)
           (org-jira-get-issue-components issue))
          ((member key '(created updated startDate))
           (org-jira-transform-time-format tmp))
          ((eq key 'status)
           (org-jira-find-value issue 'fields 'status 'statusCategory 'name))
          ((eq key 'resolution) tmp)
          ((eq key 'type)
           (org-jira-find-value issue 'fields 'issuetype 'name))
          ((eq key 'priority)
           (org-jira-find-value issue 'fields 'priority 'name))
          ((eq key 'assignee)
           (org-jira-find-value issue 'fields 'assignee 'name))
          ((eq key 'description)
           (org-jira-strip-string tmp))
          (t
           tmp))))

(defvar org-jira-jql-history nil)
(defun org-jira-get-issue-list ()
  "Get list of issues, using jql (jira query language).

Default is unresolved issues assigned to current login user; with
a prefix argument you are given the chance to enter your own
jql."
  (let ((jql org-jira-default-jql))
    (when current-prefix-arg
      (setq jql (read-string "Jql: "
                             (if org-jira-jql-history
                                 (car org-jira-jql-history)
                               "assignee = currentUser() and resolution = unresolved")
                             'org-jira-jql-history
                             "assignee = currentUser() and resolution = unresolved")))
    (list (jiralib-do-jql-search jql))))

(defun org-jira-get-issue-by-id (id)
  "Get an issue by its ID."
  (interactive (list (read-string "Issue ID: " nil 'org-jira-issue-id-history)))
  (push id org-jira-issue-id-history)
  (let ((jql (format "id = %s" id)))
    (car (list (jiralib-do-jql-search jql))))) ;; because it is calling get-issue non-interactively
;;we should send the parameter

;;;###autoload
(defun org-jira-get-issues-headonly (issues)
  "Get list of ISSUES, head only.

The default behavior is to return issues assigned to you and unresolved.

With a prefix argument, allow you to customize the jql.  See
`org-jira-get-issue-list'."

  (interactive
   (org-jira-get-issue-list))

  (let* ((issues-file (expand-file-name "issues-headonly.org" org-jira-working-dir))
         (issues-headonly-buffer (or (find-buffer-visiting issues-file)
                                     (find-file issues-file))))
    (with-current-buffer issues-headonly-buffer
      (widen)
      (delete-region (point-min) (point-max))

      (mapc (lambda (issue)
              (let* ((issue-id (cdr (assoc 'key issue)))
                     (fields (cdr(assoc 'fields issue)))
                     (issue-summary (cdr (assoc 'summary fields))))
                (insert (format "- [jira:%s] %s\n" issue-id issue-summary))))
            (append (cdr (assoc 'issues issues)) nil)))
    (switch-to-buffer issues-headonly-buffer)))

;;;###autoload
(defun org-jira-get-issue ()
  "Get a JIRA issue, allowing you to enter the issue-id first."
  (interactive)
  (org-jira-get-issues (call-interactively 'org-jira-get-issue-by-id)))

(defun org-jira-get-issue-in-current-buffer ()
  "Get a JIRA issue, allowing you to enter the issue-id first. Insert
  it in the current buffer instead of the project buffer"
  (interactive)
  (org-jira-get-issues (call-interactively 'org-jira-get-issue-by-id) t))

;;;###autoload
(defun org-jira-get-issue-project (issue)
  (org-jira-find-value issue 'fields 'project 'name))

(defun org-jira-get-issue-key (issue)
  (org-jira-find-value issue 'key))

(defun org-jira-get-issue-summary (issue)
  (org-jira-find-value issue 'fields 'summary))


;;;###autoload
(defun org-jira-get-issues (issues &optional insert-in-current-buffer)
  "Get list of ISSUES into an org buffer.

Default is get unfinished issues assigned to you, but you can
customize jql with a prefix argument.
See`org-jira-get-issue-list'"

  (interactive
   (org-jira-get-issue-list))
  (org-jira--get-issues issues insert-in-current-buffer))


(defun org-jira--get-issues (issues &optional insert-in-current-buffer)
  (let (project-buffer)
    (setq project-buffer (current-buffer))
    ;;(message project-buffer)
    (mapc (lambda (issue)
            (let* ((issue-id (cdr (assoc 'key issue)))
                   (fields (cdr(assoc 'fields issue)))
                   (proj-key (cdr (assoc 'key (assoc 'project fields)))))
              (let ((project-file (expand-file-name (concat proj-key ".org") org-jira-working-dir)))
                (unless insert-in-current-buffer (setq project-buffer (or (find-buffer-visiting project-file)
                                                                          (find-file project-file))))
                (with-current-buffer project-buffer
                  (org-jira-write-issue issue-id fields insert-in-current-buffer)
                  (delete-trailing-whitespace)
                  (save-buffer)))))
          issues) ;;(cdr issues)) why on earth do you need to cdr?
    (switch-to-buffer project-buffer)))

(defun org-jira-write-issue (issue-id fields &optional at-point)

  (message "Write Issue: %s" issue-id)
  (message "%s" (pp fields))

  (let* ((issue-summary (cdr (assoc 'summary fields))))
    (org-jira-mode t)
    (widen)
    (save-restriction
      (if at-point
          (org-jira-insert-issue-header (org-jira-get-issue-val 'status fields)
                                        issue-summary
                                        (org-jira-get-issue-val 'project fields))
        (progn (goto-char (point-min))
               ;;(outline-show-all)
               (org-jira-write-issue-header (org-find-entry-with-id issue-id)
                                            (org-jira-get-issue-val 'status fields)
                                            issue-summary
                                            (org-jira-get-issue-val 'project fields)))))

    ;; Set up issue Properties
    (mapc (lambda (entry)
            (let ((val (org-jira-get-issue-val entry fields)))
              (message "   property %s --> %s" entry val)
              (when (and val (not (string= val "")))
                ;; special case with 'priority
                ;; 'priority is included in org-special-properties
                ;; SO IT CAN'T BE USED AS ORG-ENTRY
                (cond
                 ((eq entry 'priority)
                  (org-entry-put (point) "prio" val))
                 (t
                  (org-entry-put (point) (symbol-name entry) val))))))
          '(assignee reporter issuetype priority resolution status components created updated))
    (org-entry-put (point) "ID" issue-id)

    (org-jira-write-issue-headings '(description))
    (newline)

    (org-jira-update-comments-for-current-issue)
    (newline)
    (org-jira-update-worklogs-for-current-issue)
    (newline))

  (if (not (= (length (cdr (assoc 'subtasks fields))) 0))
      "(save-restriction
        (mapc 'org-jira-handle-subtask (cdr (assoc 'subtasks fields))))"))

(defun org-jira-handle-subtask (issue)
  (let* ((subtask-id (cdr (assoc 'key issue)))
         (subtask-fields (cdr (assoc 'fields issue)))
         (subtask-point (org-find-entry-with-id subtask-id))
         (parent-point (point))
         (subtask-summary (cdr (assoc 'summary subtask-fields))))
    (if subtask-point
        (progn
          (goto-char subtask-point)
          (ignore-errors (org-refile nil nil (list nil (buffer-file-name) nil parent-point))))
      (org-jira-write-issue-header (org-find-entry-with-id subtask-id)
                                   (org-jira-get-issue-val 'status subtask-fields)
                                   subtask-summary
                                   (org-jira-get-issue-val 'project subtask-fields)))))

(defun org-jira-insert-issue-header (status issue-headline project)
  (unless (= (point) (point-max))
    (save-excursion
      (let* ((current-point (point))
             (do-not-move (if (= (line-beginning-position) current-point)
                              (progn (outline-next-heading)
                                     (outline-previous-heading)
                                     (if (= current-point (point)) 't nil))
                            nil)))
        (unless do-not-move (outline-next-heading)))))
  (save-excursion
    (insert "* ")
    (insert (concat
             (cond (org-jira-use-status-as-todo
                    (upcase (replace-regexp-in-string " " "-" status)))
                   ((member status org-jira-done-states) "DONE")
                   ("TODO"))
             " [" issue-id "] " issue-headline))

    (insert "\n"))
  (org-set-tags-to
   (list org-jira-tag (replace-regexp-in-string "-" "_" project))))

(defun org-jira-write-issue-header (issue-point status issue-headline project)
  (if (and issue-point (>= issue-point (point-min))
           (<= issue-point (point-max)))
      (progn
        (goto-char issue-point)
        (forward-thing 'whitespace)
        (kill-line))
    (progn (goto-char (point-max))
           (insert "* ")))
  (insert (concat (cond (org-jira-use-status-as-todo
                         (upcase (replace-regexp-in-string " " "-" status)))
                        ((member status org-jira-done-states) "DONE")
                        ("TODO")) " [" issue-id "] "
                        issue-headline))

  (save-excursion
    (unless (search-forward "\n" (point-max) 1)
      (newline)))
  (org-narrow-to-subtree)
  (org-change-tag-in-region
   (point-min)
   (save-excursion
     (forward-line 1)
     (point))
   (replace-regexp-in-string "-" "_" (concat org-jira-tag ":" project))
   nil))

(defun org-jira-write-issue-headings (headings)
  (mapc (lambda (heading-entry)
          (ensure-on-issue-id
              issue-id
            (let* ((entry-heading (concat
                                   (symbol-name heading-entry)
                                   (format ": [[%s][%s]]"
                                           (jiralib-get-issue-url issue-id)
                                           issue-id))))
              (setq p (org-find-exact-headline-in-buffer entry-heading))
              (if (and p (>= p (point-min))
                       (<= p (point-max)))
                  (progn
                    (goto-char p)
                    (org-narrow-to-subtree)
                    (goto-char (point-min))
                    (forward-line 1)
                    (delete-region (point) (point-max)))
                (if (org-goto-first-child)
                    (org-insert-heading)
                  (goto-char (point-max))
                  (org-insert-subheading t))
                (insert entry-heading "\n"))

              (insert (replace-regexp-in-string "^" "  " (org-jira-get-issue-val heading-entry fields))))))
        headings))

;;;###autoload
(defun org-jira-update-comment ()
  "Update a comment for the current issue."
  (interactive)
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
         (comment-id (org-jira-get-from-org 'comment 'id))
         (comment (replace-regexp-in-string "^  " "" (org-jira-get-comment-body comment-id))))
    (if comment-id
        (jiralib-edit-comment issue-id comment-id comment)
      (jiralib-add-comment issue-id comment)
      (org-jira-delete-current-comment)
      (org-jira-update-comments-for-current-issue))))

(defun org-jira-add-blank-worklog ()
  "Update a worklog for the current issue."
  (interactive)
  (org-end-of-subtree)
  (insert "\n** worklog: \n")
  (org-set-property "comment" "")
)

(defun org-clock-start-time-current-item ()
  "the start time for current subtree."
  (interactive)
  (org-with-silent-modifications
   (let* ((re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
		      org-clock-string
		      "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
	  ts
	  ts-epoch)
     (save-excursion
       (save-restriction
       (org-narrow-to-subtree)
       (goto-char (point-max))
       (while (re-search-backward re nil t)
         (cond
          ((match-end 2)
           ;; Two time stamps
           (setq current-ts (match-string 2)
                 current-ts (apply 'encode-time (org-parse-time-string current-ts))
                 current-ts-epoch (org-float-time current-ts))
           (when (or (not ts-epoch)
                     (> ts-epoch current-ts-epoch))
                 (setq ts-epoch current-ts-epoch
                       ts current-ts))
           )))
       ts ;;return the minimum start time
       )))))

(defun org-jira-update-worklog ()
  "Update a worklog for the current issue."
  (interactive)
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
         (worklog-id (org-jira-get-from-org 'worklog 'id))
         (already_submitted (org-jira-get-from-org 'worklog 'submitted_on))
         ;;(timeSpent (org-jira-get-from-org 'worklog 'timeSpent))
         (timeSpent (number-to-string (org-clock-sum-current-item)))
         (timeSpent (if timeSpent
                        timeSpent
                      (read-string "Input the time you spent (such as 3w 1d 2h): ")))
         (timeSpent (replace-regexp-in-string " \\(\\sw\\)\\sw*\\(,\\|$\\)" "\\1" timeSpent))
         (startDate (format-time-string "%Y-%m-%dT%T.000+0000" (org-clock-start-time-current-item)))
         ;;         (startDate (org-jira-get-from-org 'worklog 'startDate))
         (startDate (if startDate
                        startDate
                      (org-jira-time-format-to-jira (org-read-date nil nil nil "Input when did you start"))))
         (comment (replace-regexp-in-string "^  " "" (org-jira-get-from-org 'worklog 'comment)))
         (worklog `((comment . ,comment)
                    (timeSpent . ,timeSpent)
                    (timeSpentInSeconds . 10)
                    (startDate . ,startDate)))
         (worklog (if worklog-id
                      (cons `(id . ,(replace-regexp-in-string "^worklog-" "" worklog-id)) worklog)
                    worklog)))
    (if worklog-id
        (jiralib-update-worklog worklog)
      (if already_submitted (message "work log already submitted")
        (progn (jiralib-add-worklog-and-autoadjust-remaining-estimate issue-id startDate timeSpent comment)
               (org-jira-mark-current-worklog-submitted)
               (org-jira-update-worklogs-for-current-issue))))))

(defun org-jira-delete-current-comment ()
  "Delete the current comment."
  (ensure-on-comment
   (delete-region (point-min) (point-max))))

(defun org-jira-delete-current-worklog ()
  "Delete the current worklog."
  (ensure-on-worklog
   (delete-region (point-min) (point-max))))

(defun org-jira-mark-current-worklog-submitted ()
  "Mark the current worklog as submitted."
  (ensure-on-worklog
   (org-set-property "submitted_on" (format-time-string "%a %b %d %H:%M:%S %Z %Y"  (current-time)))))


;;;###autoload
(defun org-jira-copy-current-issue-key ()
  "Copy the current issue's key into clipboard."
  (interactive)
  (let ((issue-id (org-jira-get-from-org 'issue 'key)))
    (with-temp-buffer
      (insert issue-id)
      (kill-region (point-min) (point-max)))))

(defun org-jira-get-comment-id (comment)
  (org-jira-find-value comment 'id))

(defun org-jira-get-comment-author (comment)
  (org-jira-find-value comment 'author 'name))

(defun display-comment (comment)
  ;; really needed ?
  ;; (replace-regexp-in-string "^" "  " (or comment "")))

  ;; ;; emacs 25.1
  ;;   (thread-last comment
  ;;                (replace-regexp-in-string "{code}" "#+END_EXAMPLE")
  ;;                (replace-regexp-in-string "{code}" "#+END_EXAMPLE")
  ;;                ))

  ;; (replace-regexp-in-string
  ;;  "{code:java}" "#+BEGIN_EXAMPLE"
  ;;  (replace-regexp-in-string
  ;;   "{code}" "#+END_EXAMPLE"
  ;;   (or comment ""))
  ;;  )
  (or comment ""))

(defun org-jira-update-comments-for-current-issue ()
  "Update the comments for the current issue."
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
         (comments (jiralib-get-comments issue-id)))
    (mapc (lambda (comment)
            (ensure-on-issue-id issue-id
              (let* ((comment-id (org-jira-get-comment-id comment))
                     (comment-author (or (car (rassoc
                                               (org-jira-get-comment-author comment)
                                               jira-users))
                                         (org-jira-get-comment-author comment)))
                     (comment-headline (format "Comment: %s" comment-author)))
                (setq p (org-find-entry-with-id comment-id))
                (when (and p (>= p (point-min))
                           (<= p (point-max)))
                  (goto-char p)
                  (org-narrow-to-subtree)
                  (delete-region (point-min) (point-max)))
                (goto-char (point-max))
                ;; (unless (looking-at "^")
                ;;   (insert "\n"))
                (insert "** ")
                (insert comment-headline "\n")
                (org-narrow-to-subtree)
                (org-entry-put (point) "ID" comment-id)
                (let ((created (org-jira-get-comment-val 'created comment))
                      (updated (org-jira-get-comment-val 'updated comment)))
                  (org-entry-put (point) "created" created)
                  (unless (string= created updated)
                    (org-entry-put (point) "updated" updated)))
                (goto-char (point-max))
                (insert (display-comment (org-jira-find-value comment 'body)))
                (newline))))
          (cl-mapcan (lambda (comment) (if (string= (org-jira-get-comment-author comment) "admin")
                                           nil
                                         (list comment)))
                     comments))))

(defun org-jira-update-worklogs-for-current-issue ()
  "Update the worklogs for the current issue."
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
         (worklogs (jiralib-get-worklogs issue-id)))
    (mapc (lambda (worklog)
            (ensure-on-issue-id issue-id
              (let* ((worklog-id (concat "worklog-" (cdr (assoc 'id worklog))))
                     (worklog-author (or (car (rassoc
                                               (cdr (assoc 'author worklog))
                                               jira-users))
                                         (cdr (assoc 'author worklog))))
                     (worklog-headline (format "Worklog: %s" worklog-author)))
                (setq p (org-find-entry-with-id worklog-id))
                (when (and p (>= p (point-min))
                           (<= p (point-max)))
                  (goto-char p)
                  (org-narrow-to-subtree)
                  (delete-region (point-min) (point-max)))
                (goto-char (point-max))
                ;; (unless (looking-at "^")
                ;;   (insert "\n"))
                (insert "** ")
                (insert worklog-headline "\n")
                (org-narrow-to-subtree)
                (org-entry-put (point) "ID" worklog-id)
                (let ((created (org-jira-get-worklog-val 'created worklog))
                      (updated (org-jira-get-worklog-val 'updated worklog)))
                  (org-entry-put (point) "created" created)
                  (unless (string= created updated)
                    (org-entry-put (point) "updated" updated)))
                (org-entry-put (point) "startDate" (org-jira-get-worklog-val 'startDate worklog))
                (org-entry-put (point) "timeSpent" (org-jira-get-worklog-val 'timeSpent worklog))
                (goto-char (point-max))
                (insert (replace-regexp-in-string "^" "  " (or (cdr (assoc 'comment worklog)) ""))))))
          worklogs)))


;;;###autoload
(defun org-jira-update-issue ()
  "Update an issue."
  (interactive)
  (let ((issue-id (org-jira-parse-issue-id)))
    (if issue-id
        (org-jira-update-issue-details issue-id)
      (error "Not on an issue"))))

;;;###autoload
(defun org-jira-todo-to-jira ()
  "Convert an ordinary todo item to a jira ticket."
  (interactive)
  (ensure-on-todo
   (when (org-jira-parse-issue-id)
     (error "Already on jira ticket"))
   (save-excursion (org-jira-create-issue
                    (first (last (org-get-tags))) ;; last return a list but first return an element, WTF?
                    (jiralib-get-issue-type-id org-jira-default-issue-type)
                    org-jira-default-priority
                    org-jira-default-assignee
                    (org-get-heading t t)
                    (org-get-entry)))
   ;;(delete-region (point-min) (point-max))
   ))


;;;###autoload
(defun org-jira-get-subtasks ()
  "Get subtasks for the current issue."
  (interactive)
  (ensure-on-issue
   (org-jira-get-issues-headonly (jiralib-do-jql-search (format "parent = %s" (org-jira-parse-issue-id))))))

(defvar org-jira-project-read-history nil)
(defvar org-jira-priority-read-history nil)
(defvar org-jira-type-read-history nil)

(defun org-jira-read-component (project)
  "Read component name."
  (completing-read
   "Component: "
   (jiralib-make-list (jiralib-get-components project) 'key)
   nil
   t
   nil
   'org-jira-project-read-history
   (car org-jira-project-read-history)))

(defun org-jira-read-assignable-user (project)
  "Read assignable name."
  (completing-read
   "Assignable user: "
   (jiralib-make-list (jiralib-get-assignable-users project) 'key)
   nil
   t
   nil
   'org-jira-project-read-history
   (car org-jira-project-read-history)))

(defun org-jira-read-interactive-assoc-list (prompt-string assoc-data-list)
  "Get the alist coming from jira and turn it a list
   for the interactive prompts then return the code of selecte entry"
  (cdr (rassoc (completing-read prompt-string (mapcar 'cdr assoc-data-list)) assoc-data-list)))

(defun org-jira-read-project ()
  (org-jira-read-interactive-assoc-list "Project: " (jiralib-get-projects)))
  ;; "Read project name."
  ;; (completing-read
  ;;  "Project: "
  ;;  ;;(jiralib-make-list (jiralib-get-projects) 'key)
  ;;  nil
  ;;  t
  ;;  nil
  ;;  'org-jira-project-read-history
  ;;  (car org-jira-project-read-history)))

(defun org-jira-read-priority ()
  "Read priority name."
  (completing-read
   "Priority: "
   (mapcar 'cdr (jiralib-get-priorities))
   nil
   t
   nil
   'org-jira-priority-read-history
   (car org-jira-priority-read-history)))

(defun org-jira-read-issue-type ()
  "Read issue type name."
  (completing-read
   "Type: "
   (mapcar 'cdr (jiralib-get-issue-types))
   nil
   t
   nil
   'org-jira-type-read-history
   (car org-jira-type-read-history)))

;; TO DEBUG
(defun org-jira-read-subtask-type ()
  "Read issue type name."
  (completing-read
   "Type: "
   (mapcar 'cdr (jiralib-get-subtask-types))
   nil
   t
   nil
   'org-jira-type-read-history
   (car org-jira-type-read-history)))

(defun org-jira-get-issue-struct (parent-id project components issue-type priority
                                            reporter assignee versions
                                            fixVersions summary description)
  "Create an issue struct for PROJECT, of TYPE, with SUMMARY and DESCRIPTION."
  (let ((ticket-struct  (list (cons 'project (list (cons 'id (jiralib-get-project-id project))))
                              (cons 'summary (format "%s%s" summary
                                                     (if (and (boundp 'parent-id) parent-id)
                                                         (format " (subtask of [jira:%s])" parent-id)
                                                       "")))
                              (cons 'issuetype (list (cons 'id issue-type)))
                              (cons 'description description)
                              )))
    (message "get-issue-struct assignee = %s" assignee)
    (if assignee
        (append ticket-struct (list (cons 'assignee (list (cons 'name assignee))))))
    (if priority
        (append ticket-struct (list (cons 'priority (list (cons 'id priority))))))
    (if versions
        (append ticket-struct (list (cons 'versions (list (list (cons 'id versions)))))))
    ;; (if fixversions
    ;;     (append ticket-struct (cons 'fixVersions (list (list (cons 'id fixVersions))))))
    (if components
        (append ticket-struct (list (cons 'components (list (list (cons 'id components)))))))

    ticket-struct))

;;;###autoload
(defun org-jira-create-issue (&optional project issue_type priority assignee summary description)
  "Create an issue in PROJECT"
  (interactive)

  (let* ((project (or project (let ((proj (jiralib-get-projects)))
                                (if proj
                                    (cdr (rassoc (completing-read "Project: " (mapcar 'cdr proj)) proj))))))
         (components nil);; (let ((comp (jiralib-get-components project)))
         ;;              (if comp
         ;;                  (car (rassoc (completing-read "Component: " (mapcar 'cdr comp)) comp)))))
         (issue-type (or issue_type (let ((type (jiralib-get-issue-types)))
                                      (if type
                                          (car (rassoc (completing-read "IssueType: " (mapcar 'cdr type)) type))))))
         (priority (or priority (let ((prio (jiralib-get-priorities)))
                                  (if prio
                                      (car (rassoc (completing-read "Priority: " (mapcar 'cdr prio)) prio))))))
         (assignee (or assignee
                       (let ((assign (jiralib-get-assignable-users project)))
                         (if assign
                             (car (rassoc (completing-read "Assignee: " (mapcar 'cdr assign)) assign))))))
         (versions nil);;(let ((ver (jiralib-get-versions project)))
         ;;            (if ver
         ;;                (car (rassoc (completing-read "Version: " (mapcar 'cdr ver)) ver)))))
         (fixVersions nil)
         ;; (fixVersions (car (rassoc (completing-read "Fix version: " (mapcar 'cdr (jiralib-get-fixVersions project)))
         ;;                        (jiralib-get-fixVersions project))))
         (summary (or summary (read-string "Summary: ")))
         (description (or description (read-string "Description: "))))
    (if (or (not project)
            (not issue-type)
            (not priority)
            (not assignee)
            (equal versions "")
            ;; (equal fixVersions "")
            (equal summary ""))
        (error "You must provide all informations!"))
    (let* ((parent-id nil)
           (ticket-struct (org-jira-get-issue-struct parent-id project components issue-type priority
                                                     nil assignee versions
                                                     fixVersions summary description)))
      (let ((issue-id (cdr (assoc 'key (jiralib-create-issue ticket-struct)))))
        (if issue-id
            (progn
              (org-set-property "ID" issue-id)
              (org-jira-refresh-issue))
          (error "Fail to create issue"))))))

;;;###autoload
(defun org-jira-create-subtask (project type summary description)
  "Create a subtask issue for PROJECT, of TYPE, with SUMMARY and DESCRIPTION."
  (interactive (ensure-on-issue (list (org-jira-read-project)
                                      (org-jira-read-subtask-type)
                                      (read-string "Summary: ")
                                      (read-string "Description: "))))
  (if (or (equal project "")
          (equal type "")
          (equal summary ""))
      (error "Must provide all information!"))
  (let* ((parent-id (org-jira-parse-issue-id))
         (ticket-struct (org-jira-get-issue-struct parent-id project type summary description)))
    (org-jira-get-issues (list (jiralib-create-subtask ticket-struct parent-id)))))

(defun org-jira-strip-string (str)
  "Remove the beginning and ending white space for a string STR."
  (replace-regexp-in-string "\\`\n+\\|\n+\\'" "" str))

(defun org-jira-find-prop (key)
   (cond ((eq key 'description)
          (org-goto-first-child)
          (forward-thing 'whitespace)
          (if (looking-at "description: ")
              (org-jira-strip-string (org-get-entry))
            (error "Can not find description field for this issue")))
         ((eq key 'summary)
          (ensure-on-issue
           (org-get-heading t t)))
         (t
          (when (symbolp key)
            (setq key (symbol-name key)))
          (when (string= key "key")
            (setq key "ID"))
          (or (org-entry-get (point) key) ""))))

(defun org-jira-get-issue-val-from-org (key)
  "Return the requested value by KEY from the current issue."
  (ensure-on-issue (org-jira-find-prop key)))

(defvar org-jira-actions-history nil)
(defun org-jira-read-action (actions)
  "Read issue workflow progress ACTIONS."
  (let ((action (completing-read
                 "Action: "
                 (mapcar 'cdr actions)
                 nil
                 t
                 nil
                 'org-jira-actions-history
                 (car org-jira-actions-history))))
    (car (rassoc action actions))))

(defvar org-jira-fields-history nil)
(defun org-jira-read-field (fields)
  "Read (custom) FIELDS for workflow progress."
  (let ((field-desc (completing-read
                     "More fields to set: "
                     (cons "Thanks, no more fields are *required*." (mapcar 'cdr fields))
                     nil
                     t
                     nil
                     'org-jira-fields-history))
        field-name)
    (setq field-name (car (rassoc field-desc fields)))
    (if field-name
        (intern field-name)
      field-name)))


(defvar org-jira-resolution-history nil)
(defun org-jira-read-resolution ()
  "Read issue workflow progress resolution."
  (if (not jiralib-use-restapi)
      (let ((resolution (completing-read
                         "Resolution: "
                         (mapcar 'cdr (jiralib-get-resolutions))
                         nil
                         t
                         nil
                         'org-jira-resolution-history
                         (car org-jira-resolution-history))))
        (car (rassoc resolution (jiralib-get-resolutions))))
    (let* ((resolutions (org-jira-find-value fields 'resolution 'allowedValues))
           (resolution-name (completing-read
                             "Resolution: "
                             (mapcar (lambda (resolution)
                                       (org-jira-find-value resolution 'name))
                                     resolutions))))
      (cons 'name resolution-name))))

;;;###autoload
(defun org-jira-refresh-issue ()
  "Refresh issue from jira to org."
  (interactive)
  (ensure-on-issue
   (let* ((issue-id (org-jira-id)))
     (org-cut-subtree)
     (org-jira-get-issues (list (jiralib-get-issue issue-id)) 't))))

(defvar org-jira-fields-values-history nil)
;;;###autoload
(defun org-jira-progress-issue (&optional action-id)
  "Progress issue workflow."
  (interactive)
    (ensure-on-issue
    ;; (save-excursion
    ;;  (save-restriction
    ;;    (while (org-up-heading-safe)) ; go to the top heading
    ;;    (let ((org-jira-id (org-jira-id)))
    ;;      (unless (and org-jira-id (string-match (jiralib-get-issue-regexp) org-jira-id))
    ;;        (error "Not on a issue region!")))

   (let* ((issue-id (org-jira-id))
          (actions (jiralib-get-available-actions issue-id))
          (action (or action-id (org-jira-read-action actions)))
          (rest-fieds (jiralib-call "getFieldsForAction" issue-id action))
          (fields (jiralib-get-fields-for-action issue-id action))
          (field-key)
          (custom-fields-collector nil)
          (custom-fields (progn
                                        ; delete those elements in fields, which have
                                        ; already been set in custom-fields-collector

                           (while fields
                             (setq fields (cl-remove-if (lambda (strstr)
                                                          (cl-member-if (lambda (symstr)
                                                                          (string= (car strstr)  (symbol-name (car symstr))))
                                                                        custom-fields-collector))
                                                        fields))
                             (setq field-key (org-jira-read-field fields))
                             (if (not field-key)
                                 (setq fields nil)
                               (setq custom-fields-collector
                                     (cons
                                      (funcall (if jiralib-use-restapi
                                                   #'list
                                                 #'cons) field-key
                                                 (if (eq field-key 'resolution)
                                                     (org-jira-read-resolution)
                                                   (let ((field-value (completing-read
                                                                       (format "Please enter %s's value: "
                                                                               (cdr (assoc (symbol-name field-key) fields)))
                                                                       org-jira-fields-values-history
                                                                       nil
                                                                       nil
                                                                       nil
                                                                       'org-jira-fields-values-history)))
                                                     (if jiralib-use-restapi
                                                         (cons 'name field-value)
                                                       field-value))))
                                      custom-fields-collector))))
                           custom-fields-collector)))
     (jiralib-progress-workflow-action issue-id action custom-fields)
     (save-excursion
       (org-up-heading-safe)
       (org-set-property "status" (cdr (assoc action actions))))))
   ;;(org-jira-refresh-issue) this delete the worklog
   )

(defun org-jira-clock-in ()
  "Clock in the worklog and change the status of the issue to IN-PROGRESS and update JIRA status"
  (interactive)
  (org-clock-in)
  (save-excursion
    (org-up-heading-safe)
    (org-todo "IN-PROGRESS"))
  (org-jira-progress-issue org-jira-progress-action-id))

(defun org-jira-done ()
  "Clock in the worklog and change the status of the issue to IN-PROGRESS and update JIRA status"
  (interactive)
  (save-excursion
    (org-up-heading-safe)
    (org-todo "DONE"))
  (org-jira-progress-issue org-jira-done-action-id))

(defun update-issue ()
  (let* ((org-issue-description (replace-regexp-in-string "^  " "" (org-jira-get-issue-val-from-org 'description)))
         (org-issue-resolution (org-jira-get-issue-val-from-org 'resolution))
         (org-issue-priority (org-jira-get-issue-val-from-org 'prio))
         (org-issue-type (org-jira-get-issue-val-from-org 'type))
         (org-issue-assignee (jiralib-get-user (org-jira-get-issue-val-from-org 'assignee)))
         (org-issue-status (org-jira-get-issue-val-from-org 'status))
         (org-issue-summary (replace-regexp-in-string "^\[[A-Za-z0-9-]*\] " ""
                                                      (org-jira-get-issue-val-from-org 'summary)))
         (issue (jiralib-get-issue issue-id))
         (project (org-jira-get-issue-val 'project issue))
         (project-components (jiralib-get-components project)))

    (message "Update issue: description = %s" org-issue-description )
    (message "Update issue: assignee = %s" org-issue-assignee)

    (jiralib-update-issue issue-id
                          (list
                           (cons 'priority (let ((id (car (rassoc org-issue-priority (jiralib-get-priorities)))))
                                             `((id . ,id)
                                               (name . ,org-issue-priority))))
                           (cons 'description org-issue-description)
                           (cons 'assignee org-issue-assignee)
                           (cons 'summary org-issue-summary)))
    (org-jira-get-issues (list (jiralib-get-issue issue-id)))))

(defun org-jira-update-issue-details (issue-id)
  "Update the details of issue ISSUE-ID."
  (ensure-on-issue-id issue-id (update-issue)))

(defun org-jira-parse-issue-id ()
  "Get issue id from org text."
    (let ((continue t)
          issue-id)
      (save-excursion
        (while continue
          (when (string-match (jiralib-get-issue-regexp)
                              (or (setq issue-id (org-entry-get (point) "ID"))
                                  ""))
          (setq continue nil))
        (unless (and continue (org-up-heading-safe))
          (setq continue nil)))
      issue-id)
    )
  )

(defun org-jira-get-from-org (type entry)
  "Get an org property from the current item.

TYPE is the type to of the current item, and can be 'issue, or 'comment.

ENTRY will vary, and is the name of the property to return.  If
it is a symbol, it will be converted to string."
  (when (symbolp entry)
    (setq entry (symbol-name entry)))
  (cond
   ((eq type 'issue)
    (org-jira-get-issue-val-from-org entry))
   ((eq type 'comment)
    (org-jira-get-comment-val-from-org entry))
   ((eq type 'worklog)
    (org-jira-get-worklog-val-from-org entry))
   (t (error "Unknown type %s" type))))

(defun org-jira-get-comment-val-from-org (entry)
  "Get the JIRA issue field value ENTRY of the current comment item."
  (ensure-on-comment
   (when (symbolp entry)
     (setq entry (symbol-name entry)))
   (when (string= entry "id")
     (setq entry "ID"))
   (org-entry-get (point) entry)))

(defun org-jira-get-worklog-val-from-org (entry)
  "Get the JIRA issue field value ENTRY of the current worklog item."
  (ensure-on-worklog
   (when (symbolp entry)
     (setq entry (symbol-name entry)))
   (when (string= entry "id")
     (setq entry "ID"))
   (org-entry-get (point) entry)))

(defun org-jira-get-comment-body (&optional comment-id)
  "Get the comment body of the comment with id COMMENT-ID."
  (ensure-on-comment
   (goto-char (point-min))
   ;; so that search for :END: won't fail
   (org-entry-put (point) "ID" comment-id)
   (search-forward ":END:")
   (forward-line)
   (org-jira-strip-string (buffer-substring-no-properties (point) (point-max)))))

(defun org-jira-get-worklog-comment (&optional worklog-id)
  "Get the worklog comment of the worklog with id WORKLOG-ID."
  (ensure-on-worklog
   (goto-char (point-min))
   ;; so that search for :END: won't fail
   (org-entry-put (point) "ID" worklog-id)
   (search-forward ":END:")
   (forward-line)
   (org-jira-strip-string (buffer-substring-no-properties (point) (point-max)))))

(defun org-jira-id ()
  "Get the ID entry for the current heading."
  (org-entry-get (point) "ID"))

;;;###autoload
(defun org-jira-browse-issue ()
  "Open the current issue in external browser."
  (interactive)
  (ensure-on-issue
   (browse-url (concat (jiralib-get-baseurl) "/browse/" (org-jira-id)))))

;;;###autoload
(defun org-jira-get-issues-from-filter (filter)
  "Get issues from the server-side stored filter named FILTER.

Provide this command in case some users are not able to use
client side jql (maybe because of JIRA server version?)."
  (interactive
   (list (completing-read "Filter: " (mapcar 'cdr (jiralib-get-saved-filters)))))
  (org-jira-get-issues (jiralib-get-issues-from-filter (car (rassoc filter (jiralib-get-saved-filters))))))

;;;###autoload
(defun org-jira-get-issues-from-filter-headonly (filter)
  "Get issues *head only* from saved filter named FILTER.
See `org-jira-get-issues-from-filter'."
  (interactive
   (list (completing-read "Filter: " (mapcar 'cdr (jiralib-get-saved-filters)))))
  (org-jira-get-issues-headonly (jiralib-get-issues-from-filter (car (rassoc filter (jiralib-get-saved-filters))))))

(org-add-link-type "jira" 'org-jira-open)

(defun org-jira-open (path)
  "Open a Jira Link from PATH."
  (org-jira-get-issues (list (jiralib-get-issue path))))

(provide 'org-jira)
;;; org-jira.el ends here
