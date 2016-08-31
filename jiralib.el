;;; jiralib.el -- Provide connectivity to JIRA REST service

;; Authors:
;; Bertrand Lallau <bertrand.lallau@gmail.com>
;; Bao Haojun <baohaojun@gmail.com>

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

;; Package-Requires: ((request "0.2.0"))
;; Keywords: rest, jira

;;; Commentary:
;; This file provides a programatic interface to JIRA.  It provides access to
;; JIRA from other programs, but no user level functionality.

;; Jira References:
;;
;; https://docs.atlassian.com/jira/REST/latest/
;;

(eval-when-compile (require 'cl))
(require 'request)
(require 'json)
(require 'url-parse)
(require 'seq)

;;; Code:
(defgroup jiralib nil
  "Jiralib customization group."
  :group 'applications)

(defgroup jiralib-faces nil
  "Faces for displaying Jiralib information."
  :group 'jiralib)

(defcustom jiralib-host ""
  "User customizable host name of the Jiralib server.

This will be used with USERNAME to compute password from
.authinfo file.  Will be calculated from jiralib-url if not set."
  :group 'jiralib
  :type 'string
  :initialize 'custom-initialize-set)

(defvar jiralib-mode-hook nil)

(defvar jiralib-mode-map nil)

(defcustom jiralib-url
  "http://localhost:8081/"
  "The address of the jira host."
  :type 'string
  :group 'jiralib)

(defvar jiralib-token nil
  "JIRA token used for authentication.")

(defvar jiralib-rest-auth-head nil
  "JIRA restapi auth head.")

(defvar jiralib-user-login-name nil
  "The name of the user logged into JIRA.
This is maintained by `jiralib-login'.")

(setq jiralib-use-restapi t)



(defun jiralib-get-baseurl ()
  "return the baseurl in canonical format, i.e. without trailing /"
  (replace-regexp-in-string "/*$" "" jiralib-url))

(defun jiralib-get-issue-url (issue-id)
  "return a url to issue with the given issue id."
  (format "%s/browse/%s" (jiralib-get-baseurl) issue-id))


(defun jiralib-login (username password)
  "Login into JIRA as user USERNAME with PASSWORD."
  (interactive
   (if (> 24 emacs-major-version)
       (let ((user (read-string "Username for Jira server login? "))
             (password (read-passwd "Password for Jira server login? ")))
         (list user password))
     (jiralib--get-credentials-from-authsource)))
  (jiralib--set-token username password)
  (jiralib--test-connection))

(defun jiralib--get-credentials-from-authsource ()
  (let ((found (nth 0 (auth-source-search :max 1
                                          :host (if (string= jiralib-host "")
                                                    (url-host (url-generic-parse-url jiralib-url))
                                                  jiralib-host)
                                          :port (url-port (url-generic-parse-url jiralib-url))
                                          :require '(:user :secret)
                                          :create t)))
        user secret)
    (when found
      (let* ((user (plist-get found :user))
             (sec (plist-get found :secret))
             (secret (if (functionp sec)
                         (funcall sec)
                       sec)))
        (list user secret)))))

(defun jiralib--set-token (username password)
  (setq jiralib-token
        `("Authorization" . , (format "Basic %s"
                                      (base64-encode-string
                                       (concat username ":" password))))))

(defun jiralib--test-connection ()
  "get JIRA configuration to check if we are connected"

  ;;; check access to the the current user information to verify
  ;;; authentication succeeded
  (jiralib--rest-call-it "myself"))



(defun jiralib-call (method &rest params)
  "Invoke the JIRA METHOD with supplied PARAMS.

This function should be used for all JIRA interface calls, as the
method ensures the user is logged in and invokes the REST method
with the correct authentication token.

All JIRA inteface methods take an authentication token as the
first argument.  The authentication token is supplied by this
function, so PARAMS should omit this parameter.  For example, the
\"getIssue\" method takes two parameters: auth and key, however,
when invoking it through `jiralib-call', the call shoulbe be:

  (jiralib-call \"getIssue\" KEY)"
  (unless jiralib-token
    (call-interactively 'jiralib-login))
  (case (intern method)
    ('getStatuses (jiralib--rest-call-it "status"))
    ('getIssueTypes (jiralib--rest-call-it "issuetype"))
    ('getUser (jiralib--rest-call-it "user" :params `((username . ,(first params)))))
    ('getAssignableUsers (jiralib--rest-call-it "user/assignable/search" :params `((project . ,(first params))(maxResults . ,"1000"))))
    ('getVersions (jiralib--rest-call-it (format "project/%s/versions" (first params))))
    ('getWorklogs nil) ; fixme
    ('addComment (jiralib--rest-call-it
                  (format "issue/%s/comment" (first params))
                  :type "POST"
                  :data (json-encode (second params))))
    ('addWorklogAndAutoAdjustRemainingEstimate (jiralib--rest-call-it
                                                (format "issue/%s/worklog" (first params))
                                                :type "POST"
                                                :data (json-encode (second params))))
    ('createIssue (jiralib--rest-call-it
                   "issue"
                   :type "POST"
                   :data (json-encode (list (cons 'fields (first params))))))
    ('createIssueWithParent (jiralib--rest-call-it))
    ('editComment (jiralib--rest-call-it
                   (format "issue/%s/comment/%s" (first params) (second params))
                   :data (json-encode `((body . ,(third params))))
                   :type "PUT"))
    ('getComments (org-jira-find-value (jiralib--rest-call-it
                                        (format "issue/%s/comment" (first params))) 'comments))
    ('getComponents (jiralib--rest-call-it
                     (format "project/%s/components" (first params))))
    ('getIssue (jiralib--rest-call-it
                (format "issue/%s" (first params))))
    ('getIssuesFromJqlSearch (append (cdr ( assoc 'issues (jiralib--rest-call-it
                                                           "search"
                                                           :type "POST"
                                                           :data (json-encode `((jql . ,(first params))
                                                                                (maxResults . ,(second params))))))) nil))
    ('getPriorities (jiralib--rest-call-it
                     "priority"))
    ('getProjectsNoSchemes (jiralib--rest-call-it "project" :params `((expand . ,(first params)))))
    ('getResolutions (append (jiralib--rest-call-it
                              "resolution") nil))
    ('getAvailableActions
     (let* ((issue-id (first params))
            (data (jiralib--rest-call-it
                   (format "issue/%s/transitions" issue-id)))
            (transitions (cdr (assoc 'transitions data))))
       transitions))
    ('getFieldsForAction (org-jira-find-value (car (let ((issue (first params))
                                                         (action (second params)))
                                                     (seq-filter (lambda (trans)
                                                                   (or (string-equal action (org-jira-find-value trans 'id))
                                                                       (string-equal action (org-jira-find-value trans 'name))))
                                                                 (cdar (jiralib--rest-call-it
                                                                        (format "issue/%s/transitions" (first params))
                                                                        :params '((expand . "transitions.fields")))))))
                                              'fields))
    ('progressWorkflowAction (jiralib--rest-call-it
                              (format "issue/%s/transitions" (first params))
                              :type "POST"
                              :data (json-encode `(,(car (second params)) ,(car (third params))))))
    ('updateIssue (jiralib--rest-call-it
                   (format "issue/%s" (first params))
                   :type "PUT"
                   :data (json-encode `((fields . ,(second params))))))))

(defun jiralib--utf-8-parser ()
  "Decode the HTTP response using the charset UTF-8."
  (decode-coding-region (point-min) (point-max) 'utf-8)
  (json-read))

(cl-defun jiralib--report-error (&key
                                 (data nil)
                                 (error-thrown nil)
                                 (symbol-status nil)
                                 (response response)
                                 &allow-other-keys)
  (message (concat "Error: " (pp error-thrown)))
  (message (concat "Data: " (pp data)))
  (message (concat "Symbol-Status: " (pp symbol-status)))
  (message (concat "Response: " (pp response)))
  (error "Error accessing JIRA REST interface (%s) : %s"
         (pp symbol-status) (pp response)))


(defun jiralib--rest-call-it (api &rest args)
  "Invoke the corresponding jira rest method API, passing ARGS to REQUEST."
  (append (request-response-data
           (apply #'request (concat (jiralib-get-baseurl)
                                    "/rest/api/2/"
                                    (replace-regexp-in-string "^/*" "" api))
                  :sync t
                  :headers `(,jiralib-token ("Content-Type" . "application/json"))
                  :parser 'jiralib--utf-8-parser
                  :error #'jiralib--report-error
                  args)) nil))

;;;; Some utility functions

(defun jiralib-make-list (data field)
  "Map all assoc elements in DATA to the value of FIELD in that element."
  (loop for element in data
        collect (cdr (assoc field element))))

(defun jiralib-make-assoc-list (data key-field value-field)
  "Create an association list from a structure array.

DATA is a list of association lists
KEY-FIELD is the field to use as the key in the returned alist
VALUE-FIELD is the field to use as the value in the returned alist"
  (mapcar (lambda (e)
            (cons (cdr (assoc key-field e))
                  (cdr (assoc value-field e))))
          data))

(defun jiralib-make-assoc-list-on-cond (data key-field value-field cond-name cond-value)
  "Create an association list from a structure array matching condition.

DATA is a list of association lists
KEY-FIELD is the field to use as the key in the returned alist
VALUE-FIELD is the field to use as the value in the returned alist
COND-NAME is the field to use as the key for condition
COND-VALUE is the field to use as the value for condition"

  (loop for element in data
        when (equal (cdr (assoc cond-name element)) cond-value)
        collect (cons (cdr (assoc key-field element))
                      (cdr (assoc value-field element)))))

;;;; Wrappers around JIRA methods

(defun jiralib-update-issue (key fields)
  "Update the issue with id KEY with the values in FIELDS."
  (jiralib-call "updateIssue" key fields))

(defvar jiralib-status-codes-cache nil)

(defun jiralib-get-statuses ()
  "Return an assoc list mapping a status code to its name.
NOTE: Status codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, then
will cache it."
  (unless jiralib-status-codes-cache
    (setq jiralib-status-codes-cache
          (jiralib-make-assoc-list (jiralib-call "getStatuses") 'id 'name)))
  jiralib-status-codes-cache)

(defvar jiralib-issue-types-cache nil)

(defun jiralib-get-issue-types ()
  "Return an assoc list mapping an issue type code to its name.
NOTE: Issue type codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jiralib-issue-types-cache
    (setq jiralib-issue-types-cache
          (jiralib-make-assoc-list (jiralib-call "getIssueTypes") 'id 'name)))
  jiralib-issue-types-cache)

(defun jiralib-get-issue-type-id (name)
  (car (rassoc name (jiralib-get-issue-types))))

(defvar jiralib-priority-codes-cache nil)

(defun jiralib-get-priorities ()
  "Return an assoc list mapping a priority code to its name.
NOTE: Priority codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jiralib-priority-codes-cache
    (setq jiralib-priority-codes-cache
          (jiralib-make-assoc-list (jiralib-call "getPriorities") 'id 'name)))
  jiralib-priority-codes-cache)

(defvar jiralib-versions-cache nil)

(defun jiralib-get-versions (key)
  "Return an assoc list mapping version id and version number.
This function will only ask JIRA for the list of name once, than
will cache it."
  (unless jiralib-versions-cache
    (setq jiralib-versions-cache
          (jiralib-make-assoc-list-on-cond (jiralib-call "getVersions" key) 'id 'name 'released t)))
  jiralib-versions-cache)

(defvar jiralib-fixVersions-cache nil)

(defun jiralib-get-fixVersions (key)
  "Return an assoc list mapping version id and version number.
This function will only ask JIRA for the list of name once, than
will cache it."
  (unless jiralib-fixVersions-cache
    (setq jiralib-fixVersions-cache
          (jiralib-make-assoc-list-on-cond (jiralib-call "getVersions" key) 'id 'name 'released :json-false)))
  jiralib-fixVersions-cache)

(defvar jiralib-assignable-users-cache nil)

(defun jiralib-get-assignable-users (key)
  "Return an assoc list mapping user key and display name.
This function will only ask JIRA for the list of name once, than
will cache it."
  (unless jiralib-assignable-users-cache
    (setq jiralib-assignable-users-cache
          (jiralib-make-assoc-list (jiralib-call "getAssignableUsers" key) 'key 'displayName)))
  jiralib-assignable-users-cache)

(defvar jiralib-resolution-code-cache nil)

(defun jiralib-get-resolutions ()
  "Return an assoc list mapping a resolution code to its name.
NOTE: Resolution codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jiralib-resolution-code-cache
    (setq jiralib-resolution-code-cache
          (jiralib-make-assoc-list (jiralib-call "getResolutions") 'id 'name)))
  jiralib-resolution-code-cache)

(defvar jiralib-issue-regexp nil)

;; NOTE: it is not such a good ideea to use this, as it needs a JIRA
;; connection to construct the regexp (the user might be prompted for a JIRA
;; username and password).
;;
;; The best use of this function is to generate the regexp once-off and
;; persist it somewhere.

(defun jiralib-get-issue-regexp ()
  "Return a regexp that will match an issue id.

The regexp is constructed from the project keys in the JIRA
database.  An issue is assumed to be in the format KEY-NUMBER,
where KEY is a project key and NUMBER is the issue number."
  (unless jiralib-issue-regexp
    (let ((projects (mapcar (lambda (e) (downcase (cdr (assoc 'key e))))
                            (jiralib-call "getProjectsNoSchemes"))))
      (setq jiralib-issue-regexp (concat "\\<" (regexp-opt projects) "-[0-9]+\\>"))))
  jiralib-issue-regexp)

(defun jiralib-do-jql-search (jql &optional limit)
  "Run a JQL query and return the list of issues that matched.
LIMIT is the maximum number of queries to return.  Note that JIRA
has an internal limit of how many queries to return, as such, it
might not be possible to find *ALL* the issues that match a
query."
  (unless (or limit (numberp limit))
    (setq limit 100))
  (jiralib-call "getIssuesFromJqlSearch" jql limit))

(defun jiralib-get-available-actions (issue-key)
  "Return the available workflow actions for ISSUE-KEY.
This runs the getAvailableActions REST method."
  (jiralib-make-assoc-list
   (jiralib-call "getAvailableActions" issue-key)
   'id 'name))

(defun jiralib-get-fields-for-action (issue-key action-id)
  "Return the required fields to change ISSUE-KEY to ACTION-ID."
  (let ((fields (jiralib-call "getFieldsForAction" issue-key action-id)))
    (mapcar (lambda (field)
              (cons (symbol-name (car field)) (format "%s (required: %s)"
                                                      (org-jira-find-value field 'name)
                                                      (if (eq (org-jira-find-value field 'required) :json-false)
                                                          "nil"
                                                        "t")))) fields)))

(defun jiralib-progress-workflow-action (issue-key action-id params)
  "Progress issue with ISSUE-KEY to action ACTION-ID, and provide the needed PARAMS."
  (jiralib-call "progressWorkflowAction" issue-key `((transition (id . ,action-id)))
                `((fields . ,params))))


(defun jiralib-add-worklog-and-autoadjust-remaining-estimate (issue-key start-date time-spent comment)
  "Log time spent on ISSUE-KEY to its worklog.
The time worked begins at START-DATE and has a TIME-SPENT
duration.  JIRA will automatically update the remaining estimate
by subtracting TIME-SPENT from it.

START-DATE should be in the format 2010-02-05T14:30:00Z

TIME-SPENT can be in one of the following formats: 10m, 120m
hours; 10h, 120h days; 10d, 120d weeks.

COMMENT will be added to this worklog."
  (jiralib-call "addWorklogAndAutoAdjustRemainingEstimate"
                issue-key
                `((started . ,start-date)
                  (timeSpent . ,time-spent)
                  (comment   . ,comment))))

;;;; Issue field accessors

(defun jiralib-issue-key (issue)
  "Return the key of ISSUE."
  (cdr (assoc 'key issue)))

(defun jiralib-issue-owner (issue)
  "Return the owner of ISSUE."
  (cdr (assq 'assignee issue)))

(defun jiralib-issue-status (issue)
  "Return the status of ISSUE as a status string (not as a number!)."
  (let ((status-code (cdr (assq 'status issue))))
    (cdr (assoc status-code (jiralib-get-statuses)))))

(defun jiralib-custom-field-value (custom-field issue)
  "Return the value of CUSTOM-FIELD for ISSUE.
Return nil if the field is not found"
  (catch 'found
    (dolist (field (cdr (assq 'customFieldValues issue)))
      (when (equal (cdr (assq 'customfieldId field)) custom-field)
        (throw 'found (cadr (assq 'values field)))))))

(defvar jiralib-current-issue nil
  "This holds the currently selected issue.")

(defvar jiralib-projects-list nil
  "This holds a list of projects and their details.")

(defvar jiralib-types nil
  "This holds a list of issues types.")

(defvar jiralib-priorities nil
  "This holds a list of priorities.")

(defvar jiralib-user-fullnames nil
  "This holds a list of user fullnames.")

;; (defun jiralib-get-project-name (key)
;;   "Return the name of the JIRA project with id KEY."
;;   (let ((projects jiralib-projects-list)
;;         (name nil))
;;     (dolist (project projects)
;;       (if (equal (cdr (assoc 'key project)) key)
;;           (setf name (cdr (assoc 'name project)))))
;;     name))

(defun jiralib-get-project-id (key)
  "Return the ID of the JIRA project with name KEY."
  ;;we need to make sure that the list is populated
  (let ((projects (jiralib-get-projects))
        (id nil))
    (dolist (project projects)
      (if (equal (cdr project) key)
          (setf id (car project))))
    id))

(defun jiralib-get-type-name (id)
  "Return the name of the issue type with ID."
  (let ((types jiralib-types)
        (name nil))
    (dolist (type types)
      (if (equal (cdr (assoc 'id type)) id)
          (setf name (cdr (assoc 'name type)))))
    name))

(defun jiralib-get-user-fullname (username)
  "Return the full name (display name) of the user with USERNAME."
  (if (assoc username jiralib-user-fullnames)
      (cdr (assoc username jiralib-user-fullnames))
    (progn
      (let ((user (jiralib-get-user username)))
        (setf jiralib-user-fullnames (append jiralib-user-fullnames (list (cons username (cdr (assoc 'fullname user))))))
        (cdr (assoc 'fullname user))))))


(defun jiralib-get-filter (filter-id)
  "Return a filter given its FILTER-ID."
  (cl-flet ((id-match (filter)
                      (equal filter-id (cdr (assoc 'id filter)))))
    (cl-find-if 'id-match (jiralib-get-saved-filters))))

(defun jiralib-get-filter-alist ()
  "Return an association list mapping filter names to IDs."
  (mapcar (lambda (filter)
            (cons (cdr (assoc 'name filter))
                  (cdr (assoc 'id filter))))
          (jiralib-get-saved-filters)))

(defun jiralib-add-comment (issue-key comment)
  "Add to issue with ISSUE-KEY the given COMMENT."
  (jiralib-call "addComment" issue-key `((body . ,comment))))

(defun jiralib-edit-comment (issue-id comment-id comment)
  "Edit ISSUE-ID's comment COMMENT-ID to reflect the new COMMENT."
  (jiralib-call "editComment" issue-id comment-id comment))

(defun jiralib-create-issue (issue)
  "Create a new ISSUE in JIRALIB.

ISSUE is a Hashtable object."
  (jiralib-call "createIssue" issue))

(defun jiralib-create-subtask (subtask parent-issue-id)
  "Create SUBTASK for issue with PARENT-ISSUE-ID.

SUBTASK is a Hashtable object."
  (jiralib-call "createIssueWithParent" subtask parent-issue-id))


(defvar jiralib-subtask-types-cache nil)

(defun jiralib-get-subtask-types ()
  "Return an assoc list mapping an issue type code to its name.
NOTE: Issue type codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jiralib-subtask-types-cache
    (setq jiralib-subtask-types-cache
          (jiralib-make-assoc-list (jiralib-call "getSubTaskIssueTypes") 'id 'name)))
  jiralib-subtask-types-cache)


(defun jiralib-get-comments (issue-key)
  "Return all comments associated with issue ISSUE-KEY."
  (jiralib-call "getComments" issue-key))

(defun jiralib-get-worklogs (issue-key)
  "Return all worklogs associated with issue ISSUE-KEY."
  (jiralib-call "getWorklogs" issue-key))

(defun jiralib-update-worklog (worklog)
  "Update the WORKLOG, updating the ETA for the related issue."
  (jiralib-call "updateWorklogAndAutoAdjustRemainingEstimate" worklog))

(defun jiralib-get-components (project-key)
  "Return all components available in the project PROJECT-KEY."
  (let ((components (jiralib-call "getComponents" project-key)))
    (jiralib-make-assoc-list components 'id 'name)))

(defun jiralib-get-issue (issue-key)
  "Get the issue with key ISSUE-KEY."
  (jiralib-call "getIssue" issue-key))

(defun jiralib-get-issues-from-filter (filter-id)u
       "Get the issues from applying saved filter FILTER-ID."
       (jiralib-call "getIssuesFromFilter" filter-id))

(defun jiralib-get-issues-from-text-search (search-terms)
  "Find issues using free text search SEARCH-TERMS."
  (jiralib-call "getIssuesFromTextSearch" search-terms))

(defun jiralib-get-issues-from-text-search-with-project
    (project-keys search-terms max-num-results)
  "Find issues in projects PROJECT-KEYS, using free text search SEARCH-TERMS.

Return no more than MAX-NUM-RESULTS."
  (jiralib-call "getIssuesFromTextSearchWithProject"
                (apply 'vector project-keys) search-terms max-num-results))

(defvar jiralib-projects-list nil
  "This holds a list of projects and their details.")

(defun jiralib-get-projects ()
  "Return a list of projects available to the user."
  (unless jiralib-projects-list
    (setq jiralib-projects-list
          (let ((projects
                 (jiralib-call "getProjectsNoSchemes" "description,projectKeys")))
            (jiralib-make-assoc-list projects 'id 'key))))
  jiralib-projects-list)

(defun jiralib-get-projects-details ()
  "Return a list of projects details available to the user."
  (jiralib-call "getProjectsNoSchemes" "description,lead,projectKeys,url"))

(defun jiralib-get-saved-filters ()
  "Get all saved filters available for the currently logged in user."
  (jiralib-make-assoc-list (jiralib-call "getSavedFilters") 'id 'name))

(defun jiralib-get-server-info ()
  "Return the Server information such as baseUrl, version, edition, buildDate, buildNumber."
  (jiralib-call "getServerInfo"))

(defun jiralib-get-sub-task-issue-types ()
  "Return all visible subtask issue types in the system."
  (jiralib-call "getSubTaskIssueTypes"))

(defun jiralib-get-user (username)
  "Return a user's information given their USERNAME."
  (jiralib-call "getUser" username))

(defun jiralib-get-versions (project-key)
  "Return all versions available in project PROJECT-KEY."
  (jiralib-call "getVersions" project-key))

(defun jiralib-strip-cr (string)
  "Remove carriage returns from STRING."
  (when string (replace-regexp-in-string "\r" "" string)))

(provide 'jiralib)
;;; jiralib.el ends here
