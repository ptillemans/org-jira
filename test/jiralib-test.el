

(ert-deftest jiralib-test-get-base-url ()
  "test that the function returns a clean version of the jiralib-url"
  (let ((jiralib-url "http://localhost:8080"))
    (should (equal "http://localhost:8080" (jiralib-get-baseurl))))
  (let ((jiralib-url "http://localhost:8080/"))
    (should (equal "http://localhost:8080" (jiralib-get-baseurl))))
  (let ((jiralib-url "http://localhost:8080/////"))
    (should (equal "http://localhost:8080" (jiralib-get-baseurl)))))

(ert-deftest jiralib-test-get-issue-url ()
  "test that the function returns a proper url for browsing the jira issue"
  (let ((jiralib-url "http://localhost:8080")
        (issue-id "TEST-1"))
    (should (equal "http://localhost:8080/browse/TEST-1"
                   (jiralib-get-issue-url issue-id)))))
