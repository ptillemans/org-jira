

(ert-deftest jiralib-test-get-base-url ()
  "test that the function returns a clean version of the jiralib-url"
  (let ((jiralib-url "http://localhost:8080"))
    (should (equal "http://localhost:8080" (jiralib-get-baseurl))))
  (let ((jiralib-url "http://localhost:8080/"))
    (should (equal "http://localhost:8080" (jiralib-get-baseurl))))
  (let ((jiralib-url "http://localhost:8080/////"))
    (should (equal "http://localhost:8080" (jiralib-get-baseurl)))))
