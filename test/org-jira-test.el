(require 'jiralib)

(ert-deftest test-jiralib-make-assoc-list ()
  (let ((expect '(("1" . "name1") ("2" . "name2") ("3" . "name3")))
        (data1 '(
                 ((id . "1") (name . "name1") (dummy . "dummy1"))
                 ((id . "2") (name . "name2") (dummy . "dummy2"))
                 ((id . "3") (name . "name3") (dummy . "dummy3"))
                 ))
        (data2 [
                ((id . "1") (name . "name1") (dummy . "dummy1"))
                ((id . "2") (name . "name2") (dummy . "dummy2"))
                ((id . "3") (name . "name3") (dummy . "dummy3"))
                ]))


    (should (equal expect (jiralib-make-assoc-list data1 'id 'name)))
    (should (equal expect (jiralib-make-assoc-list data2 'id 'name)))))
