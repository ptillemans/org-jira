Feature: Do Some things
  In order to do something
  As a user
  I want to do something

  Background:
    Given I am in buffer "foo"
    And I should be in buffer "foo"
    And the buffer is empty
    And I turn on org-mode
    And I turn on org-jira-mode


  Scenario: Do Something
    And I insert:
    """
    * TODO do something
    ** Description
    Do something very important.
    """
    Given I start an action chain
    And I press "C-c t j"
    And I type "TEST"
    When I execute the action chain
    Then I should see:
    """
    * TODO [TEST-1] do something
    :PROPERTIES:
    :assignee: xyz
    :prio:     Major
    :status:   Open
    :created:  2016-08-24 14:02:45
    :updated:  2016-08-24 14:02:45
    :ID:       TEST-1
    :END:
    ** description: [http://localhost:8080/browse/TEST-1][TEST-1]
    Do something very important
    """
