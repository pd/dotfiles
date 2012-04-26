Feature: shell-mode management

  Scenario: Launching a new shell from a file
    Given I have a shell-mode buffer named "*shell*"
    And   I am in buffer "*scratch*"
    When  I press "C-c s"
    Then  I should be in buffer "*shell*"

  Scenario: Switching to an already open shell from a file
    Given I have a shell-mode buffer named "*shell*"
    And   I am in buffer "*scratch*"
    When  I press "C-c s"
    Then  I should be in buffer "*shell*"

  Scenario: Launching a new shell from a file
    Given I have a shell-mode buffer named "*shell*"
    And   I am in buffer "*scratch*"
    When  I press "C-u C-c s"
    Then  I should be in buffer "*shell*<2>"

  Scenario: Launching a new shell from an open shell
    Given I am in buffer "*scratch*"
    When  I press "C-c s"
    Then  I should be in buffer "*shell*"
    And   the buffer major-mode should be "shell-mode"
    When  I press "C-c s"
    Then  I should be in buffer "*shell*<2>"
    And   the buffer major-mode should be "shell-mode"

  Scenario: Switching to the nearest shell
    Given these temporary directories exist:
      | project-a/lib      |
      | project-a/features |
      | project-b/lib      |
      | project-b/features |
    Given I have a shell-mode buffer in "/tmp/project-a" named "*project-a*"
    And   I have a shell-mode buffer in "/tmp/project-b/features" named "*project-b*"
    And   I have a shell-mode buffer in "/" named "*other*"
    And   I am viewing the file "/tmp/project-a/lib/foo.rb"
    When  I press "C-c s"
    Then  I should be in buffer "*project-a*"
    When  I am viewing the file "/tmp/project-b/foo.rb"
    And   I press "C-c s"
    Then  I should be in buffer "*project-b*"
