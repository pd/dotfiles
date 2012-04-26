Feature: shell-mode management

  Scenario: Launching a new shell from an open shell
    Given I am in buffer "*scratch*"
    When  I press "C-c s"
    Then  I should be in buffer "*shell*"
    And   the buffer major-mode should be "shell-mode"
    When  I press "C-c s"
    Then  I should be in buffer "*shell*<2>"
    And   the buffer major-mode should be "shell-mode"

  Scenario: Switching to an already open shell from a file
    Given I have a shell-mode buffer named "*shell*"
    And   I am in buffer "*scratch*"
    When  I press "C-c s"
    Then  I should be in buffer "*shell*"
