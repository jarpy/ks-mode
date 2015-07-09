Feature: Statement Blocks

  Scenario: Blank lines are indented at the left margin.
    "This code block has four spaces inside it. They should be removed."
    Given the code
    """
    {
        
    }
    """
    Then it should indent like
    """
    {

    }
    """

  Scenario: Blank lines do not affect indentation.
    Given the code
    """
    if true {

    stage.

    }
    """
    Then it should indent like
    """
    if true {

      stage.

    }
    """

  Scenario: Header comments do not affect indentation
    Given the code
    """
    // Comments are nice, but should not mess up indentation.

    @LAZYGLOBAL off.
    """
    Then it should indent like
    """
    // Comments are nice, but should not mess up indentation.

    @LAZYGLOBAL off.
    """
