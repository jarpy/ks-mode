
Feature: Continuation Lines

  Scenario: Multi-line statements get a hanging indent.
    Given the code
    """
    parameter
    x,
    y,
    z.
    """
    Then it should indent like
    """
    parameter
      x,
      y,
      z.
    """

  Scenario: De-indent after closing a multi-line statment.
    Given the code
    """
    parameter
      x,
      y.

      stage.
    """
    Then it should indent like
    """
    parameter
      x,
      y.

    stage.
    """
