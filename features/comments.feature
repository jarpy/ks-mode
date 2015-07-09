Feature: Comments.
  Scenario: Full-stop (period) in comments does not affect indentation.
    Given the code
    """
    one, // one.
    two. // two.
    """
    Then it should indent like
    """
    one, // one.
      two. // two.
    """
