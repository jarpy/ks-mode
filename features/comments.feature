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

  Scenario: Comments are indented within statement blocks.
    Given the code
    """
    function splode {
    // Blow up.
    }
    """
    Then it should indent like
    """
    function splode {
      // Blow up.
    }
    """
