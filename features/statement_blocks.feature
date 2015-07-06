Feature: Statement Blocks

  Scenario: Multi-line statement block
    Given the code
    """
    if true {
    print "true".
    }
    """
    Then it should indent like
    """
    if true {
      print "true".
    }
    """
