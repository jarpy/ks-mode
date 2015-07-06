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

  Scenario: Inline else if.
    Given the code
    """
    if true {
    print "t"
    } else if false {
    print "f"
    }
    """
    Then it should indent like
    """
    if true {
      print "t"
    } else if false {
      print "f"
    }
    """


  Scenario: Nested statement block indent nicely.
    Given the code
    """
    {
    {
    }
    }
    """
    Then it should indent like
    """
    {
      {
      }
    }
    """

