Feature: Statement Blocks

  Scenario: Statement blocks get indented contents.
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

  Scenario: Inline else-if aligns correctly.
    Given the code
    """
    if true {
    print "t".
    } else if false {
    print "f".
    }
    """
    Then it should indent like
    """
    if true {
      print "t".
    } else if false {
      print "f".
    }
    """

  Scenario: Nested statement blocks get nested indent.
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

    Scenario: K&R style blocks are OK too... I guess :(
      Given the code
      """
      function boom
      {
      stage.
      }
      """
      Then it should indent like
      """
      function boom
      {
        stage.
      }
      """
