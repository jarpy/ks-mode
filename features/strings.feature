Feature: Strings

  Scenario: Braces in strings are ignored
    Given the code
    """
    print "This string has a { in it.".    
    print "This one has a { too!".
    """
    Then it should indent like
    """
    print "This string has a { in it.".    
    print "This one has a { too!".
    """
