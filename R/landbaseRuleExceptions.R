#------------------------------------------------------------------------------
# Landbase Rule Exceptions
#
# Defines default landbase classes for Protected Areas and Ownership,
# with optional jurisdiction- and SYU-specific overrides.
#
# Classes:
#   0 = excluded from landbase
#   1 = included but non-harvestable
#   2 = harvestable
#------------------------------------------------------------------------------

landbaseRuleExceptions <- function() {
  
  list(
    
    # -------------------------------------------------------------------------
    # Protected Area rules
    # -------------------------------------------------------------------------
    
    protectedArea = list(
      
      default = c(
        "0" = 2,
        "1" = 1,
        "2" = 1,
        "3" = 1,
        "4" = 1,
        "5" = 1,
        "6" = 1,
        "7" = 2,
        "8" = 1,
        "9" = 1
      ),
      
      # Optional jurisdiction-specific overrides
      jurisdiction = list(),
      
      # Optional SYU-specific overrides
      SYU = list()
    ),
    
    # -------------------------------------------------------------------------
    # Ownership rules
    # -------------------------------------------------------------------------
    
    ownership = list(
      
      default = c(
        "11"  = 2,
        "12"  = 2,
        "13"  = 2,
        "20"  = 1,
        "31"  = 0,
        "32"  = 0,
        "33"  = 1,
        "40"  = 1,
        "50"  = 0,
        "100" = 0
      ),
      
      # Optional jurisdiction-specific overrides
      jurisdiction = list(),
      
      # Optional SYU-specific overrides
      SYU = list()
    )
  )
}