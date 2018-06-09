import os

# Directory where post-processing files are to be found
dirpost = os.getenv('REP_EMS') + '/UTIL/post_DEPHY/'
#dirpost = configMUSC.mainrep + '/UTIL/post/'

# File describing the correspondance between in-model variables 
# and output postprocessed variables 
# + a few variable attribbutes (long_name and units)
variablesDict = 'variables.py' # Available: variables.py, variables_DEPHY.py

# File containing information for postprocessing:
# - variables to postprocess
# - possibly pressure and/or altitude levels 
#   on which 3D variables are to be interpolated
defaultConfigPost = 'config_default.py'

# If True, use a config file specific to each case,
# ie a file named 'config_CASE.py' where CASE is the case name
caseDependent = True
#caseDependent = False

