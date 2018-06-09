import sys,os

# if ldebug=True: a few information about your environment are printed
ldebug = False

# A few test about your environment
if ldebug:
    print '-'*60
    print '-'*10, 'Debug information from configmain.py'

# Directory where EMS is installed
REP_EMS = os.getenv('REP_EMS')
if REP_EMS is None:
    print "REP_EMS is not defined in your environnement"
    sys.exit()

if ldebug:
    print "REP_EMS:", REP_EMS

# Directory where MUSC simulations are installed and run
REP_MUSC = os.getenv('REP_MUSC')
if REP_MUSC is None:
    print "REP_MUSC is not defined in your environnement"
    sys.exit()

if ldebug:
    print "REP_MUSC:", REP_MUSC

PYTHONPATH = os.getenv('PYTHONPATH')
if ldebug:
    print "PYTHONPATH:", PYTHONPATH


# ecoclimap files
ecoclimap = REP_EMS + '/UTIL/ecoclimap_cnrm_cm6.02'

# If True, everything is cleanup before reinstalling/rerunning a setup 
loverwrite = True #False
# If True, only an update is performed (not yet working well)
lupdate = True #False

if ldebug:
    print 'Your Environment for MUSC Simulations (EMS) is installed in', REP_EMS
    print 'MUSC simulations are installed in', REP_MUSC
    print 'ecoclimap files are in', ecoclimap
    print 'loverwrite:', loverwrite
    print 'lupdate:', lupdate
    print '-'*60

