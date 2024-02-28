files='
Axis.py                 __init__.py             plotbasics.py
Case.py                 attributes.py           thermo.py
Variable.py             constants.py            variables_attributes.py
'

for f in $files
do
    gvim -d $f /Users/romain/dev/DEPHY-SCM/dephycf/$f
done
