rm -f module.te module.fc module.m4
echo INPUT: $1
cat $1
echo OUTPUT: $1
sl2cs $1 2>&1
ec=$?
m4 -E -I.. module.te.m4 | grep -v "^[ ]*$" > module.te
echo module.te: $1
cat module.te 2>&1
echo
m4 -I.. module.fc.m4 | grep -v "^[ ]*$" > module.fc
echo module.fc: $1
cat module.fc 2>&1
echo
rm -f module.te module.fc
exit $ec
