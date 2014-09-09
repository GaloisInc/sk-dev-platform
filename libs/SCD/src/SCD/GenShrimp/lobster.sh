rm -f module.te module.fc
echo INPUT: $1
cat $1
echo OUTPUT: $1
lobster $1 2>&1
ec=$?
echo module.te: $1
cat module.te 2>&1
echo
echo module.fc: $1
cat module.fc 2>&1
echo
rm -f module.te module.fc
exit $ec
