#!/usr/bin/env csh

# This is a test script for avgpix.
# Pass the location of avgpix as a parameter, e.g.
# tstavgpix.csh $R2LIB
# or
# tstavgpix.csh .
# stdout/err is captured in tstavgpix.log

if ($1 == "") then
  echo 'Pass $R2LIB or other location of binary as first argument'
  echo 'ABORTING TEST ...'
  exit
endif

$R2LIB/gen a.vic ival=1 nl=10 ns=10 >& tstavgpix.log
$R2LIB/gen b.vic ival=3 nl=10 ns=10 >>& tstavgpix.log
$R2LIB/gen c.vic ival=11 nl=10 ns=10 >>& tstavgpix.log
$R2LIB/gen d.vic ival=0 linc=0 sinc=0 nl=10 ns=10 >>& tstavgpix.log

echo "#ignore this line\na.vic\nb.vic\nc.vic\nd.vic" > list.txt
echo "list.txt:" >>& tstavgpix.log
cat list.txt >>& tstavgpix.log
echo "list.txt (end)" >>& tstavgpix.log

$1/avgpix inp=a.vic out=avg.vic list=list.txt >>& tstavgpix.log

$R2LIB/label -list inp=avg.vic -nousrtim >>& tstavgpix.log
$R2LIB/list inp=a.vic -nousrtim >>& tstavgpix.log
$R2LIB/list inp=avg.vic -nousrtim >>& tstavgpix.log

rm a.vic b.vic c.vic d.vic list.txt avg.vic
