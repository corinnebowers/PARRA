#!/bin/bash

touch $1.par

echo "resroot	$1" > $1.par
echo "dirroot	$2" >> $1.par
echo "sim_time	$3" >> $1.par
saveint=`expr $3 / 10`; echo "saveint	$saveint" >> $1.par
echo "massint	900" >> $1.par
echo "initial_tstep	$4" >> $1.par
echo "" >> $1.par

echo "DEMfile	$5" >> $1.par
echo "SGCbank	$5" >> $1.par
echo "SGCwidth	$6" >> $1.par
if [ ! -z "$7" ]; then echo "SGCn	$7" >> $1.par; fi
if [ ! -z "$8" ]; then echo "SGCr	$8" >> $1.par; fi
if [ ! -z "$9" ]; then echo "SGCp	$9" >> $1.par; fi
echo "" >> $1.par

echo "bcifile	${10}" >> $1.par
if [ ! -z "${11}" ]; then echo "bdyfile	${11}" >> $1.par; fi
if [ ! -z "${12}" ]; then echo "startfile	${12}" >> $1.par; fi
if [ ! -z "${13}" ]; then echo "manningfile	${13}" >> $1.par; fi
if [ ! -z "${14}" ]; then echo "fpfric	${14}" >> $1.par; fi
echo "" >> $1.par

echo "acceleration" >> $1.par
echo "elevoff" >> $1.par
echo "depthoff" >> $1.par

