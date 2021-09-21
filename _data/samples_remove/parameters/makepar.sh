touch $1.par

echo "resroot	$1" > $1.par
echo "dirroot	$2" >> $1.par
echo "sim_time	$3" >> $1.par
saveint=`expr $3 / 10`; echo "saveint	$saveint" >> $1.par
massint=`expr $3 / 100`; echo "massint	$massint" >> $1.par
echo "initial_tstep	$4" >> $1.par
echo "" >> $1.par

echo "DEMfile	$5" >> $1.par
echo "SGCbank	$5" >> $1.par
if [ ! -z "$6" ]; then echo "SGCwidth	$6" >> $1.par; fi
if [ ! -z "$7" ]; then echo "SGCn	$7" >> $1.par; fi
if [ ! -z "$8" ]; then echo "SGCr	$8" >> $1.par; fi
echo "" >> $1.par

echo "bcifile	$9" >> $1.par
if [ ! -z "${10}" ]; then echo "bdyfile	${10}" >> $1.par; fi
if [ ! -z "${11}" ]; then echo "startfile	${11}" >> $1.par; fi
if [ ! -z "${12}" ]; then echo "manningfile	${12}" >> $1.par; fi
if [ ! -z "${13}" ]; then echo "fpfric	${13}" >> $1.par; fi
echo "" >> $1.par

echo "acceleration" >> $1.par
echo "elevoff" >> $1.par
echo "depthoff" >> $1.par
#echo "debug" >> $1.par
#echo "checkpoint" >> $1.par

#rename par file
#mv test.par $1.par
