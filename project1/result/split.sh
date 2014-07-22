#! /bin/sh
i=0
cat result.txt | while read line
do
  i=`expr $i + 1`
  echo $line |awk '{print $3}' > problem_id_$i.txt
  echo "problem_id_$i.txt"
done
