#!/usr/bin/env bash

goodCount=0
goodOk=0
echo -e "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
echo -e "Running good examples. No errors are expected.\n"
for e in examples/good/*.tms; do
  ((goodCount=goodCount+1))
  echo -e "----<=========================================>----"
  echo -e "Results of test $e:\n"
  ./src/interpreter "$e"
  if [[ $? -eq 0 ]]; then
    ((goodOk=goodOk+1))
    echo -e "\n\n< OK >\n\n"
  else
    echo -e "\n\n< NOT OK >\n\n"
  fi
done

badCount=0
badOk=0
echo -e "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
echo -e "Running incorrect examples. Should produce error.\n"
for e in examples/bad/*.tms; do
  ((badCount=badCount+1))
  echo -e "----<=========================================>----"
  echo -e "Results of test $e:\n"
  ./src/interpreter "$e"
  if [[ $? -eq 1 ]]; then
    ((badOk=badOk+1))
    echo -e "\n\n< OK >\n\n"
  else
    echo -e "\n\n< NOT OK >\n\n"
  fi
done

echo -e "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
echo -e "Good passed: $goodOk / $goodCount"
echo -e "Bad passed:  $badOk / $badCount"
