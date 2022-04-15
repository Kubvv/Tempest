#!/usr/bin/env bash

interpreter=src/interpreter
examples_path=examples

echo -e "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
echo -e "Running good examples. No errors are expected.\n"

all_good=0
passed_good=0
for example in $examples_path/good/*.tms; do
  echo -e "----<=========================================>----"
  echo -e "Running $example:\n"
  ./$interpreter "$example"

  if [[ $? -eq 0 ]]; then
    ((passed_good=passed_good+1))
    echo -e "\n\n< OK >\n\n"
  else
    echo -e "\n\n< NOT OK >\n\n"
  fi
  ((all_good=all_good+1))
done

echo -e "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
echo -e "Running incorrect examples. Should produce error.\n"

all_bad=0
passed_bad=0
for example in $examples_path/bad/*.tms; do
  echo -e "----<=========================================>----"
  echo -e "Running $example:\n"
  ./$interpreter "$example"

  if [[ $? -eq 1 ]]; then
    ((passed_bad=passed_bad+1))
    echo -e "\n\n< OK >\n\n"
  else
    echo -e "\n\n< NOT OK >\n\n"
  fi
  ((all_bad=all_bad+1))
done

all_passed=$((passed_good + passed_bad))
all=$((all_good + all_bad))
echo -e "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
echo -e "Good passed: $passed_good / $all_good"
echo -e "Bad passed:  $passed_bad / $all_bad"
echo -e "----<=========================================>----"
echo -e "All passed:  $all_passed / $all"
