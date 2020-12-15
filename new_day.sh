#!/bin/bash

RED='\033[0;31m'
NC='\033[0m'

YEAR=2020
DAY=$1

if [[ -z $DAY ]]; then
  echo -e "${RED}Usage: $0 <day>${NC}"
  exit 1
fi

if [[ $DAY -lt 1 || $DAY -gt 25 ]]; then
  echo -e "${RED}Please enter a day between 1-25.${NC}"
  exit 1
fi

INPUT_DAY=$(printf "%02d" $DAY)
TRIMMED_DAY=$(echo "$DAY" | sed "s/^0//g")

if [[ ! -f "./execs/Day${INPUT_DAY}.hs" ]]; then
  NAME=$(curl https://adventofcode.com/$YEAR/day/$TRIMMED_DAY | grep -Eoh "Day [0-9]+:\s*(.+)\s*---" | sed -E 's/^Day [0-9]+:\s*//g' | sed 's/\s*---$//g' | awk '{$1=$1};1')
  hbs -D "{\"XX\": $TRIMMED_DAY, \"NAME\": \"$NAME\"}" -s ./templates/DayXX.hs.template > ./execs/Day$INPUT_DAY.hs

else
  echo "Solution file exists, humbly declining to create"
fi
