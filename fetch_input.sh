#!/bin/bash

RED='\033[0;31m'
NC='\033[0m'

YEAR=2020
SESSION=$(cat ~/.aoc/credentials)

if [[ -z "${SESSION}" ]]; then
  echo -e "${RED}Error fetching credentials, add your session token to ~/.aoc/credentials${NC}"
  exit 1
fi

if [[ -z $1 ]]; then
  echo -e "${RED}Usage: $0 <day>${NC}"
  exit 1
fi

if [[ $1 < 1 || $1 > 25 ]]; then
  echo -e "${RED}Please enter a day between 1-25.${NC}"
  exit 1
fi

DAY=$(echo "$1" | sed "s/0//g")
curl --cookie "session=${SESSION}" https://adventofcode.com/${YEAR}/day/${DAY}/input > inputs/input${DAY}.txt 2>/dev/null
