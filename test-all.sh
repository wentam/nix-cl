#!/bin/sh

if [ $# -lt 2 ]; then
  echo "Usage: $(basename $0) [path/to/releases.txt] [path/to/systems.txt]"
  echo "As of 2023-03-13, text files can be found through http://beta.quicklisp.org/dist/quicklisp.txt"
  exit 1;
fi

releases_file="$1"
systems_file="$2"

trap "trap - SIGTERM; kill -- -$$" SIGINT SIGTERM EXIT

list="$  pkgs/ql.nix";

tested=0
pass=0
fail=0


echo "-- begin --" > /tmp/fails

while read -r line; do
  if [[ $(echo $line | head -c1) == "#" ]]; then continue; fi

  project_name="$(echo $line | cut -d' ' -f1)"
  system_file="$(echo $line | cut -d' ' -f2)"
  system_name="$(echo $line | cut -d' ' -f3)"

  attr_name="$(echo $system_name | sed "s|\.|-dot-|g" | sed "s|+|-plus-|g" | sed "s|/|-slash-|g" | sed "s|^\([0-9]\)|cl-\\1|")";

  echo "Building cl.$attr_name"

  nix build ".#cl.$attr_name" >/dev/null 2>/dev/null
  if [ $? -eq 0 ]; then
   pass=$(($pass + 1))
  else
   fail=$(($fail + 1))
   echo "FAILURE: $attr_name"
   echo "$attr_name" >> /tmp/fails
  fi

  tested=$(($tested + 1))

  echo "$pass/$tested good";
  echo "$fail/$tested failed";
done < $systems_file
