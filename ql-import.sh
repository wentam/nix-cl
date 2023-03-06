#!/bin/sh

set -e

# releases.txt columns:
# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]

# systems.txt columns:
# project system-file system-name [dependency1..dependencyN]

if [ $# -lt 2 ]; then
  echo "Usage: $(basename $0) [path/to/releases.txt] [path/to/systems.txt]"
  echo "As of 2023-03-13, text files can be found through http://beta.quicklisp.org/dist/quicklisp.txt"
  exit 1;
fi

releases_file="$1"
systems_file="$2"

get_release_for_project() {
  project_name="$1"
  awk "{ if ( \$1 == \"$project_name\" ) { print } }" $releases_file | head -n1
}

echo "# These are automatically generated packages - see cl.nix for human-created packages"
echo
echo "{ generic-builders, pkgs, cl, ... }: let"
echo "  build-asdf-system      = generic-builders.build-asdf-system;"
echo "  build-quicklisp-system = generic-builders.build-quicklisp-system;"
echo "  bqs = build-quicklisp-system;"
echo "in with cl; {"
while read -r line; do
  if [[ $(echo $line | head -c1) == "#" ]]; then continue; fi

  project_name="$(echo $line | cut -d' ' -f1)"
  system_file="$(echo $line | cut -d' ' -f2)"
  system_name="$(echo $line | cut -d' ' -f3)"
  deps="$(echo $line | cut -d' ' -f4-)"

  converted_deps=""
  for dep in $deps; do
    conv=$(echo "$dep" | sed "s|\.|-dot-|g" | sed "s|+|-plus-|g" | sed "s|/|-slash-|g" \
     | sed "s|^\([0-9]\)|cl-\\1|" | sed 's/^asdf$//g')
    converted_deps="$converted_deps $conv";
  done

  converted_deps="$(echo $converted_deps | awk '{$1=$1};1')"

  release_line="$(get_release_for_project "$project_name")"

  #name="$(echo $release_line | cut -d' ' -f1)"
  url="$(echo $release_line | cut -d' ' -f2)"
  size="$(echo $release_line | cut -d' ' -f3)"
  md5="$(echo $release_line | cut -d' ' -f4)"
  content_sha1="$(echo $release_line | cut -d' ' -f5)"
  version="$(echo $release_line | cut -d' ' -f6 | sed "s/^$project_name\(-\|_\)//i")"

  date="$(echo $url | sed "s|.*archive/${project_name}/||" | sed "s|/.*$||")";
  hash="$(nix hash to-sri "${md5}" --type md5 )";

  attr_name="$(echo $system_name | sed "s|\.|-dot-|g" | sed "s|+|-plus-|g" | sed "s|/|-slash-|g" | sed "s|^\([0-9]\)|cl-\\1|")";

  echo "  ${attr_name} = (bqs {"
  echo "    name = \"${system_name}\";"
  echo "    version = \"${version}\";"
  echo "    hash = \"$hash\";"
  echo "    url = \"$url\";"
  echo "    cl-deps = [ $converted_deps ];";
  echo "  });"

done < $systems_file

echo "}"

#echo "# These are automatically generated packages - see cl.nix for human-created packages"
#echo
#echo "{ generic-builders, pkgs, ... }: let"
#echo "  build-asdf-system      = generic-builders.build-asdf-system;"
#echo "  build-quicklisp-system = generic-builders.build-quicklisp-system;"
#echo "  bqs = build-quicklisp-system;"
#echo "in {"
#while read -r line; do
#  if [[ $(echo $line | head -c1) == "#" ]]; then continue; fi
#
#  name="$(echo $line | cut -d' ' -f1)"
#  url="$(echo $line | cut -d' ' -f2)"
#  size="$(echo $line | cut -d' ' -f3)"
#  md5="$(echo $line | cut -d' ' -f4)"
#  content_sha1="$(echo $line | cut -d' ' -f5)"
#  version="$(echo $line | cut -d' ' -f6 | sed "s/^$name-//")"
#
#  date="$(echo $url | sed "s|.*archive/${name}/||" | sed "s|/.*$||")";
#  hash="$(nix hash to-sri "${md5}" --type md5 )";
#
#  attr_name="$(echo $name | sed "s|\.|-dot-|g" | sed "s|+|-plus-|g" | sed "s|/|-|g" | sed "s|^\([0-9]\)|cl-\\1|")";
#
#  echo "  ${attr_name} = bqs { name = \"${name}\"; version=\"${version}\"; date=\"$date\"; hash=\"$hash\"; };"
#done < $releases_file
#echo "}"
