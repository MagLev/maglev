#!/usr/bin/env bash

# set -x
token="${1:-0}" ; shift
if [[ -z "$token" ]] ; then
  tiers="0"
elif [[ "$token" = "all" ]] ; then
  tiers="$(ls | awk '/^[0-9]+$/')"
elif [[ "$token" -ge 0 ]] ; then
  tiers="$token"
else
  echo -e "The first parameter must be either a positive integer for the tier #, 'all' for all tiers, or a comma separated list of tier #'s: 0,1'"
fi

if [[ ! -z "$*" ]] ; then
  rubies="$*"
else
  rubies="$(ls $rvm_rubies_path/ 2> /dev/null | awk '/[a-z]*-.*/' | grep -v system | sort)"
fi

for ruby in $rubies ; do
  if [[ -x $ruby ]] ; then
    list="$list $ruby"
  else
    list="$list $rvm_bin_path/$ruby"
  fi
done

timestamp="$(date +'%Y-%m-%d')"

for tier in $tiers ; do
  output_path="$(pwd)/runs/$timestamp/$tier"
  mkdir -p $output_path
  echo -e "\n[$timestamp]\n" > "$output_path/error.log"
  export NO_TIMEOUT=1
  ruby compare-rvm-nowarmup.rb $tier $list 2>> "$output_path/error.log" \
    | tee "$output_path/run.yaml"
  if [[ $? -eq 0 ]] ; then
    rake to_web NORMALIZE=time TIMESTAMP=$timestamp YAML_DIR=runs/$timestamp/$tier \
      OUTPUT_DIR=runs/$timestamp/$tier
    rake to_web NORMALIZE=speed TIMESTAMP=$timestamp YAML_DIR=runs/$timestamp/$tier \
      OUTPUT_DIR=runs/$timestamp/$tier
  fi
done

rake to_csv TIMESTAMP=$timestamp YAML_DIR=runs/$timestamp
