#!/usr/bin/env zsh
set -e

script_dir=${0:a:h}
frontend_bin=${1:a}
test_name=${2}

temp_dir=$(mktemp -d)

echo "Temp dir: $temp_dir"

(cd $temp_dir && $frontend_bin setup -P pk -V vk -C "$script_dir/$test_name/$test_name.r1cs")

(cd $temp_dir && $frontend_bin prove -P pk -V vk -x "$script_dir/$test_name/$test_name.x" -w "$script_dir/$test_name/$test_name.w" -p proof)

(cd $temp_dir && $frontend_bin verify -V vk -x "$script_dir/$test_name/$test_name.x" -p proof | grep "Verification status: 1")

#rm -r $temp_dir
