#!/bin/bash

function setup
{
  sudo apt-get install wmctrl xautomation
}

set -e
function build_pico8_html
{
  local source_file=$1
  local workspace=$(mktemp -d)
  local output_dir=$2

  echo ">> Building PICO-8 HTML export"
  echo "SOURCE_FILE = ${source_file}"
  echo "PICO8       = ${pico8_binary}"
  echo "WORKSPACE   = ${workspace}"
  echo "OUTPUT_DIR  = ${output_dir}"
  sleep 1

  set -x

  mkdir -p $workspace/carts

  $pico8_binary -home $workspace -pixel_perfect 1 -windowed 1 &
  PICO_PID=$!
  echo $PICO_PID

  sleep 1
  cp "${source_file}" $workspace/carts/

  cart_name=$(basename ${source_file})


  sleep 1
  xte "str load ${cart_name}"
  xte "key Return"
  sleep 1
  send_command "run"
  sleep 1
  xte "key F7"
  sleep 1
  xte "key Escape"
  sleep 1
  send_command "save ${cart_name}.png"
  send_command "export ${cart_name}.html ${cart_name}"
  #xte "str save ${source_file}.html"
  #xte "key Return"



  sleep 1

  kill -15 $PICO_PID


  cp ${workspace}/carts/* $2/

  echo "Done"
  return 
}


function send_command
{
  xte "str $1"
  xte "key Return"
  sleep 1
}


pico8_binary="${HOME}/Documents/p8workspace/pico8"

output_workspace=${CIRCLE_ARTIFACTS:-~/tmp}


build_pico8_html $1 ${output_workspace}
