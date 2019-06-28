#!/bin/sh

set -e

host="$1"
shift

port="$1"
shift

additional_wait="$1"
shift

cmd="$@"

until nc -z ${host} ${port}
do
  echo "Service is still unavailable on: ${host}:${port}"
  sleep 1
done

if [[ $additional_wait -gt 0 ]]; then
	echo "waiting $additional_wait seconds more for the service"
	sleep ${additional_wait}
fi

echo "Service is up - executing command"
exec $cmd

