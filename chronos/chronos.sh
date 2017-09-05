#!/bin/bash
set -e

export JOB_NAME="tuf_reposerver_refresh_timestamps"
export DOCKER_IMAGE="advancedtelematic/tuf-reposerver"

export TUF_KEYSERVER_HOST=$TUF_KEYSERVER_HOST
export TUF_KEYSERVER_PORT=$TUF_KEYSERVER_PORT
export DB_MIGRATE=$DB_MIGRATE
export DB_PASSWORD=$DB_PASSWORD
export DB_URL=$DB_URL
export DB_USER=$DB_USER

export SCHEDULE="R/2017-02-01T18:00:00Z/P1D"

REQ=$(envsubst < job.json)
CHRONOS_HOST="sched0.prod01.internal.advancedtelematic.com:9090"
CHRONOS_ENDPOINT="/v1/scheduler/iso8601"

echo $REQ | jq .

curl -L \
     -H 'Content-Type: application/json' \
     -X POST \
     -d"$REQ" \
"${CHRONOS_HOST}${CHRONOS_ENDPOINT}"
