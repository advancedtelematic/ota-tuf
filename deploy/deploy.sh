#!/bin/bash
set -e

if [[ "$1" == "" ]]; then
    echo "usage: $0 <docker_tag>"
    exit -1
fi

export DOCKER_TAG=$1
export JOB_NAME="${JOB_NAME-ota_tuf}"
export VAULT_ENDPOINT=${VAULT_ENDPOINT-$(echo $JOB_NAME | tr "-" "_")}
export IMAGE_NAME="ota_tuf"
export REGISTRY="advancedtelematic"
export IMAGE_ARTIFACT=${REGISTRY}/${IMAGE_NAME}:${DOCKER_TAG}
export USE_MEM="1024.0"
export USE_CPU="0.5"
export JVM_OPTS="-Xmx512m"

# Merge service environment variables with secrets from this vault endpoint.
export CATALOG_ADDR="http://catalog.gw.prod01.internal.advancedtelematic.com"

cat deploy/service.json |
    envsubst |
    curl --show-error --silent --fail \
         --header "X-Vault-Token: ${VAULT_TOKEN}" \
         --request POST \
         --data @- \
         ${CATALOG_ADDR}/service/${VAULT_ENDPOINT}
