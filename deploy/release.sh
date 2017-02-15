#!/bin/bash

set -e

export VAULT_ENDPOINT="ota_tuf_release"
export VDATA=$(curl -s -H "X-Vault-Token: ${VAULT_TOKEN}" -X GET ${VAULT_ADDR}/v1/secret/${VAULT_ENDPOINT})
export PUBLISH_USER="$(echo $VDATA | jq -r .data.publish_user)"
export PUBLISH_PASSWORD="$(echo $VDATA | jq -r .data.publish_password)"
export PUBLISH_REALM="Sonatype Nexus Repository Manager"
export PUBLISH_URL="http://nexus.advancedtelematic.com:8081/content/repositories/"
