#!/bin/bash

docker rm --force ota_tuf-vault || true

docker run -d \
       --name ota_tuf-vault \
       -e VAULT_DEV_ROOT_TOKEN_ID=f8c637c5-b762-e6a7-7974-bf45d3061106 \
       -p 8200:8200 vault

function docker_vault() {
    docker run --cap-add IPC_LOCK \
           --link ota_tuf-vault \
           -e VAULT_TOKEN=f8c637c5-b762-e6a7-7974-bf45d3061106 \
           -e VAULT_ADDR=http://ota_tuf-vault:8200 \
           --volume $(pwd)/src/main/resources:/tmp/resources \
           vault $*
}

docker_vault policy-write ota-tuf /tmp/resources/vault_policy.hcl

docker_vault mount -path=ota-tuf/keys generic

docker_vault token-create -policy ota-tuf -id="74e0216d-cc15-5ab9-454d-908a04d14787"
