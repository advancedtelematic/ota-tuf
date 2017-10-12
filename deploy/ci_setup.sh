#!/bin/bash

set -u

docker rm --force ota_tuf-mariadb || true
docker rm --force tuf-vault || true

mkdir ota_tuf_entrypoint.d/ || true

echo "
create user 'tuf_repo' identified by 'tuf_repo';
CREATE DATABASE tuf_repo;
GRANT ALL PRIVILEGES ON \`tuf_repo%\`.* TO 'tuf_repo'@'%';
FLUSH PRIVILEGES;

CREATE DATABASE ota_tuf;
GRANT ALL PRIVILEGES ON \`ota_tuf%\`.* TO 'ota_tuf'@'%';
FLUSH PRIVILEGES;
" > ota_tuf_entrypoint.d/db_user.sql

MYSQL_PORT=${MYSQL_PORT-3306}

VAULT_TOKEN=f8c637c5-b762-e6a7-7974-bf45d3061106

docker run -d \
       --cap-add IPC_LOCK \
       --name tuf-vault \
       -e VAULT_DEV_ROOT_TOKEN_ID=$VAULT_TOKEN \
       --volume $(pwd)/keyserver/src/main/resources:/tmp/resources \
       -p 8200:8200 vault

function docker_vault() {
    id=$(docker ps | grep tuf-vault | awk {'print $1'})
    sleep 5
    docker exec \
           -e VAULT_TOKEN=$VAULT_TOKEN \
           -e VAULT_ADDR='http://0.0.0.0:8200' $id vault $*
}

docker run -d \
  --name ota_tuf-mariadb \
  -p $MYSQL_PORT:3306 \
  -v $(pwd)/ota_tuf_entrypoint.d:/docker-entrypoint-initdb.d \
  -e MYSQL_ROOT_PASSWORD=root \
  -e MYSQL_USER=ota_tuf \
  -e MYSQL_PASSWORD=ota_tuf \
  mariadb:10.1 \
  --character-set-server=utf8 --collation-server=utf8_unicode_ci \
  --max_connections=1000

function mysqladmin_alive {
    docker run \
           --rm \
           --link ota_tuf-mariadb \
           mariadb:10.1 \
           mysqladmin ping --protocol=TCP -h ota_tuf-mariadb -P 3306 -u root -proot
}

docker_vault policy-write ota-tuf /tmp/resources/vault_policy.hcl

docker_vault mount -path=ota-tuf/keys generic

docker_vault token-create -policy ota-tuf -id="74e0216d-cc15-5ab9-454d-908a04d14787"

TRIES=60
TIMEOUT=1s


for t in `seq $TRIES`; do
    res=$(mysqladmin_alive || true)
    if [[ $res =~ "mysqld is alive" ]]; then
        echo "mysql is ready"
        exit 0
    else
        echo "Waiting for mariadb"
        sleep $TIMEOUT
    fi
done

exit -1
