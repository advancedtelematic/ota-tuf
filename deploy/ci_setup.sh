#!/bin/bash

set -u

docker rm --force ota_tuf-mariadb
docker rm --force tuf-vault

# Some jobs don't behave, nuke them all
if [[ `docker ps -q | wc -l` -gt 0 ]]; then
    docker ps -q | xargs docker rm --force
fi

if [[ ! -d "ota_tuf_entrypoint.d/" ]]; then
    mkdir --verbose ota_tuf_entrypoint.d/
fi

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

function docker_vault {
    id=$(docker ps | grep tuf-vault | awk {'print $1'})
    docker exec \
           -e VAULT_TOKEN=$VAULT_TOKEN \
           -e VAULT_ADDR='http://0.0.0.0:8200' $id vault $*
}

function start_vault {
    docker run -d \
           --name tuf-vault \
           -e SKIP_SETCAP=true \
           -e VAULT_DEV_ROOT_TOKEN_ID=$VAULT_TOKEN \
           --volume $(pwd)/keyserver/src/main/resources:/tmp/resources \
           -p 8200:8200 \
           -d vault
}

function setup_vault {
    docker_vault policy-write ota-tuf /tmp/resources/vault_policy.hcl
    sleep 1s

    docker_vault mount -path=ota-tuf/keys generic
    sleep 1s

    docker_vault token-create -policy ota-tuf -id="74e0216d-cc15-5ab9-454d-908a04d14787"
}

function ensure_vault_running {
    local tries=20
    local timeout=10s

    sleep 5s

    for t in `seq $tries`; do
        res=$(curl --silent --fail 127.0.0.1:8200)
        if [[ $? == 22 ]]; then
            echo "vault is ready"

            setup_vault
            break
        else
            echo "Vault not ready, trying again"
            docker logs tuf-vault
            docker rm --force tuf-vault
            docker rm --force crypto-vault

            sleep $timeout
            start_vault
        fi
    done

    res=$(curl --silent --fail 127.0.0.1:8200)
    if [[ $? != 22 ]]; then
        echo "Error, could not start vault"
        exit -1;
    fi
}

function start_mariadb() {
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
}

function mysqladmin_alive {
    docker run \
           --rm \
           --link ota_tuf-mariadb \
           mariadb:10.1 \
           mysqladmin ping --protocol=TCP -h ota_tuf-mariadb -P 3306 -u root -proot
}

function wait_for_mysql {
    local tries=60
    local timeout=1s

    for t in `seq $tries`; do
        res=$(mysqladmin_alive)
        if [[ $res =~ "mysqld is alive" ]]; then
            echo "mysql is ready"
            echo
            break
        else
            echo "Waiting for mariadb"
            echo
            sleep $timeout
        fi
    done
}

start_mariadb

start_vault

ensure_vault_running

wait_for_mysql
