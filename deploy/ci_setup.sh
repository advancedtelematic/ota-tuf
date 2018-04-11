#!/bin/bash

set -u

docker rm --force ota_tuf-mariadb

# Some jobs don't behave, nuke them all
if [[ `docker ps -q | wc -l` -gt 0 ]]; then
    docker ps -q | xargs docker rm --force
fi

if [[ ! -d "ota_tuf_entrypoint.d/" ]]; then
    mkdir -v ota_tuf_entrypoint.d/
fi

echo "
create user 'tuf_repo' identified by 'tuf_repo';
CREATE DATABASE tuf_repo;
GRANT ALL PRIVILEGES ON \`tuf_repo%\`.* TO 'tuf_repo'@'%';

CREATE DATABASE ota_tuf;
GRANT ALL PRIVILEGES ON \`ota_tuf%\`.* TO 'ota_tuf'@'%';

create user 'tuf_ca' identified by 'tuf_ca';
CREATE DATABASE tuf_ca;
GRANT ALL PRIVILEGES ON \`tuf_ca%\`.* TO 'tuf_ca'@'%';

FLUSH PRIVILEGES;
" > ota_tuf_entrypoint.d/db_user.sql

MYSQL_PORT=${MYSQL_PORT-3306}

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

wait_for_mysql
