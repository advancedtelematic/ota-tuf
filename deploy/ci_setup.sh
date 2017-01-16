#!/bin/bash

set -u

docker rm --force ota_tuf-mariadb || true

mkdir ota_tuf_entrypoint.d/ || true

echo "
CREATE DATABASE ota_tuf;
GRANT ALL PRIVILEGES ON \`ota_tuf%\`.* TO 'ota_tuf'@'%';
FLUSH PRIVILEGES;
" > ota_tuf_entrypoint.d/db_user.sql

MYSQL_PORT=${MYSQL_PORT-3306}

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

