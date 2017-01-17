# Ota-tuf

## Running

You'll need vault:
    
    docker run -it -e VAULT_DEV_ROOT_TOKEN_ID=f8c637c5-b762-e6a7-7974-bf45d3061106 -p 8200:8200 vault
    vault mount -path=ota-tuf/keys generic

## Running tests

You'll need a mariadb instance running with the users configured in
`application.conf`. If you want it quick you can use
`deploy/ci_setup.sh`. This will create a new docker container running
a database with the proper permissions.

To run tests simply run `sbt test`.


## Teamcity jobs

In the `deploy` directory there are some scripts you can use to setup
the jobs in Teamcity.
