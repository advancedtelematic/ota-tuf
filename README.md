# Ota-tuf

## Running

You'll need vault installed (v0.5.2):

    vault server -dev -dev-root-token-id="f8c637c5-b762-e6a7-7974-bf45d3061106"
    export VAULT_TOKEN="f8c637c5-b762-e6a7-7974-bf45d3061106"
    vault policy-write ota-tuf src/main/resources/vault_policy.hcl
    vault mount -path=ota-tuf/keys generic

    vault token-create -format json -policy ota-tuf -id="74e0216d-cc15-5ab9-454d-908a04d14787"
    
There is a script to run these commands and start a vault instance in `deploy/dev-vault.sh`

## Running tests

You'll need a mariadb instance running with the users configured in
`application.conf`. If you want it quick you can use
`deploy/ci_setup.sh`. This will create a new docker container running
a database with the proper permissions.

To run tests simply run `sbt test`.

To run integration tests you will also need a running instance of
vault, see above.

    sbt it:test


## Teamcity jobs

In the `deploy` directory there are some scripts you can use to setup
the jobs in Teamcity.
