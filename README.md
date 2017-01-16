# ATS service Blueprint

Someone did it so you don't have to.

Build up on this template to create a new service in the ATS infrastructure.

## Running tests

You'll need a mariadb instance running with the users configured in
`application.conf`. If you want it quick you can use
`deploy/ci_setup.sh`. This will create a new docker container running
a database with the proper permissions.

To run tests simply run `sbt test`.


## Teamcity jobs

In the `deploy` directory there are some scripts you can use to setup
the jobs in Teamcity.
