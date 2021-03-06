# Nexus integration tests

Starts the delta ecosystem with docker-compose and run tests on it

Relies on sbt-docker-compose: 
https://github.com/Tapad/sbt-docker-compose

## Archived

The tests have been ported to the [BlueBrain/nexus](https://github.com/BlueBrain/nexus) repository.

To run the all the tests:
```sbtshell
dockerComposeTest skipBuild
```

To reuse a docker-compose instance:
```
dockerComposeUp skipBuild
```
Which will gives an instance id to run tests:
```sbtshell
dockerComposeTest <instance_id>
```
All tests are designed to be run several times in a row without having to start and stop the docker-compose instance

To run just some tests, we can just provide tags:
```sbtshell
dockerComposeTest <instance_id> -tags:tag1,tag2
```

The available tags are:
* Iam
* Realms
* Permissions
* Acls
* Orgs
* Projects
* Archives
* Resources
* Views
* CompositeViews
* Events
* Storage
* AppInfo

## Funding & Acknowledgment

The development of this software was supported by funding to the Blue Brain Project, a research center of the École polytechnique fédérale de
Lausanne (EPFL), from the Swiss government's ETH Board of the Swiss Federal Institutes of Technology.

Copyright © 2015-2021 Blue Brain Project/EPFL

