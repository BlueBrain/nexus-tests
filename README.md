# Nexus integration and performance tests

## Runing performance tests

In order to set up the tests it's necessary to set up the projects. It can be done using `scripts/schemas.sh` or `scripts/schemas-single-project.sh`.
The first script is puts all the schemas in a single project and uses CrossProjectResolver to resolve them. The second puts the schemas in each project. Currently the second script is recommended due to performance concerns for cross-project resolution.
The scripts accepts the following parameter:
```
-a admin service base URL(including v1 prefix)
-k KG base URL (including v1 prefix)
-t token - token of a user who has permission to create organizations/projects/resources
-n number of projects to create
```

Running this script will create projects in organization `perftestorg` with names `perftestproj${i}` where `$i` is project number starting from 1.


Running the upload simulation

```
sbt  'gatling-it:testOnly ch.epfl.bluebrain.nexus.perf.UploadSimulation'
```


You can set following parameters via setting environment variables:

```
$KG_TOKEN - token
$KG_BASE - KG base URL (including v1 prefix)
$PROJECT_NUMBER - target project number where data is going to be uploaded
$UPLOAD_SIZE - number of resources that will be uploaded, down to the nearest multiple of 20
$PARALLEL_USERS - number of parallel users used by the simulation to upload the data.
```
