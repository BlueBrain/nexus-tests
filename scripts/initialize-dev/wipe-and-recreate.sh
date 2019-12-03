#!/usr/bin/env bash

GREEN="\033[1;32m"
RESET="\033[0m"
# Scale down services
echo -e "${GREEN}Scaling down services${RESET}"
oc project bbp-nexus-dev
oc scale statefulset kg --replicas=0
oc scale statefulset admin --replicas=0
oc scale statefulset iam --replicas=0

# Make sure the pods are deleted
oc wait pods/kg-0 --for=delete --namespace=bbp-nexus-dev --timeout=3m || true
oc wait pods/admin-0 --for=delete --namespace=bbp-nexus-dev --timeout=3m || true
oc wait pods/iam-0 --for=delete --namespace=bbp-nexus-dev --timeout=3m || true

# Fetch cassandra user and password from the Openshift secrets
c_username=`oc get secret cassandra -o=jsonpath='{.data.username}' | base64 --decode`
c_password=`oc get secret cassandra -o=jsonpath='{.data.password}' | base64 --decode`

# Truncate cassandra tables
echo -e "${GREEN}Truncating cassandra tables${RESET}"
for keyspace in admin iam
do
  for table in tag_views tag_scanning metadata messages tag_write_progress
  do
    current_truncate="TRUNCATE ${keyspace}.${table};"
    truncate_cmd=$truncate_cmd$current_truncate
  done
done

for table in tag_views tag_scanning projections_progress metadata messages projections_failures tag_write_progress
do
  current_truncate="TRUNCATE kg.${table};"
  truncate_cmd=$truncate_cmd$current_truncate
done
oc rsh cassandra-0 cqlsh -u $c_username -p $c_password cassandra-0 -e "$truncate_cmd"

# Restore cassandra tables
echo -e "${GREEN}Restoring cassandra tables${RESET}"
oc rsh cassandra-0 rm -rf /tmp/cassandra_dump
oc rsync "${BASH_SOURCE%/*}/cassandra_dump" cassandra-0:/tmp/
oc rsh cassandra-0 bash /tmp/cassandra_dump/restore.sh $c_username $c_password

# Delete ElasticSearch dev indices
echo -e "${GREEN}Deleting ElasticSearch indices${RESET}"
curl -s -XDELETE 'http://elasticsearch.dev.nexus.ocp.bbp.epfl.ch/kg_*'

# Delete Sparql dev namespaces
echo -e "${GREEN}Deleting Sparql namespaces${RESET}"
for i in `curl -s 'http://blazegraph.dev.nexus.ocp.bbp.epfl.ch/blazegraph/namespace?describe-each-named-graph=false' | grep sparqlEndpoint | grep -o --color "rdf:resource=\"[^\"]*" | sed 's/rdf:resource="//' | sed 's#/sparql$##' | grep -v kb | grep -v LBS`
 do curl -s -XDELETE "$i"
done

# Scale up services
echo -e "${GREEN}Scaling up services${RESET}"
oc scale statefulset iam --replicas=1
oc wait pods/iam-0 --for condition=ready --namespace=bbp-nexus-dev --timeout=4m
oc scale statefulset admin --replicas=1
oc wait pods/admin-0 --for condition=ready --namespace=bbp-nexus-dev --timeout=4m
oc scale statefulset kg --replicas=1
oc wait pods/kg-0 --for condition=ready --namespace=bbp-nexus-dev --timeout=4m
oc rsync "${BASH_SOURCE%/*}/delete_files" kg-0:/tmp/
oc rsh kg-0 bash /tmp/delete_files/delete_files.sh
until curl -s http://kg.dev.nexus.ocp.bbp.epfl.ch | grep '"name":"kg"' &> /dev/null; do echo "Waiting until kg service is up"; sleep 2; done

echo -e "${GREEN}Services are up and backed up with the following configuration: ${RESET}"
echo "- Users authenticated in BBP or Github have full permissions on /"
echo "- Project neurosciencegraph/datamodels containing 174 resources"
echo "- Project bbp/nmc containing 3754 resources"
echo "- Project nse/gpfs containing 5 resources"
