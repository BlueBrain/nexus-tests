#!/usr/bin/env bash

GREEN="\033[1;32m"
RED="\033[1;31m"
RESET="\033[0m"

projects=()
orgs=()

function usage {
  echo "Usage: $0 -e ENV [-p project] [-h] [-o organization]"
  echo "ENV: 'dev', 'staging' or 'sandbox'"
}

while getopts "e:p:o:h" OPTION; do
  case $OPTION in
    e)
      env_label=$OPTARG
      [[ ! $env_label =~ dev|staging|sandbox ]] && {
          echo -e "${RED}Environment '$env_label' not recognized.${RESET}"
          usage
          exit 1
      }
      ENV="$(echo "https://$env_label.nexus.ocp.bbp.epfl.ch/v1")"
      ELASTICSEARCH="$(echo "http://elasticsearch.$env_label.nexus.ocp.bbp.epfl.ch")"
      BLAZEGRAPH="$(echo "http://blazegraph.$env_label.nexus.ocp.bbp.epfl.ch/blazegraph")"
      OC_PROJECT="$(echo "bbp-nexus-$env_label")"
      ;;
    p)
      projects+=("$OPTARG")
      ;;
    o)
      orgs+=("$OPTARG")
      ;;
    h)
      usage
      exit 0
      ;;
    *)
      usage
      exit 1
      ;;
  esac
done

if [ -z "$ENV" ] || [ ${#projects[@]} -eq 0 ] && [ ${#orgs[@]} -eq 0 ]; then
  usage
  exit 1
fi

function runIfNotEmpty {
  command=$1
  if [ ! -z "$command" ] || [ "$command" !== "DELETE" ]; then oc rsh cassandra-0 cqlsh -u $c_username -p $c_password cassandra-0 -e "$command";fi
}

function fetch_project_uuid {
  project=$1
  curl -H "Authorization: Bearer $TOKEN" -s "$ENV/projects/$project" | jq "._uuid" | tr -d '"'
}

function fetch_org_uuid {
  org=$1
  curl -H "Authorization: Bearer $TOKEN" -s "$ENV/orgs/$org" | jq "._uuid" | tr -d '"'
}

function prepare_delete_cassandra_rows_commands {
  keyspace=$1
  persistence_id=$2
  partition_nr=$3
  delete_messages_cmd=$(echo "DELETE FROM $keyspace.messages WHERE persistence_id='$persistence_id' AND partition_nr=$partition_nr;$delete_messages_cmd")
  delete_tag_scanning_cmd=$(echo "DELETE FROM $keyspace.tag_scanning WHERE persistence_id='$persistence_id';$delete_tag_scanning_cmd")
  delete_tag_write_progress_cmd=$(echo "DELETE FROM $keyspace.tag_write_progress WHERE persistence_id='$persistence_id';$delete_tag_write_progress_cmd")
  oc rsh cassandra-0 cqlsh --no-color -u $c_username -p $c_password cassandra-0 -e "PAGING OFF;SELECT tag_name,timebucket,timestamp,tag_pid_sequence_nr FROM $keyspace.tag_views WHERE persistence_id='$persistence_id' ALLOW FILTERING" | sed 's/\ //g; /^----.*/d; /^(/d; /^Warning:/d; /^tag_name|timebucket/d; /^DisabledQuerypaging/d; /^[[:space:]]*$/d;' > curr_tag_view.csv
  for line2 in $(cat curr_tag_view.csv);do
    tag_name=$(echo $line2 | cut -d '|' -f1)
    timebucket=$(echo $line2 | cut -d '|' -f2)
    timestamp=$(echo $line2 | cut -d '|' -f3)
    tag_pid_sequence_nr=$(echo $line2 | cut -d '|' -f4)
    delete_tag_views_cmd=$(echo "DELETE FROM $keyspace.tag_views WHERE persistence_id='$persistence_id' AND tag_name='$tag_name' AND timebucket=$timebucket AND timestamp=$timestamp AND tag_pid_sequence_nr=$tag_pid_sequence_nr;$delete_tag_views_cmd")
  done
  rm -f curr_tag_view.csv
}

function init_cassandra_commands {
  delete_messages_cmd=""
  delete_tag_scanning_cmd=""
  delete_tag_write_progress_cmd=""
  delete_tag_views_cmd=""
}

function run_cassandra_commands {
  runIfNotEmpty "${delete_messages_cmd}"
  runIfNotEmpty "${delete_tag_scanning_cmd}"
  runIfNotEmpty "${delete_tag_write_progress_cmd}"
  runIfNotEmpty "${delete_tag_views_cmd}"
}

function addProjects {
  org=$1
  from=$2
  json="$(curl -H "Authorization: Bearer $TOKEN" -s "$ENV/projects/$org?from=$from&size=50")"
  cur_projects=($(echo $json | jq '.["_results"] | .[] | {label: (._organizationLabel + "/" + ._label)}' | sed '/"label"/!d' | cut -d'"' -f4))
  if [ ${#cur_projects[@]} -gt 0 ]; then
    for project in "${cur_projects[@]}"; do
      echo "Project '$project' belongs to provided organization '$org'. Adding it to the list of projects to delete"
      projects+=("$project")
    done
    addProjects "$org" $((from+50))
  fi
}

TOKEN="$(oc get secret nexus-sa -o=jsonpath='{.data.service-account-token}' | base64 --decode)"
for i in "${!orgs[@]}"; do
  org="${orgs[$i]}"
  org_uuid=$(fetch_org_uuid "$org")
  orgs[$i]="$org;;$org_uuid"
  addProjects "$org" 0
done

projects_count=0

for i in "${!projects[@]}"; do
  project="${projects[$i]}"
  project_uuid=$(fetch_project_uuid "$project")
  if [ -n "$project_uuid" ] && [ "$project_uuid" !== "null" ]; then
    ((projects_count++))
  fi
  projects[$i]="$project;;$project_uuid"
done

if [ $projects_count -eq 0 ]; then
  echo -e "${RED}No projects selected.${RESET}"
  usage
  exit 1
else
  oc project $OC_PROJECT
  c_username=$(oc get secret cassandra -o=jsonpath='{.data.username}' | base64 --decode)
  c_password=$(oc get secret cassandra -o=jsonpath='{.data.password}' | base64 --decode)
  oc scale statefulset kg --replicas=0
  oc scale statefulset admin --replicas=0
  oc wait pods/kg-0 --for=delete --timeout=3m || true > /dev/null
  oc wait pods/admin-0 --for=delete --timeout=3m || true > /dev/null
  echo -e "${GREEN}ADMIN and KG services have been scaled DOWN.${RESET}"

  for projectObt in "${projects[@]}"; do
    project=$(echo $projectObt | awk -F ';;' '{print $0}')
    project_uuid=$(echo $projectObt | awk -F ';;' '{print $1}')
    if [ -z "$project_uuid" ] || [ "$project_uuid" == "null" ]; then
      echo -e "${RED}Project $project was not found. Ignoring.${RESET}"
    else
      oc rsh cassandra-0 cqlsh --no-color -u $c_username -p $c_password cassandra-0 -e "PAGING OFF;SELECT persistence_id,partition_nr FROM kg.messages WHERE tags CONTAINS 'project=$project_uuid' ALLOW FILTERING" | sed 's/\ //g; /^----.*/d; /^(/d; /^Warning:/d; /^persistence_id|partition_nr/d; /^DisabledQuerypaging/d; /^[[:space:]]*$/d;' > output_kg.csv
      init_cassandra_commands
      for line in $(cat output_kg.csv);do
        persistence_id=$(echo $line | cut -d '|' -f1)
        partition_nr=$(echo $line | cut -d '|' -f2)
        prepare_delete_cassandra_rows_commands "kg" "$persistence_id" "$partition_nr"
      done
      run_cassandra_commands
      echo -e "${GREEN}Project $project was deleted from KG cassandra tables.${RESET}"

      persistence_id="projects-$project_uuid"
      oc rsh cassandra-0 cqlsh --no-color -u $c_username -p $c_password cassandra-0 -e "PAGING OFF;SELECT partition_nr FROM admin.messages WHERE persistence_id='$persistence_id' ALLOW FILTERING" | sed 's/\ //g; /^----.*/d; /^(/d; /^Warning:/d; /^partition_nr/d; /^DisabledQuerypaging/d; /^[[:space:]]*$/d;' > output_admin.csv
      init_cassandra_commands
      for partition_nr in $(cat output_admin.csv);do
        prepare_delete_cassandra_rows_commands "admin" "$persistence_id" "$partition_nr"
      done
      run_cassandra_commands
      echo -e "${GREEN}Project $project was deleted from ADMIN cassandra tables.${RESET}"

      curl -X DELETE -s -o /dev/null "$ELASTICSEARCH/*_${project_uuid}_*" > /dev/null
      echo -e "${GREEN}Project $project was deleted from ElasticSearch.${RESET}"


      for namespace_endpoint in $(curl -s "$BLAZEGRAPH/namespace?describe-each-named-graph=false" | grep -o "rdf:resource=\"[^\"]*" | sed "s/rdf:resource=\"//; s/\/sparql$//; /void#Dataset$/d; /LBS/d; /kb$/d; /$project_uuid/!d");do
        curl -s -o /dev/null -X DELETE "$namespace_endpoint" > /dev/null
      done
      echo -e "${GREEN}Project $project was deleted from Blazegraph.${RESET}"
      rm -f output_kg.csv output_admin.csv
    fi
  done

  for org in "${orgs[@]}"; do
    org_uuid=$(fetch_org_uuid "$org")
    if [ -z "$project_uuid" ] || [ "$project_uuid" == "null" ]; then
      echo -e "${RED}Organization $org was not found. Ignoring.${RESET}"
    else
      persistence_id="organizations-$project_uuid"
      oc rsh cassandra-0 cqlsh --no-color -u $c_username -p $c_password cassandra-0 -e "PAGING OFF;SELECT partition_nr FROM admin.messages WHERE persistence_id='$persistence_id' ALLOW FILTERING" | sed 's/\ //g; /^----.*/d; /^(/d; /^Warning:/d; /^partition_nr/d; /^DisabledQuerypaging/d; /^[[:space:]]*$/d;' > output_admin.csv
      init_cassandra_commands
      for partition_nr in $(cat output_admin.csv);do
        prepare_delete_cassandra_rows_commands "admin" "$persistence_id" "$partition_nr"
      done
      run_cassandra_commands
      echo -e "${GREEN}Organization $org was deleted from ADMIN cassandra tables.${RESET}"
      rm -f output_admin.csv
    fi
  done

  oc scale statefulset admin --replicas=1
  oc scale statefulset kg --replicas=1
  oc wait pods/admin-0 --for condition=ready --timeout=3m || true > /dev/null
  oc wait pods/kg-0 --for condition=ready --timeout=3m || true > /dev/null
  echo -e "${GREEN}ADMIN and KG services have been scaled UP.${RESET}"
fi


