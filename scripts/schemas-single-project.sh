#!/usr/bin/env bash
set -eu

BASE=`dirname $0`

while getopts "a:k:t:r:s:n:" opt; do
  case $opt in
    a)
      ADMIN_BASE=$OPTARG
      ;;
    k)
      KG_BASE=$OPTARG
      ;;
    t)
      TOKEN=$OPTARG
      ;;
    n)
      NUM_PROJECTS=$OPTARG
      ;;

    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      exit 1
      ;;
  esac
done


RESP_LOG='\n%{url_effective} \n Status: %{http_code} \n Total: %{time_total}\n'

# Create org for test projects
curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPUT -H "Content-Type: application/json" "${ADMIN_BASE}/orgs/perftestorg" -d '{"name": "some"}'


# Create test projects
for i in $(seq 1 $NUM_PROJECTS); do
    curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPUT -H "Content-Type: application/json" "${ADMIN_BASE}/projects/perftestorg/perftestproj$i" -d '{"name": "some"}'
done

sleep 10

# Create schemas in  test projects
for i in $(seq 1 $NUM_PROJECTS); do
# Add context
    curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPOST -H "Content-Type: application/json" "${KG_BASE}/resources/perftestorg/perftestproj$i/resource" -d @${BASE}/contexts/neurosciencegraph/core/schema.json
    for s in `cat ${BASE}/order-schemas.txt`; do
	    curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPOST -H "Content-Type: application/json" "${KG_BASE}/schemas/perftestorg/perftestproj$i" -d @${BASE}/schemas/$s
    done
done
