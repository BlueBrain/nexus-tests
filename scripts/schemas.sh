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
    r)
      USER_REALM=$OPTARG
      ;;
    s)
      USER_SUB=$OPTARG
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

# Add organizations
for org in `cat ${BASE}/order-schemas.txt | cut -d'/' -f1 | sort -r | uniq`; do
	curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPUT -H "Content-Type: application/json" "${ADMIN_BASE}/orgs/$org" -d '{"name": "some"}'
done




# Add projects
for proj in `cat ${BASE}/order-schemas.txt | cut -d'/' -f1,2 | sort -r | uniq`; do
	curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPUT -H "Content-Type: application/json" "${ADMIN_BASE}/projects/$proj" -d '{"name": "some"}'
done

sleep 15

# Add context
curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPOST -H "Content-Type: application/json" "${KG_BASE}/resources/neurosciencegraph/core/resource" -d @${BASE}/contexts/neurosciencegraph/core/schema.json

# Add cross project and in account resolvers for organization neurosciencegraph
for proj in `echo "core commons electrophysiology experiment"`; do
	curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPOST -H "Content-Type: application/json" "${KG_BASE}/resolvers/neurosciencegraph/$proj" -d '{"@id": "http://example.com/neurosciencegraph/'$proj'/cross/resolver", "@type": ["Resolver", "CrossProject"], "projects": ["nexus/schemaorgsh", "nexus/schemaorg", "nexus/provsh", "nexus/core"], "identities": [{"@type": "UserRef", "realm": "'${USER_REALM}'", "sub": "'${USER_SUB}'"} ], "priority": 20 }'
	curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPOST -H "Content-Type: application/json" "${KG_BASE}/resolvers/neurosciencegraph/$proj" -d '{"@id": "http://example.com/neurosciencegraph/'$proj'/account/resolver", "@type": ["Resolver", "InAccount"], "identities": [{"@type": "UserRef", "realm": "'${USER_REALM}'", "sub": "'${USER_SUB}'"} ], "priority": 10 }'
done

# Add in account resolver for organization nexus
for proj in `echo "schemaorgsh schemaorg provsh core"`; do
	curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPOST -H "Content-Type: application/json" "${KG_BASE}/resolvers/nexus/$proj" -d '{"@id": "http://example.com/nexus/'$proj'/account/resolver", "@type": ["Resolver", "InAccount"], "identities": [{"@type": "UserRef", "realm": "'${USER_REALM}'", "sub": "'${USER_SUB}'"} ], "priority": 10 }'
done

sleep 10

# Add schemas
for i in `cat ${BASE}/order-schemas.txt`; do
	org=`echo $i | cut -d'/' -f1`
	proj=`echo $i | cut -d'/' -f2`
	curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPOST -H "Content-Type: application/json" "${KG_BASE}/schemas/$org/$proj" -d @${BASE}/schemas/$i
done


# Create org for test projects
curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPUT -H "Content-Type: application/json" "${ADMIN_BASE}/orgs/perftestorg" -d '{"name": "some"}'


# Create test projects
for i in $(seq 1 $NUM_PROJECTS); do
    curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPUT -H "Content-Type: application/json" "${ADMIN_BASE}/projects/perftestorg/perftestproj$i" -d '{"name": "some"}'
done


# Create resolvers test projects
for i in $(seq 1 $NUM_PROJECTS); do
    curl -w "${RESP_LOG}" -H "Authorization: Bearer $TOKEN" -XPOST -H "Content-Type: application/json" "${KG_BASE}/resolvers/perftestorg/perftestproj$i" -d '{"@id": "http://example.com/neurosciencegraph/'$proj'/cross/resolver", "@type": ["Resolver", "CrossProject"], "projects": ["nexus/schemaorgsh", "nexus/schemaorg", "nexus/provsh", "nexus/core", "neurosciencegraph/commons", "neurosciencegraph/core", "neurosciencegraph/electrophysiology", "neurosciencegraph/experiment"], "identities": [{"@type": "UserRef", "realm": "'${USER_REALM}'", "sub": "'${USER_SUB}'"} ], "priority": 20 }'
done
