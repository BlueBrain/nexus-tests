#!/usr/bin/env python


import os
import sys
import json
import uuid
import pprint
import pytest
import requests


class TestSchema(object):
    """Test suite for testing 'Schemas'.
    """

    headers = {'Content-type': 'application/json'}

    @pytest.mark.dev
    def test1_schemas(self, domain_url, schema_url, uid):
        """Test to ...
        """

        # Create a new domain first for the schema
        payload = {
            "description": "Test domain"
        }

        rurl = os.path.join(domain_url, uid)
        response = requests.put(rurl, data=json.dumps(payload),
                                headers=self.headers)
        assert response.status_code == 201

        # Create a new schema
        #PUT /v0/schemas/{orgId}/{domId}/{name}/{version}
        surl = os.path.join(schema_url, uid ,'myschema', 'v1.0.0')

        # Define the schema here
        payload = {
            "@context": [
              {
                "this": "https://nexus.example.com/v0/schemas/nexus/schemaorg/quantitativevalue/v1.0.0/shapes/"
              },
              "https://bbp-nexus.epfl.ch/staging/v0/contexts/nexus/core/shacl20170720/v0.1.0"
            ],
            "@type": "owl:Ontology",
            "shapes": [
              {
                "@id": "this:QuantitativeValueShape",
                "@type": "sh:NodeShape",
                "description": "A point value or interval for product characteristics and other purposes.",
                "property": [
                  {
                    "path": "schema:unitCode",
                    "name": "Unit Code",
                    "description": "The unit of measurement given using the UN/CEFACT Common Code (3 characters) or a URL. Other codes than the UN/CEFACT Common Code may be used with a prefix followed by a colon.",
                    "or": [
                      {
                        "datatype": "xsd:anyURI"
                      },
                      {
                        "datatype": "xsd:string"
                      }
                    ]
                  },
                  {
                    "path": "schema:unitText",
                    "name": "Unit Text",
                    "description": "A string or text indicating the unit of measurement. Useful if you cannot provide a standard unit code for unitCode.",
                    "datatype": "xsd:string"
                  },
                  {
                    "path": "schema:value",
                    "name": "Value",
                    "description": "The value of the quantitative value or property value node.",
                    "datatype": "xsd:float"
                  }
                ]
              }
            ]
        }
        response = requests.put(surl, data=json.dumps(payload),
                                headers=self.headers)
        assert response.status_code == 201

        response = requests.get(surl)
        assert response.status_code == 200
        print response.text
        response = json.loads(response.text)
        current_rev = response['nxv:rev']
        assert response["@id"] == surl

        # Update a schema

        payload = {
            "@context": [
              {
                "this": "https://nexus.example.com/v0/schemas/nexus/schemaorg/quantitativevalue/v1.0.0/shapes/"
              },
              "https://bbp-nexus.epfl.ch/staging/v0/contexts/nexus/core/shacl20170720/v0.1.0"
            ],
            "@type": "owl:Ontology",
            "shapes": [
              {
                "@id": "this:QuantitativeValueShape",
                "@type": "sh:NodeShape",
                "description": "A measured length.",
                "property": [
                  {
                    "path": "schema:unitCode",
                    "name": "Unit Code",
                    "description": "The unit of measurement given using the UN/CEFACT Common Code (3 characters) or a URL. Other codes than the UN/CEFACT Common Code may be used with a prefix followed by a colon.",
                    "or": [
                      {
                        "datatype": "xsd:anyURI"
                      },
                      {
                        "datatype": "xsd:string"
                      }
                    ]
                  },
                  {
                    "path": "schema:unitText",
                    "name": "Unit Text",
                    "description": "Can be m or cm or whetever.",
                    "datatype": "xsd:string"
                  },
                  {
                    "path": "schema:value",
                    "name": "Value",
                    "description": "The value of the measurement.",
                    "datatype": "xsd:float"
                  }
                ]
              }
            ]
        }
        response = requests.put(surl, data=json.dumps(payload),
                                headers=self.headers, params={'rev':current_rev})
        print response.text
        assert response.status_code == 200

        # Fetch previous vrsion and current version

        #GET /v0/schemas/{orgId}/{domId}/{name}/{version}?rev={rev}
        response1 = requests.get(surl, params={'rev': '1'})
        response2 = requests.get(surl, params={'rev': '2'})
        assert response1.status_code == 200
        assert response2.status_code == 200
        j1 = json.loads(response1.text)
        j2 = json.loads(response2.text)
        pprint.pprint(j1)
        assert j1['shapes'][0]['description'] == "A point value or interval for product characteristics and other purposes."
        assert j2['shapes'][0]['description'] == "A measured length."
