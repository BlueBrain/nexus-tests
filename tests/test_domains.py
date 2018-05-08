#!/usr/bin/env python


import os
import sys
import json
import uuid
import pprint
import pytest
import requests


class TestDomains(object):
    """Test suite for testing 'Domains'.
    """

    headers = {'Content-type': 'application/json'}

    @pytest.mark.dev
    def test1_domains(self, domain_url, uid):
        """Test to ...
        """

        # Create a new domain
        payload = {
            "description": "Test domain"
        }

        rurl = os.path.join(domain_url, uid)
        response = requests.put(rurl, data=json.dumps(payload),
                                headers=self.headers)
        assert response.status_code == 201

        # Verify creation
        response = requests.get(rurl)
        assert response.status_code == 200
        response = json.loads(response.text)
        current_rev = response['nxv:rev']
        assert response["@id"] == rurl

         # Try to create again; error expected
        response = requests.put(rurl, data=json.dumps(payload),
                                headers=self.headers)
        assert response.status_code == 409

        # Retrieve domain in different formats
        for format_ in ['compacted', 'expanded', 'flattened']:
            response = requests.get(rurl, params={'format':format_})
            print "\n\n FORMAT %s" % format_
            pprint.pprint(json.loads(response.text))

        # Update the domain: Not possible

        # Retrieve all domains created so far
        params = {
            'q': 'test',
            'size': 50,
        }
        response = requests.get(domain_url, params=params)
        response = json.loads(response.text)
        for domain in response['results']:

            # Get the current version of the domain
            domain_url = domain['resultId']
            response = requests.get(domain_url)
            assert response.status_code == 200
            response = json.loads(response.text)
            deprecated = response['nxv:deprecated']
            current_rev = response['nxv:rev']

            if not deprecated:
                # Try to deprecate the correct version
                r = requests.delete(domain_url, params={'rev': current_rev})
                assert current_rev == 1
                assert r.status_code == 200
                print("Deprecated domain %s, version %s" % (domain_url.split('/')[-1], current_rev))
            else:
                assert current_rev == 2
                print("Domain %s already deprecated" % domain_url.split('/')[-1])

