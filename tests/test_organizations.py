#!/usr/bin/env python


import os
import sys
import json
import uuid
import pytest
import requests


class TestOrganizations(object):
    """Test suite for testing 'Organizations'.
    """

    headers = {'Content-type': 'application/json'}

    @pytest.mark.disabled
    def test1_create_org(self, url, uid):
        """Test to create, update, get and deprecate an 'organization'.
        """

        # Create an organization
        payload = {
            "@context": {
                "schema": "http://schema.org/"
            },
            "schema:name": "testorg"
        }

        rurl = os.path.join(url, "organizations", uid)
        response = requests.put(rurl, data=json.dumps(payload),
                                headers=self.headers)
        assert response.status_code == 201

        # Verify creation
        response = requests.get(rurl)
        assert response.status_code == 200
        response = json.loads(response.text)
        current_rev = response['nxv:rev']
        assert response["schema:name"] == "testorg"

        # Try to create again; error expected
        response = requests.put(rurl, data=json.dumps(payload),
                                headers=self.headers)
        assert response.status_code == 409

        # Update organization
        payload = {
            "@context": {
                "schema": "http://schema.org/"
            },
            "schema:name": "testorgX"
        }
        r = requests.put(rurl, data=json.dumps(payload),
                         params={'rev': current_rev},
                         headers=self.headers)
        assert r.status_code == 200

        # Verify update
        response = requests.get(rurl)
        assert response.status_code == 200
        response = json.loads(response.text)
        current_rev = response['nxv:rev']
        assert current_rev == 2
        assert response["schema:name"] == "testorgX"

        # Deprecate organization
        r = requests.delete(rurl)
        assert r.status_code == 404
        assert r.text == "Request is missing required query parameter 'rev'"

        # Try to deprecate the wrong version
        r = requests.delete(rurl, params={'rev': 1})
        assert r.status_code == 409

        # Now the correct version
        r = requests.delete(rurl, params={'rev': 2})
        assert r.status_code == 200
