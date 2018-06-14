#!/usr/bin/env python


import os
import sys
import json
import uuid
import time
import urllib
import pprint
import pytest
import filecmp
import requests


def check_status(response, code):
    """Convenient method to check the status code.
    """
    if response.status_code == code:
        pass
    else:
        text = "Status Code returned: %s   expected: %s" % (response.status_code, code)
        raise ValueError(text + "\n" + response.text)

def get_revision(url):
    """Returns the version of the current revision of an instance.
    """
    response = requests.get(url)
    check_status(response, 200)
    resp = json.loads(response.text)
    return resp['nxv:rev']

class TestSchema(object):
    """Test suite for testing 'Instances'.
    """
    headers = {'Content-type': 'application/json'}

    @pytest.mark.dev
    def test1_instance(self, domain_url, schema_url, data_url, uid):
        """Test to create, update and delete instances; use attachments; perform a simple search
        """
        print("ID for this session: %s" % uid)

        # Create a new domain first for the schema
        payload = {
            "description": "Test domain"
        }
        rurl = os.path.join(domain_url, uid)
        response = requests.put(rurl, data=json.dumps(payload),
                                headers=self.headers)
        check_status(response, 201)

        # Create a new schema
        payload = {
            "@context": [
                "https://bbp-nexus.epfl.ch/staging/v0/contexts/nexus/core/schema/v0.2.0"
            ],
            "@type": "nxv:Schema",
            "shapes": {
                "@type": "sh:NodeShape",
                "description": "schema.org person description.",
                "nodeKind": "sh:BlankNodeOrIRI",
                "targetClass": "schema:Person",
                "property": [
                {
                    "path": "schema:email",
                    "datatype": "xsd:string",
                    "name": "Email",
                    "pattern": "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"
                },
                {
                    "datatype": "xsd:string",
                    "description": "Given name. In the U.S., the first name of a Person. This can be used along with familyName instead of the name property.",
                    "name": "givenName",
                    "path": "schema:givenName",
                    "minCount": "1"
                },
                {
                    "datatype": "xsd:string",
                    "description": "Family name. In the U.S., the last name of an Person. This can be used along with givenName instead of the name property.",
                    "name": "familyName",
                    "path": "schema:familyName",
                    "minCount": "1"
                }
            ]
          }
        }
        surl = os.path.join(schema_url, uid ,'testschema', 'v1.0.0')
        response = requests.put(surl, data=json.dumps(payload),
                                headers=self.headers)
        check_status(response, 201)

        # Verify the URL
        response = requests.get(surl)
        check_status(response, 200)
        response = json.loads(response.text)
        current_rev = response['nxv:rev']
        assert response["@id"] == surl

        # Publish the schema
        surl = os.path.join(schema_url, uid ,'testschema', 'v1.0.0','config?rev=%s'%current_rev)
        response = requests.patch(surl, data = json.dumps({'published':True}),
                                  headers=self.headers)

        # Create an instance
        durl = os.path.join(data_url, uid ,'testschema', 'v1.0.0')
        payload = {
            "@context": {
                "Person": "http://schema.org/Person",
                "givenName": "http://schema.org/givenName",
                "familyName": "http://schema.org/familyName"
            },
            "@type": [
                "Person"
            ],
            "familyName": "Nexus",
            "givenName": "Brian"
        }
        response = requests.post(durl, data=json.dumps(payload),
                                headers=self.headers)
        check_status(response, 201)
        resp = json.loads(response.text)
        url_inst = resp['@id']
        current_rev = resp['nxv:rev']
        print("URL of the instance: %s" % url_inst)

        # Update the instance (incomplete data; error expected)
        payload = {
            "@context": {
                "Person": "http://schema.org/Person",
                "givenName": "http://schema.org/givenName",
                "familyName": "http://schema.org/familyName"
            },
            "@type": [
                "Person"
            ],
            "familyName": "Monty",
        }
        fullurl = "%s?rev=%s" % (url_inst, current_rev)
        response = requests.put(fullurl, data=json.dumps(payload),
                                headers=self.headers)
        check_status(response, 400)

        # Update the instance; with all required data now
        payload = {
            "@context": {
                "Person": "http://schema.org/Person",
                "givenName": "http://schema.org/givenName",
                "familyName": "http://schema.org/familyName"
            },
            "@type": [
                "Person"
            ],
            "familyName": "Monty",
            "givenName": "Brian"
        }
        fullurl = "%s?rev=%s" % (url_inst, current_rev)
        response = requests.put(fullurl, data=json.dumps(payload),
                                headers=self.headers)
        check_status(response, 200)

        # Update the instance (old version, error expected)
        payload = {
            "@context": {
                "Person": "http://schema.org/Person",
                "givenName": "http://schema.org/givenName",
                "familyName": "http://schema.org/familyName"
            },
            "@type": [
                "Person"
            ],
            "familyName": "Monty",
            "givenName": "Python"
        }
        fullurl = "%s?rev=%s" % (url_inst, current_rev)
        response = requests.put(fullurl, data=json.dumps(payload),
                                headers=self.headers)
        check_status(response, 409)

        # Get the current instance
        response = requests.get(url_inst)
        check_status(response, 200)
        response = json.loads(response.text)
        current_rev = response['nxv:rev']

        # Update again with the correct version
        payload = {
            "@context": {
                "Person": "http://schema.org/Person",
                "givenName": "http://schema.org/givenName",
                "familyName": "http://schema.org/familyName"
            },
            "@type": [
                "Person"
            ],
            "familyName": "Monty",
            "givenName": "Python"
        }
        fullurl = "%s?rev=%s" % (url_inst, current_rev)
        response = requests.put(fullurl, data=json.dumps(payload),
                                headers=self.headers)
        check_status(response, 200)

        # Verify the update
        response = requests.get(url_inst)
        check_status(response, 200)
        resp = json.loads(response.text)
        assert resp['familyName'] == 'Monty'
        assert resp['givenName'] == 'Python'
        current_rev = resp['nxv:rev']

        # Verify old instance
        fullurl = "%s?rev=%s" % (url_inst, 2)
        response = requests.get(fullurl)
        check_status(response, 200)
        resp = json.loads(response.text)
        assert resp['familyName'] == 'Monty'
        assert resp['givenName'] == 'Brian'

        # Create a different instance with the same schema
        durl = os.path.join(data_url, uid ,'testschema', 'v1.0.0')
        payload = {
            "@context": {
                "Person": "http://schema.org/Person",
                "givenName": "http://schema.org/givenName",
                "familyName": "http://schema.org/familyName"
            },
            "@type": [
                "Person"
            ],
            "familyName": "Einstein",
            "givenName": "Albert"
        }
        response = requests.post(durl, data=json.dumps(payload),
                                headers=self.headers)
        check_status(response, 201)
        resp = json.loads(response.text)
        url_inst2 = resp['@id']
        current_rev = resp['nxv:rev']
        print("URL of the instance: %s" % url_inst2)

        # Attach binary data
        current_rev = get_revision(url_inst)
        img1 = 'mp.jpg'
        img1c = 'mp_copy.jpg'
        img2 = 'python.jpg'
        img2c = 'python_copy.jpg'
        files = {'file':  open(img1, 'rb')}
        fullurl = "%s/attachment?rev=%s" % (url_inst, current_rev)
        response = requests.put(fullurl, files = files)
        check_status(response, 201)

        # Verify binary data
        fullurl = "%s/attachment" % (url_inst)
        response = requests.get(fullurl, headers = self.headers)
        check_status(response, 200)
        revision1 = get_revision(url_inst)
        with open(img1c,"wb") as f:
            for chunk in response:
                f.write(chunk)
        assert filecmp.cmp(img1, img1c)

        # Upload a changed image
        files = {'file':  open(img2, 'rb')}
        fullurl = "%s/attachment?rev=%s" % (url_inst, revision1)
        response = requests.put(fullurl, files = files)
        check_status(response, 201)
        revision2 = get_revision(url_inst)

        # Verify binary data current revision
        fullurl = "%s/attachment" % (url_inst)
        response = requests.get(fullurl, headers = self.headers)
        check_status(response, 200)
        with open(img2c,"wb") as f:
            for chunk in response:
                f.write(chunk)
        assert filecmp.cmp(img2, img2c)

        # Verify binary data previous revision
        fullurl = "%s/attachment?rev=%s" % (url_inst, revision1)
        response = requests.get(fullurl, headers = self.headers)
        check_status(response, 200)
        with open(img1c,"wb") as f:
            for chunk in response:
                f.write(chunk)
        assert filecmp.cmp(img1, img1c)

        # Delete the latest attachment
        fullurl = "%s/attachment?rev=%s" % (url_inst, revision2)
        response = requests.delete(fullurl, headers = self.headers)
        check_status(response, 200)

        # Verify deletion
        fullurl = "%s/attachment" % (url_inst)
        response = requests.get(fullurl, headers = self.headers)
        check_status(response, 404)
        assert response.text == "The requested resource could not be found but may be available again in the future."

        # Verify binary data previous
        fullurl = "%s/attachment?rev=%s" % (url_inst, revision1)
        response = requests.get(fullurl, headers = self.headers)
        check_status(response, 200)
        with open(img1c,"wb") as f:
            for chunk in response:
                f.write(chunk)
        assert filecmp.cmp(img1, img1c)

        # Search for an instance
        search_url = "%s/%s" % (data_url, uid)
        params = {
            'filter': '{"op":"eq","path":"schema:familyName","value":"Einstein"}'
        }
        t0 = time.time()
        while time.time()-t0<20:
            time.sleep(1)
            response = requests.get(search_url, params=urllib.urlencode(params))
            if len(json.loads(response.text)['results']) > 0:
                print("Time until instance indexed: %.0f" % (time.time()-t0))
                break
        instance = json.loads(response.text)['results'][0]
        assert instance['source']['@id'] == url_inst2

        # Deprecate Einstein instance
        fullurl = "%s?rev=1" % url_inst2
        response = requests.delete(fullurl, headers = self.headers)
        check_status(response, 200)

        # still there?
        response = requests.get(url_inst2, headers = self.headers)
        resp = json.loads(response.text)
        assert resp['nxv:deprecated'] == True

        # Try to update this instance; wrong revision
        payload = {
            "@context": {
                "Person": "http://schema.org/Person",
                "givenName": "http://schema.org/givenName",
                "familyName": "http://schema.org/familyName"
            },
            "@type": [
                "Person"
            ],
            "familyName": "Einstein",
            "givenName": "Alberto"
        }
        fullurl = "%s?rev=1" % (url_inst2)
        response = requests.put(fullurl, data=json.dumps(payload),
                                headers=self.headers)
        check_status(response, 409)
        assert json.loads(response.text)['code'] == "IncorrectRevisionProvided"

        # Try to update this instance; deprecated
        payload = {
            "@context": {
                "Person": "http://schema.org/Person",
                "givenName": "http://schema.org/givenName",
                "familyName": "http://schema.org/familyName"
            },
            "@type": [
                "Person"
            ],
            "familyName": "Einstein",
            "givenName": "Alberto"
        }
        fullurl = "%s?rev=2" % (url_inst2)
        response = requests.put(fullurl, data=json.dumps(payload),
                                headers=self.headers)
        check_status(response, 400)
        assert json.loads(response.text)['code'] == "InstanceIsDeprecated"

        # Delete the attachment from the first instance
        current_rev = get_revision(url_inst)
        fullurl = "%s/attachment?rev=%s" % (url_inst, current_rev)
        response = requests.delete(fullurl, files = files)
        check_status(response, 404)
        assert json.loads(response.text)['code'] == "AttachmentNotFound"

        # Try to get older attachment
        fullurl = "%s/attachment?rev=%d" % (url_inst, int(current_rev)-1)
        response = requests.get(fullurl, headers = self.headers)
        check_status(response, 200)


