
import os
import uuid
import pytest


map_url = {
    'STAGE': "https://bbp-nexus.epfl.ch/staging",
    'PROD': "https://bbp.epfl.ch/nexus"
}

def pytest_addoption(parser):
    """Add extra arguments to the py.test call.
    """
    parser.addoption("--remote", action="store_true", default=False,
                     help="Running the tests remotely")
    parser.addoption("--with-head", action="store_true", default=False,
                     help="Running the tests with an actual browser (headless else)")
    parser.addoption("--screenshots", action="store_true", default=False,
                     help="Creating screenshots along every log (for debugging reasons)")
    parser.addoption("--environment", action="store", default="STAGE",
                     help="Entry point for the tests. Default: %(default)s")
    parser.addoption("--api", action="store", default="v0",
                     help="API version (v0 or v0). Default: %(default)s")

@pytest.fixture
def url(request):
    """Create URL path based on environment and version.
    """
    environment = request.config.getoption("--environment")
    api = request.config.getoption("--api")
    myurl = os.path.join(map_url[environment], api)
    return myurl


@pytest.fixture(scope="module")
def uid():
    test_uuid = "test" + str(uuid.uuid4()).replace('-','')[:20]
    return test_uuid
