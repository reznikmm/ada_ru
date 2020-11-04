import sys
sys.path.insert(0, sys.argv[1])
from tests import TESTS
import json
print (json.dumps(TESTS))
