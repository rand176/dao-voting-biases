"""One import bootstrap for the source-tree Python package and child CLIs."""
import os
import sys
from tempfile import TemporaryDirectory
from pathlib import Path

SOURCE = str(Path(__file__).resolve().parents[1] / "src/python")
sys.path.insert(0, SOURCE)
os.environ["PYTHONDONTWRITEBYTECODE"] = "1"
os.environ["PYTHONPATH"] = SOURCE + os.pathsep + os.environ.get("PYTHONPATH", "")

# Set roots before collection imports modules with path constants. Direct pytest
# invocations get the same isolated artifact policy as run.py test.
_TEST_OUTPUTS = TemporaryDirectory(prefix="dao-bias-pytest-")
os.environ["DAO_BIAS_OUTPUT_ROOT"] = _TEST_OUTPUTS.name
os.environ["DAO_BIAS_DATA_ROOT"] = str(Path(__file__).resolve().parents[1])


def pytest_unconfigure(config):
    _TEST_OUTPUTS.cleanup()
