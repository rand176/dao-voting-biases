from __future__ import annotations

from pathlib import Path


import os

PACKAGE_ROOT = Path(__file__).resolve().parents[4]
INPUT_ROOT = Path(os.environ.get("DAO_BIAS_DATA_ROOT", PACKAGE_ROOT)).resolve()
OUTPUT_ROOT = Path(os.environ.get("DAO_BIAS_OUTPUT_ROOT", PACKAGE_ROOT / "outputs")).resolve()
REPO_ROOT = INPUT_ROOT
DATA_2024_DIR = OUTPUT_ROOT / "data" / "2024"
INPUT_DATA_DIR = INPUT_ROOT / "data" / "2024"

RAW_DATA_DIR = INPUT_DATA_DIR / "raw"
DERIVED_DATA_DIR = DATA_2024_DIR / "derived"
DERIVED_ACTIVE_DIR = DERIVED_DATA_DIR / "active"
DERIVED_LEGACY_DIR = DERIVED_DATA_DIR / "legacy"
DERIVED_DEBUG_DIR = DERIVED_DATA_DIR / "debug"
DERIVED_ARCHIVE_LARGE_DIR = DERIVED_DATA_DIR / "archive_large"

CLEAN_VOTE_OUTPUTS_DIR = DATA_2024_DIR / "clean_vote_outputs"
CLEAN_VOTE_CUMULATIVE_OUTPUTS_DIR = DATA_2024_DIR / "clean_vote_cumulative_outputs"
CLEAN_AUTHOR_VOTE_OUTPUTS_DIR = DATA_2024_DIR / "clean_author_vote_outputs"
CLEAN_HUMAN_LABEL_OUTPUTS_DIR = DATA_2024_DIR / "clean_human_label_outputs"
CLEAN_PROPOSAL_FLAGS_DIR = DATA_2024_DIR / "clean_proposal_flags"
HUMANLABELS_DIR = INPUT_DATA_DIR / "humanlabels"
VPI_DIR = DATA_2024_DIR / "vpi"


def raw_data_path(*parts: str) -> Path:
    return RAW_DATA_DIR.joinpath(*parts)


def active_data_path(*parts: str) -> Path:
    return DERIVED_ACTIVE_DIR.joinpath(*parts)


def legacy_data_path(*parts: str) -> Path:
    return DERIVED_LEGACY_DIR.joinpath(*parts)


def debug_data_path(*parts: str) -> Path:
    return DERIVED_DEBUG_DIR.joinpath(*parts)


def archive_large_data_path(*parts: str) -> Path:
    return DERIVED_ARCHIVE_LARGE_DIR.joinpath(*parts)


def prepared_input(*parts: str) -> Path:
    """Prefer generated artifacts, otherwise use only the explicitly selected inputs."""
    generated = DATA_2024_DIR.joinpath(*parts)
    return generated if generated.exists() else INPUT_DATA_DIR.joinpath(*parts)
