"""Explicit input selection: public mode never falls back to private files."""

from pathlib import Path


def resolve_data_input(relative_path: str | Path, analysis_root: Path, *, public_data: bool = False) -> Path:
    relative = Path(relative_path)
    if relative.is_absolute() or ".." in relative.parts:
        raise ValueError("Input paths must be relative to the analysis repository")
    root = analysis_root / "public_data" if public_data else analysis_root
    path = root / relative
    if not path.is_file():
        mode = "Public" if public_data else "Private"
        guidance = (
            "Restore the published input; no private fallback is attempted."
            if public_data else "Private mode requires original analysis inputs."
        )
        raise FileNotFoundError(f"{mode} input is unavailable: {path}. {guidance}")
    return path


def prepared_analysis_input(relative_path: str | Path, analysis_root: Path) -> Path:
    """Prefer generated artifacts only for the explicitly selected input tree."""
    import os
    selected = os.environ.get("DAO_BIAS_DATA_ROOT")
    output = os.environ.get("DAO_BIAS_OUTPUT_ROOT")
    if selected and output and analysis_root.resolve() == Path(selected).resolve():
        generated = Path(output) / relative_path
        if generated.is_file():
            return generated
    return analysis_root / relative_path
