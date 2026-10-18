
from pathlib import Path
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parent
CODE_DIR = ROOT / "code"
DATA_DIR = ROOT / "data"
OUTPUT_DIR = ROOT / "outputs"

REQUIRED_INPUTS = [
    DATA_DIR / "raw" / "public.dat",
    DATA_DIR / "raw" / "codebook",
    DATA_DIR / "processed" / "data_09_19_722513.csv",
]

SCRIPTS = [
    "Replication.R",
    "SCM_722513.R",
]

EXPECTED_OUTPUTS = [
    OUTPUT_DIR / "tables" / "table2_replication.png",
    OUTPUT_DIR / "tables" / "table3_replication.png",
    OUTPUT_DIR / "tables" / "table4_replication.png",
    OUTPUT_DIR / "tables" / "table4_diagnostic.png",
    OUTPUT_DIR / "figures" / "Figure1_Replication.png",
    OUTPUT_DIR / "figures" / "large_counties_norm_emplvl_722513_gap_plot.png",
    OUTPUT_DIR / "figures" / "medium_counties_norm_emplvl_722513_gap_plot.png",
    OUTPUT_DIR / "figures" / "small_counties_norm_emplvl_722513_gap_plot.png",
    OUTPUT_DIR / "figures" / "all_counties_norm_emplvl_722513_gap_plot.png",
]


def main():
    if shutil.which("Rscript") is None:
        sys.exit(
            "ERROR: Rscript was not found. Install R and add it to PATH."
        )

    missing = [
        str(path.relative_to(ROOT))
        for path in REQUIRED_INPUTS + [CODE_DIR / s for s in SCRIPTS]
        if not path.is_file()
    ]

    if missing:
        sys.exit("ERROR: Required files are missing:\n" + "\n".join(missing))

    (OUTPUT_DIR / "tables").mkdir(parents=True, exist_ok=True)
    (OUTPUT_DIR / "figures").mkdir(parents=True, exist_ok=True)

    for script in SCRIPTS:
        print(f"\n{'=' * 60}\nRunning {script}\n{'=' * 60}", flush=True)

        try:
            subprocess.run(
                ["Rscript", script],
                cwd=CODE_DIR,
                check=True,
            )
        except subprocess.CalledProcessError as exc:
            sys.exit(
                f"\nERROR: {script} failed with exit code {exc.returncode}. "
                "Workflow stopped."
            )

    missing_outputs = [
        str(path.relative_to(ROOT))
        for path in EXPECTED_OUTPUTS
        if not path.is_file() or path.stat().st_size == 0
    ]

    if missing_outputs:
        sys.exit(
            "\nERROR: Workflow finished, but outputs are missing or empty:\n"
            + "\n".join(missing_outputs)
        )

    print("\nSUCCESS: Both R scripts completed.")
    print("All nine expected output files exist and are non-empty.")
    print(f"Tables:  {OUTPUT_DIR / 'tables'}")
    print(f"Figures: {OUTPUT_DIR / 'figures'}")


if __name__ == "__main__":
    main()
