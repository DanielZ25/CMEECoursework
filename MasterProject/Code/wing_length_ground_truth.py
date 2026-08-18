#!/usr/bin/env python3
"""Build the wing-length ground-truth audit from existing clustering labels.

No embeddings or clustering are run. The script reads the canonical 461-row
label matrix, maps each (trait name, definition) pair back to its first unit in
VecTraits, and writes auditable V1-V8 tracking and summary tables.
"""

from __future__ import annotations

import csv
import re
from collections import Counter, defaultdict
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "Data" / "VecTraits.csv"
LABELS = ROOT / "Results" / "featexp_label_matrix.csv"
SILHOUETTE = ROOT / "Results" / "featexp_silhouette.csv"
RESULTS = ROOT / "Results"
VERSIONS = [f"V{i}" for i in range(1, 9)]
LENGTH_UNITS = {"mm", "cm", "m", "um", "µm"}


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig", errors="replace") as f:
        return list(csv.DictReader(f))


def write_csv(path: Path, rows: list[dict[str, object]], fields: list[str]) -> None:
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


raw = read_csv(DATA)
labels = read_csv(LABELS)
silhouette_rows = read_csv(SILHOUETTE)

if len(labels) != 461:
    raise ValueError(f"Expected 461 label rows, found {len(labels)}")

pair_first: dict[tuple[str, str], dict[str, str]] = {}
pair_counts: Counter[tuple[str, str]] = Counter()
for row in raw:
    pair = (
        row.get("OriginalTraitName", "").strip(),
        row.get("OriginalTraitDef", "").strip(),
    )
    pair_counts[pair] += 1
    pair_first.setdefault(pair, row)

label_by_pair = {
    (row["OriginalTraitName"].strip(), row["OriginalTraitDef"].strip()): row
    for row in labels
}
if len(label_by_pair) != 461:
    raise ValueError("The label matrix does not contain 461 unique name-definition pairs")

silhouette = {row["version"]: row for row in silhouette_rows}
wing_phrase = re.compile(r"\bwing\s+length\b", re.IGNORECASE)
mass_phrase = re.compile(r"\b(mass|weight)\b", re.IGNORECASE)

candidate_rows: list[dict[str, object]] = []
ground_truth_pairs: set[tuple[str, str]] = set()

for pair, label_row in label_by_pair.items():
    name, definition = pair
    if not wing_phrase.search(f"{name} {definition}"):
        continue

    source = pair_first.get(pair, {})
    unit = source.get("OriginalTraitUnit", "").strip()
    is_length_unit = unit.lower() in LENGTH_UNITS
    is_mass_measure = bool(mass_phrase.search(definition)) and not is_length_unit
    include = is_length_unit and not is_mass_measure

    if include:
        decision = "include"
        reason = "Definition explicitly measures wing length and unit is a linear length unit"
        ground_truth_pairs.add(pair)
    else:
        decision = "exclude"
        reason = "Actual measurement is dry mass (mg); wing length appears only as a related variable"

    out: dict[str, object] = {
        "OriginalTraitName": name,
        "OriginalTraitDef": definition,
        "OriginalTraitUnit": unit,
        "raw_record_count": pair_counts[pair],
        "ground_truth_decision": decision,
        "decision_reason": reason,
    }
    out.update({version: label_row[version] for version in VERSIONS})
    candidate_rows.append(out)

candidate_rows.sort(
    key=lambda row: (
        row["ground_truth_decision"] != "include",
        str(row["OriginalTraitName"]).lower(),
        str(row["OriginalTraitDef"]).lower(),
    )
)

tracking_rows = [
    {
        "OriginalTraitName": row["OriginalTraitName"],
        "OriginalTraitDef": row["OriginalTraitDef"],
        "OriginalTraitUnit": row["OriginalTraitUnit"],
        "raw_record_count": row["raw_record_count"],
        "include_basis": "definition+unit",
        **{version: row[version] for version in VERSIONS},
    }
    for row in candidate_rows
    if row["ground_truth_decision"] == "include"
]

if len(tracking_rows) != 2:
    raise ValueError(f"Expected 2 wing-length ground-truth concepts, found {len(tracking_rows)}")

fragmentation_rows: list[dict[str, object]] = []
cluster_content_rows: list[dict[str, object]] = []

for version in VERSIONS:
    target_labels = [int(row[version]) for row in tracking_rows]
    non_noise = [label for label in target_labels if label != -1]
    counts = Counter(non_noise)
    dominant_cluster = counts.most_common(1)[0][0] if counts else None
    target_in_dominant = counts.get(dominant_cluster, 0) if dominant_cluster is not None else 0

    cluster_members = (
        [row for row in labels if int(row[version]) == dominant_cluster]
        if dominant_cluster is not None
        else []
    )
    cluster_size = len(cluster_members)
    purity = target_in_dominant / cluster_size if cluster_size else 0.0

    fragmentation_rows.append(
        {
            "version": version,
            "last_field_added": silhouette[version]["last_field_added"],
            "silhouette": silhouette[version]["silhouette"],
            "n_ground_truth_concepts": len(tracking_rows),
            "n_clusters_occupied": len(counts),
            "n_in_noise": target_labels.count(-1),
            "dominant_cluster": dominant_cluster if dominant_cluster is not None else "",
            "target_in_dominant_cluster": target_in_dominant,
            "dominant_share": round(target_in_dominant / len(tracking_rows), 4),
            "all_targets_same_non_noise_cluster": (
                len(counts) == 1 and target_labels.count(-1) == 0
            ),
            "dominant_cluster_size": cluster_size,
            "dominant_cluster_purity": round(purity, 4),
        }
    )

    for member in cluster_members:
        pair = (member["OriginalTraitName"].strip(), member["OriginalTraitDef"].strip())
        source = pair_first.get(pair, {})
        cluster_content_rows.append(
            {
                "version": version,
                "cluster_id": dominant_cluster,
                "OriginalTraitName": pair[0],
                "OriginalTraitDef": pair[1],
                "OriginalTraitUnit": source.get("OriginalTraitUnit", "").strip(),
                "is_wing_length_ground_truth": pair in ground_truth_pairs,
            }
        )

write_csv(
    RESULTS / "wing_length_candidate_audit.csv",
    candidate_rows,
    [
        "OriginalTraitName",
        "OriginalTraitDef",
        "OriginalTraitUnit",
        "raw_record_count",
        "ground_truth_decision",
        "decision_reason",
        *VERSIONS,
    ],
)
write_csv(
    RESULTS / "wing_length_tracking.csv",
    tracking_rows,
    [
        "OriginalTraitName",
        "OriginalTraitDef",
        "OriginalTraitUnit",
        "raw_record_count",
        "include_basis",
        *VERSIONS,
    ],
)
write_csv(
    RESULTS / "wing_length_fragmentation.csv",
    fragmentation_rows,
    [
        "version",
        "last_field_added",
        "silhouette",
        "n_ground_truth_concepts",
        "n_clusters_occupied",
        "n_in_noise",
        "dominant_cluster",
        "target_in_dominant_cluster",
        "dominant_share",
        "all_targets_same_non_noise_cluster",
        "dominant_cluster_size",
        "dominant_cluster_purity",
    ],
)
write_csv(
    RESULTS / "wing_length_cluster_contents.csv",
    cluster_content_rows,
    [
        "version",
        "cluster_id",
        "OriginalTraitName",
        "OriginalTraitDef",
        "OriginalTraitUnit",
        "is_wing_length_ground_truth",
    ],
)

print(f"Candidates: {len(candidate_rows)}")
print(f"Included concepts: {len(tracking_rows)}")
print(f"Raw records represented: {sum(int(row['raw_record_count']) for row in tracking_rows)}")
for row in fragmentation_rows:
    print(
        row["version"],
        "labels together=" + str(row["all_targets_same_non_noise_cluster"]),
        "coverage=" + str(row["dominant_share"]),
        "cluster size=" + str(row["dominant_cluster_size"]),
        "purity=" + str(row["dominant_cluster_purity"]),
    )
