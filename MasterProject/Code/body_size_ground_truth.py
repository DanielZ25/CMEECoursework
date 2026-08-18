#!/usr/bin/env python3
"""Build a broad body-size/morphometrics ground truth from existing labels.

The target is a post-clustering concept label rather than the literal trait
name "body size". It includes organism-level mass/weight and linear
morphometrics (body, wing, head, thorax and hypostome dimensions), while
excluding egg/product mass, bloodmeal mass, weight change, rates and behaviour.

No embeddings or clustering are run.
"""

from __future__ import annotations

import csv
import re
from collections import Counter
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "Data" / "VecTraits.csv"
LABELS = ROOT / "Results" / "featexp_label_matrix.csv"
SILHOUETTE = ROOT / "Results" / "featexp_silhouette.csv"
RESULTS = ROOT / "Results"
VERSIONS = [f"V{i}" for i in range(1, 9)]

DIRECT_BODY_NAMES = {
    "body mass",
    "body length",
    "body width",
    "head capsule width",
    "hypostome length",
}
CANDIDATE_PATTERN = re.compile(
    r"\b(body size|body mass|body length|body width|wing length|thorax length|"
    r"head width|size|mass|weight|length|width|morphometr\w*)\b",
    re.IGNORECASE,
)
EGG_PATTERN = re.compile(r"\begg(s| mass)?\b", re.IGNORECASE)
LINEAR_PATTERN = re.compile(
    r"\b(wing|body|head|thorax|hypostome)?\s*(length|width)\b",
    re.IGNORECASE,
)
MASS_PATTERN = re.compile(r"\b(mass|weight)\b", re.IGNORECASE)


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig", errors="replace") as f:
        return list(csv.DictReader(f))


def write_csv(path: Path, rows: list[dict[str, object]], fields: list[str]) -> None:
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def decision(name: str, definition: str) -> tuple[bool, str]:
    name_l = name.strip().lower()
    definition_l = definition.strip().lower()

    if name_l.startswith("body size"):
        return True, "Explicit body-size label; retain definition/unit anomalies for audit"
    if name_l in DIRECT_BODY_NAMES:
        return True, "Direct organism-level body mass or linear morphometric"
    if name_l == "weight" and not EGG_PATTERN.search(definition_l):
        return True, "Definition measures the weight of an individual organism or life stage"

    if EGG_PATTERN.search(f"{name_l} {definition_l}"):
        return False, "Measures eggs or reproductive output rather than organism body size"
    if "bloodmeal" in name_l or "blood meal" in definition_l:
        return False, "Measures a consumed blood meal rather than organism body size"
    if "weight loss" in name_l or "loss" in definition_l:
        return False, "Measures change in weight rather than body size at a time point"
    if any(term in name_l for term in ("growth rate", "conversion", "efficiency", "efficency")):
        return False, "Derived rate or efficiency rather than a direct size measurement"
    if name_l in {"questing height", "longevity", "starvation tolerance", "lipid content"}:
        return False, "Context, behaviour, composition or duration rather than body size"
    return False, "Mentions size/mass/length only as contextual information"


def subtype(name: str, definition: str, unit: str) -> str:
    text = f"{name} {definition}"
    if LINEAR_PATTERN.search(text) and not (
        MASS_PATTERN.search(definition) and unit.strip().lower() not in {"mm", "cm", "m", "um", "µm"}
    ):
        return "linear_morphometric"
    return "body_mass_or_weight"


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

candidate_rows: list[dict[str, object]] = []
ground_truth_pairs: set[tuple[str, str]] = set()

for pair, label_row in label_by_pair.items():
    name, definition = pair
    if not CANDIDATE_PATTERN.search(f"{name} {definition}"):
        continue

    source = pair_first.get(pair, {})
    unit = source.get("OriginalTraitUnit", "").strip()
    include, reason = decision(name, definition)
    measurement_subtype = subtype(name, definition, unit) if include else "excluded"
    quality_flag = ""
    if include and measurement_subtype == "body_mass_or_weight" and unit.lower() in {"day", "days"}:
        quality_flag = "unit inconsistent with mass definition"

    if include:
        ground_truth_pairs.add(pair)

    out: dict[str, object] = {
        "OriginalTraitName": name,
        "OriginalTraitDef": definition,
        "OriginalTraitUnit": unit,
        "raw_record_count": pair_counts[pair],
        "ground_truth_decision": "include" if include else "exclude",
        "concept_label": "body size and morphometrics" if include else "",
        "measurement_subtype": measurement_subtype,
        "data_quality_flag": quality_flag,
        "decision_reason": reason,
    }
    out.update({version: label_row[version] for version in VERSIONS})
    candidate_rows.append(out)

candidate_rows.sort(
    key=lambda row: (
        row["ground_truth_decision"] != "include",
        str(row["measurement_subtype"]),
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
        "concept_label": row["concept_label"],
        "measurement_subtype": row["measurement_subtype"],
        "data_quality_flag": row["data_quality_flag"],
        **{version: row[version] for version in VERSIONS},
    }
    for row in candidate_rows
    if row["ground_truth_decision"] == "include"
]

if len(tracking_rows) != 56:
    raise ValueError(f"Expected 56 body-size concepts, found {len(tracking_rows)}")

fragmentation_rows: list[dict[str, object]] = []
cluster_content_rows: list[dict[str, object]] = []
subtype_rows: list[dict[str, object]] = []


def cluster_size(version: str, cluster_id: int) -> int:
    return sum(int(row[version]) == cluster_id for row in labels)


def dominant_details(version: str, rows: list[dict[str, object]]) -> dict[str, object]:
    target_labels = [int(row[version]) for row in rows]
    non_noise = [label for label in target_labels if label != -1]
    counts = Counter(non_noise)
    if not counts:
        return {
            "counts": counts,
            "dominant_cluster": None,
            "co_dominant_clusters": [],
            "target_in_dominant": 0,
            "dominant_cluster_size": 0,
            "dominant_cluster_purity": 0.0,
            "n_in_noise": target_labels.count(-1),
        }

    maximum = max(counts.values())
    tied = sorted(cluster_id for cluster_id, count in counts.items() if count == maximum)
    # With equal target coverage, use the smallest total cluster only as the
    # representative purity calculation; retain every tied ID explicitly.
    representative = min(tied, key=lambda cluster_id: (cluster_size(version, cluster_id), cluster_id))
    size = cluster_size(version, representative)
    return {
        "counts": counts,
        "dominant_cluster": representative,
        "co_dominant_clusters": tied,
        "target_in_dominant": maximum,
        "dominant_cluster_size": size,
        "dominant_cluster_purity": maximum / size if size else 0.0,
        "n_in_noise": target_labels.count(-1),
    }

for version in VERSIONS:
    details = dominant_details(version, tracking_rows)
    counts = details["counts"]
    dominant_cluster = details["dominant_cluster"]
    target_in_dominant = int(details["target_in_dominant"])
    cluster_members = (
        [row for row in labels if int(row[version]) == dominant_cluster]
        if dominant_cluster is not None
        else []
    )
    representative_cluster_size = int(details["dominant_cluster_size"])
    purity = float(details["dominant_cluster_purity"])

    fragmentation_rows.append(
        {
            "version": version,
            "last_field_added": silhouette[version]["last_field_added"],
            "silhouette": silhouette[version]["silhouette"],
            "n_ground_truth_concepts": len(tracking_rows),
            "n_clusters_occupied": len(counts),
            "n_in_noise": details["n_in_noise"],
            "dominant_cluster": dominant_cluster if dominant_cluster is not None else "",
            "co_dominant_clusters": ";".join(
                str(cluster_id) for cluster_id in details["co_dominant_clusters"]
            ),
            "n_co_dominant_clusters": len(details["co_dominant_clusters"]),
            "target_in_dominant_cluster": target_in_dominant,
            "dominant_share": round(target_in_dominant / len(tracking_rows), 4),
            "dominant_cluster_size": representative_cluster_size,
            "dominant_cluster_purity": round(purity, 4),
        }
    )

    for member in cluster_members:
        pair = (member["OriginalTraitName"].strip(), member["OriginalTraitDef"].strip())
        source = pair_first.get(pair, {})
        included = pair in ground_truth_pairs
        cluster_content_rows.append(
            {
                "version": version,
                "cluster_id": dominant_cluster,
                "OriginalTraitName": pair[0],
                "OriginalTraitDef": pair[1],
                "OriginalTraitUnit": source.get("OriginalTraitUnit", "").strip(),
                "is_body_size_ground_truth": included,
                "measurement_subtype": (
                    subtype(pair[0], pair[1], source.get("OriginalTraitUnit", ""))
                    if included
                    else "not body-size ground truth"
                ),
            }
        )

    for measurement_subtype in sorted({row["measurement_subtype"] for row in tracking_rows}):
        subset = [
            row for row in tracking_rows if row["measurement_subtype"] == measurement_subtype
        ]
        sub = dominant_details(version, subset)
        subtype_rows.append(
            {
                "version": version,
                "last_field_added": silhouette[version]["last_field_added"],
                "measurement_subtype": measurement_subtype,
                "n_ground_truth_concepts": len(subset),
                "n_clusters_occupied": len(sub["counts"]),
                "n_in_noise": sub["n_in_noise"],
                "dominant_cluster": sub["dominant_cluster"] if sub["dominant_cluster"] is not None else "",
                "co_dominant_clusters": ";".join(
                    str(cluster_id) for cluster_id in sub["co_dominant_clusters"]
                ),
                "target_in_dominant_cluster": sub["target_in_dominant"],
                "dominant_share": round(int(sub["target_in_dominant"]) / len(subset), 4),
                "dominant_cluster_size": sub["dominant_cluster_size"],
                "dominant_cluster_purity": round(float(sub["dominant_cluster_purity"]), 4),
            }
        )

fields_base = [
    "OriginalTraitName",
    "OriginalTraitDef",
    "OriginalTraitUnit",
    "raw_record_count",
]
write_csv(
    RESULTS / "body_size_candidate_audit.csv",
    candidate_rows,
    fields_base
    + [
        "ground_truth_decision",
        "concept_label",
        "measurement_subtype",
        "data_quality_flag",
        "decision_reason",
        *VERSIONS,
    ],
)
write_csv(
    RESULTS / "body_size_tracking.csv",
    tracking_rows,
    fields_base
    + ["concept_label", "measurement_subtype", "data_quality_flag", *VERSIONS],
)
write_csv(
    RESULTS / "body_size_fragmentation.csv",
    fragmentation_rows,
    [
        "version",
        "last_field_added",
        "silhouette",
        "n_ground_truth_concepts",
        "n_clusters_occupied",
        "n_in_noise",
        "dominant_cluster",
        "co_dominant_clusters",
        "n_co_dominant_clusters",
        "target_in_dominant_cluster",
        "dominant_share",
        "dominant_cluster_size",
        "dominant_cluster_purity",
    ],
)
write_csv(
    RESULTS / "body_size_subtype_summary.csv",
    subtype_rows,
    [
        "version",
        "last_field_added",
        "measurement_subtype",
        "n_ground_truth_concepts",
        "n_clusters_occupied",
        "n_in_noise",
        "dominant_cluster",
        "co_dominant_clusters",
        "target_in_dominant_cluster",
        "dominant_share",
        "dominant_cluster_size",
        "dominant_cluster_purity",
    ],
)
write_csv(
    RESULTS / "body_size_cluster_contents.csv",
    cluster_content_rows,
    [
        "version",
        "cluster_id",
        "OriginalTraitName",
        "OriginalTraitDef",
        "OriginalTraitUnit",
        "is_body_size_ground_truth",
        "measurement_subtype",
    ],
)

print(f"Candidates audited: {len(candidate_rows)}")
print(f"Included concepts: {len(tracking_rows)}")
print(f"Raw records represented: {sum(int(row['raw_record_count']) for row in tracking_rows)}")
print("Subtypes:", dict(Counter(row["measurement_subtype"] for row in tracking_rows)))
for row in fragmentation_rows:
    print(
        row["version"],
        "coverage=" + str(row["dominant_share"]),
        "noise=" + str(row["n_in_noise"]),
        "clusters=" + str(row["n_clusters_occupied"]),
        "cluster size=" + str(row["dominant_cluster_size"]),
        "purity=" + str(row["dominant_cluster_purity"]),
    )
