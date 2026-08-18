import csv
from collections import defaultdict, Counter


rows = []
with open('../Data/VecTraits.csv', encoding='utf-8') as f:
    reader = csv.DictReader(f)
    for row in reader:
        rows.append(row)


trait_names = [r['OriginalTraitName'].strip() for r in rows]
unique_names = Counter(trait_names)
print(f"Total rows: {len(rows)}")
print(f"Unique OriginalTraitNames: {len(unique_names)}")

pairs = Counter()
for r in rows:
    key = (r['OriginalTraitName'].strip(), r['OriginalTraitDef'].strip())
    pairs[key] += 1
print(f"Unique name+definition pairs: {len(pairs)}")

name_to_defs = defaultdict(set)
for r in rows:
    name_to_defs[r['OriginalTraitName'].strip()].add(r['OriginalTraitDef'].strip())
multi_def = {k: v for k, v in name_to_defs.items() if len(v) > 1}
print(f"Names with multiple definitions: {len(multi_def)}")

std_filled = [r for r in rows if r['StandardisedTraitName'].strip() != '']
print(f"\nStandardisedTraitName filled: {len(std_filled)}/{len(rows)} "
      f"({100*len(std_filled)/len(rows):.1f}%)")
unique_std = Counter([r['StandardisedTraitName'].strip() for r in std_filled])
print(f"Unique StandardisedTraitName values: {len(unique_std)}")
print("Top 5:")
for name, count in unique_std.most_common(5):
    print(f"  [{count:5d}]  {name}")


name_to_units = defaultdict(set)
for r in rows:
    unit = r['OriginalTraitUnit'].strip()
    if unit:
        name_to_units[r['OriginalTraitName'].strip()].add(unit)
multi_unit = {k: v for k, v in name_to_units.items() if len(v) > 1}
print(f"\nNames with multiple units: {len(multi_unit)}")


print("\nTop 20 most frequent trait names:")
for name, count in unique_names.most_common(20):
    n_defs = len(name_to_defs[name])
    n_units = len(name_to_units.get(name, set()))
    print(f"  [{count:5d} rows]  {n_defs} defs  {n_units} units  '{name}'")


name_to_taxa = defaultdict(set)
for r in rows:
    genus = r['Interactor1Genus'].strip()
    if genus:
        name_to_taxa[r['OriginalTraitName'].strip()].add(genus)

print("\nTop 10 trait names and genera coverage:")
for name, count in unique_names.most_common(10):
    n_taxa = len(name_to_taxa.get(name, set()))
    print(f"  [{count:5d} rows]  {n_taxa:3d} genera  '{name}'")

print("\nCapitalisation inconsistencies:")
lower_names = defaultdict(list)
for name in set(trait_names):
    lower_names[name.lower()].append(name)
for lower, variants in lower_names.items():
    if len(variants) > 1:
        counts = [unique_names[v] for v in variants]
        print(f"  {variants}  (rows: {counts})")


import os

output_rows = []
for name, count in unique_names.most_common():
    output_rows.append({
        'OriginalTraitName': name,
        'total_rows': count,
        'n_definitions': len(name_to_defs[name]),
        'n_units': len(name_to_units.get(name, set())),
        'n_genera': len(name_to_taxa.get(name, set())),
        'taxa_specific': 'YES' if len(name_to_taxa.get(name, set())) <= 3 and count >= 100 else '',
        'capitalisation_issue': 'YES' if any(
            name.lower() == other.lower() and name != other
            for other in unique_names.keys()
        ) else ''
    })

os.makedirs('../Results', exist_ok=True)
with open('../Results/heterogeneity_analysis.csv', 'w', newline='', encoding='utf-8') as f:
    writer = csv.DictWriter(f, fieldnames=output_rows[0].keys())
    writer.writeheader()
    writer.writerows(output_rows)

print("\nResults saved to ../Results/heterogeneity_analysis.csv")


print("\nPreparing text for embedding...")


unique_pairs = {}
for r in rows:
    name = r['OriginalTraitName'].strip()
    defn = r['OriginalTraitDef'].strip()
    unit = r['OriginalTraitUnit'].strip()
    key = (name, defn)
    if key not in unique_pairs:
        unique_pairs[key] = unit

texts = []
pair_list = []
for (name, defn), unit in unique_pairs.items():
    text = f"Trait: {name}. Definition: {defn}. Unit: {unit}."
    texts.append(text)
    pair_list.append((name, defn, unit))

print(f"Total unique pairs to embed: {len(texts)}")
print(f"Example: {texts[0]}")

from sentence_transformers import SentenceTransformer

print("\nLoading model...")
model = SentenceTransformer('all-MiniLM-L6-v2')

print("Generating embeddings...")
embeddings = model.encode(texts, show_progress_bar=True)

print(f"Embeddings shape: {embeddings.shape}")


import Code.clustering_dbcv as clustering_dbcv
import numpy as np

print("\nClustering...")
clusterer = clustering_dbcv.HDBSCAN(
    min_cluster_size=5,
    min_samples=3,
    metric='euclidean'
)
cluster_labels = clusterer.fit_predict(embeddings)

n_clusters = len(set(cluster_labels)) - (1 if -1 in cluster_labels else 0)
n_noise = list(cluster_labels).count(-1)

print(f"Number of clusters: {n_clusters}")
print(f"Noise points (unclustered): {n_noise}")
print(f"Clustered points: {len(cluster_labels) - n_noise}")


from collections import defaultdict

cluster_contents = defaultdict(list)
for i, label in enumerate(cluster_labels):
    cluster_contents[label].append(pair_list[i])

print("\nCluster contents:")
for cluster_id in sorted(cluster_contents.keys()):
    if cluster_id == -1:
        continue
    members = cluster_contents[cluster_id]
    print(f"\n--- Cluster {cluster_id} ({len(members)} members) ---")
    for name, defn, unit in members:
        print(f"  '{name}' | {defn[:50]} | {unit}")


with open('../Results/clustering_results.txt', 'w', encoding='utf-8') as f:
    for cluster_id in sorted(cluster_contents.keys()):
        if cluster_id == -1:
            continue
        members = cluster_contents[cluster_id]
        f.write(f"\n--- Cluster {cluster_id} ({len(members)} members) ---\n")
        for name, defn, unit in members:
            f.write(f"  '{name}' | {defn[:80]} | {unit}\n")

    # noise points
    f.write(f"\n--- NOISE (unclustered, {len(cluster_contents[-1])} items) ---\n")
    for name, defn, unit in cluster_contents[-1]:
        f.write(f"  '{name}' | {defn[:80]} | {unit}\n")

print("Saved to ../Results/clustering_results.txt")


import csv

with open('../Results/clustering_results.csv', 'w', newline='', encoding='utf-8') as f:
    writer = csv.writer(f)
    writer.writerow(['cluster_id', 'n_members', 'OriginalTraitName',
                     'OriginalTraitDef', 'OriginalTraitUnit'])
    for cluster_id in sorted(cluster_contents.keys()):
        members = cluster_contents[cluster_id]
        for name, defn, unit in members:
            writer.writerow([cluster_id, len(members), name, defn, unit])

print("Saved to ../Results/clustering_results.csv")