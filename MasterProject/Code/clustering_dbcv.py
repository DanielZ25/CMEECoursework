import os
import pandas as pd
import numpy as np
from openai import OpenAI
import hdbscan
from sklearn.preprocessing import normalize


client = OpenAI(api_key="")
DATA_PATH = "/Users/danielzhu/Documents/Master Project/Data/VecTraits.csv"
RESULTS_PATH = "/Users/danielzhu/Documents/Master Project/Results"

df = pd.read_csv(DATA_PATH)
df = df.drop_duplicates(subset=["OriginalTraitName", "OriginalTraitDef"])
df = df.fillna("")
df = df.reset_index(drop=True)

print(f"Total unique trait pairs: {len(df)}")


def build_text(row, version):
    parts = []
    if version >= 1:
        parts.append(f"Trait: {row['OriginalTraitName']}.")
    if version >= 2:
        parts.append(f"Definition: {row['OriginalTraitDef']}.")
    if version >= 3:
        parts.append(f"Unit: {row['OriginalTraitUnit']}.")
    if version >= 4:
        parts.append(f"Organism order: {row['Interactor1Order']}.")
    if version >= 5:
        parts.append(f"Organism family: {row['Interactor1Family']}.")
    if version >= 6:
        parts.append(f"Life stage: {row['Interactor1Stage']}.")
    return " ".join(parts)


def get_embeddings(texts, model="text-embedding-3-large", batch_size=100):
    all_embeddings = []
    for i in range(0, len(texts), batch_size):
        batch = texts[i:i+batch_size]
        print(f"  Batch {i//batch_size + 1}/{(len(texts)-1)//batch_size + 1}...")
        response = client.embeddings.create(input=batch, model=model)
        batch_embeddings = [item.embedding for item in response.data]
        all_embeddings.extend(batch_embeddings)
    return np.array(all_embeddings)


results = []

for v in range(1, 7):
    print(f"\n=== Version {v} ===")
    
    texts = [build_text(row, v) for _, row in df.iterrows()]
    embeddings = get_embeddings(texts)
    embeddings_norm = normalize(embeddings)
    
    np.save(f"{RESULTS_PATH}/embeddings_v{v}.npy", embeddings_norm)
    print(f"Embeddings saved.")
    
    clusterer = hdbscan.HDBSCAN(
        min_cluster_size=5,
        min_samples=3,
        metric="euclidean",
        gen_min_span_tree=True
    )
    cluster_labels = clusterer.fit_predict(embeddings_norm)
    
    n_clusters  = len(set(cluster_labels)) - (1 if -1 in cluster_labels else 0)
    n_noise     = int(sum(cluster_labels == -1))
    noise_rate  = n_noise / len(cluster_labels)
    dbcv_score  = clusterer.relative_validity_
    
    sizes = pd.Series(cluster_labels)
    sizes = sizes[sizes != -1].value_counts()
    avg_size    = float(sizes.mean())
    median_size = float(sizes.median())
    
    print(f"  Clusters: {n_clusters}, Noise: {n_noise} ({noise_rate:.1%}), DBCV: {dbcv_score:.4f}")
    
    results.append({
        "version":      f"v{v}",
        "n_clusters":   n_clusters,
        "n_noise":      n_noise,
        "noise_rate":   noise_rate,
        "avg_size":     avg_size,
        "median_size":  median_size,
        "dbcv":         dbcv_score
    })
    
    df_out = df.copy()
    df_out["cluster_id"] = cluster_labels
    df_out["version"] = f"v{v}"
    df_out.to_csv(f"{RESULTS_PATH}/clustering_py_v{v}.csv", index=False)


results_df = pd.DataFrame(results)
results_df.to_csv(f"{RESULTS_PATH}/rarefaction_with_dbcv.csv", index=False)

print("\n=== Summary ===")
print(results_df.to_string(index=False))