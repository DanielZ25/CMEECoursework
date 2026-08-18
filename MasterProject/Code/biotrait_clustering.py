import pandas as pd
import numpy as np
from openai import OpenAI
import hdbscan
from sklearn.preprocessing import normalize


client = OpenAI(api_key="")
DATA_PATH = "/Users/danielzhu/Documents/Master Project/Data/GlobalDataset.csv"
RESULTS_PATH = "/Users/danielzhu/Documents/Master Project/Results"

df = pd.read_csv(DATA_PATH, encoding="ISO-8859-1", low_memory=False)
df = df.drop_duplicates(subset=["OriginalTraitName", "OriginalTraitDef"])
df = df.fillna("")
df = df.reset_index(drop=True)

print(f"Unique (name+def) pairs: {len(df)}")


def build_text(row, version):
    parts = [f"Trait: {row['OriginalTraitName']}."]
    if version >= 2 and row['OriginalTraitDef']:
        parts.append(f"Definition: {row['OriginalTraitDef']}.")
    if version >= 3 and row['OriginalTraitUnit']:
        parts.append(f"Unit: {row['OriginalTraitUnit']}.")
    if version >= 4 and row['ConOrder']:
        parts.append(f"Organism order: {row['ConOrder']}.")
    if version >= 5 and row['ConFamily']:
        parts.append(f"Organism family: {row['ConFamily']}.")
    if version >= 6 and row['ConStage']:
        parts.append(f"Life stage: {row['ConStage']}.")
    return " ".join(parts)

# ── 3. Embedding ─────────────────────────────────────────────────
def get_embeddings(texts, model="text-embedding-3-large", batch_size=100):
    all_embeddings = []
    for i in range(0, len(texts), batch_size):
        batch = texts[i:i+batch_size]
        print(f"  Batch {i//batch_size + 1}/{(len(texts)-1)//batch_size + 1}...")
        response = client.embeddings.create(input=batch, model=model)
        all_embeddings.extend([item.embedding for item in response.data])
    return np.array(all_embeddings)

# ── 4. loop ─────────────────────────────────────────────────────
results = []

for v in range(1, 7):
    print(f"\n=== Version {v} ===")
    
    texts = [build_text(row, v) for _, row in df.iterrows()]
    embeddings = get_embeddings(texts)
    embeddings_norm = normalize(embeddings)
    
    np.save(f"{RESULTS_PATH}/biotraits_embeddings_v{v}.npy", embeddings_norm)
    
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
    
    sizes = pd.Series(cluster_labels)
    sizes = sizes[sizes != -1].value_counts()
    avg_size = float(sizes.mean()) if len(sizes) > 0 else 0
    
    print(f"  Clusters: {n_clusters}, Noise: {n_noise} ({noise_rate:.1%})")
    
    results.append({
        "version":    f"v{v}",
        "x":          v,
        "n_clusters": n_clusters,
        "n_noise":    n_noise,
        "noise_rate": noise_rate,
        "avg_size":   avg_size
    })
    
    df_out = df.copy()
    df_out["text"] = texts
    df_out["version"] = f"v{v}"
    df_out["cluster_id"] = cluster_labels
    df_out.to_csv(f"{RESULTS_PATH}/biotraits_clustering_v{v}.csv", index=False)


results_df = pd.DataFrame(results)
results_df.to_csv(f"{RESULTS_PATH}/biotraits_rarefaction.csv", index=False)

print("\n=== Summary ===")
print(results_df.to_string(index=False))