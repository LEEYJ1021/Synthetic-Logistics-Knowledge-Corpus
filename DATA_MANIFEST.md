# Data Files Manifest

This file documents the data files required to run the analysis scripts.
The actual data files are available at:
https://github.com/LEEYJ1021/Synthetic-Logistics-Knowledge-Corpus

## Required Files

### SQLite Database
| File | Size (approx.) | Description |
|:---|:---|:---|
| `logistics_knowledge.db` | ~15 MB | SQLite database with 3 tables: `documents` (2,733 rows), `generation_metrics`, `dataset_statistics` |

**Schema — `documents` table:**
```sql
CREATE TABLE documents (
    doc_id          TEXT PRIMARY KEY,
    doc_type        TEXT,   -- 'shipping_order' | 'warehouse_policy' | 'customer_requirement'
    content         TEXT,
    metadata        TEXT,   -- JSON
    embedding_hash  TEXT,
    timestamp       TEXT
);
```

### FAISS Index Files
| File | Index Type | d | n | Size (approx.) |
|:---|:---|:---|:---|:---|
| `faiss_logistics.index.shipping_order` | IndexFlatL2 | 8,192 | 911 | ~60 MB |
| `faiss_logistics.index.warehouse_policy` | IndexFlatL2 | 8,192 | 911 | ~60 MB |
| `faiss_logistics.index.customer_requirement` | IndexFlatL2 | 8,192 | 911 | ~60 MB |

All three indices store dense semantic vectors (d=8,192) generated via an external LLM embedding service, with a deterministic SHA-256 hash-based fallback for reproducibility.

## Corpus Statistics

| Document Type | N | Mean Length (chars) | SD | Mean Words |
|:---|:---|:---|:---|:---|
| Shipping Order | 911 | 1,567.1 | 11.7 | 138.4 |
| Warehouse Policy | 911 | 3,985.4 | 5.8 | 385.6 |
| Customer Requirement | 911 | 4,090.4 | 13.3 | 395.1 |
| **Total** | **2,733** | — | — | — |

## Experimental Configuration (Table 6 in paper)

| Parameter | Value |
|:---|:---|
| RANDOM_SEED | 42 |
| NUM_TEST_QUERIES | 250 |
| TOP_K | 10 |
| GROUND_TRUTH_K | 50 |
| Original Dimension | 4,096 (reconstructed from FAISS) |
| Embedding Dimension | 8,192 (stored in FAISS) |
| TARGET_DIMS | [4096, 2048, 1536, 1024, 768, 512, 256, 128] |
| REDUCERS | gaussian_rp, sparse_rp |
| DISTANCE_METRICS | l2, cosine |
| HYBRID_GT_WEIGHTS | neighbors:1, bm25:1, rules:2, labels:3 |
| BOOTSTRAP_ITERS | 1,500 |

## Query Allocation

Queries are allocated proportionally by document type (stratified sampling):
- shipping_order: 84 queries
- warehouse_policy: 83 queries
- customer_requirement: 83 queries
- **Total: 250 queries**
