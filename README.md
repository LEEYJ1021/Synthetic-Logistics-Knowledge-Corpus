# Synthetic Logistics Knowledge Corpus for Cognitive Retrieval Research

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/Python-3.8%2B-blue.svg)]()
[![Language: R](https://img.shields.io/badge/Language-R-blue.svg)]()
[![Database: SQLite](https://img.shields.io/badge/Database-SQLite-blue.svg)]()

This repository provides the synthetic dataset, analysis scripts, supplementary figures, and exploratory results associated with the research paper:

**Title:** *Trade-off-Aware Cognitive Architectures for Supply Chain Knowledge Retrieval: From Dimensionality Reduction to Autonomous Optimization*

The primary goal is to ensure transparency, reproducibility, and verifiability of the research findings by making all underlying data and analysis methodology publicly available.

---

## ⚠️ Critical Disclaimer: Synthetic Data

**This repository contains a dataset that is entirely synthetic and was generated programmatically.**

> All entities — including client names, locations, order IDs, and policy details — are fictional and were created based on a lightweight logistics ontology to simulate a realistic knowledge base for research purposes. This dataset should not be used for any purpose other than reproducing or building upon the research described in the paper.

---

## 1. Repository Structure

```
├── logistics_knowledge.db                    # Synthetic SQLite corpus (2,733 documents)
├── DATA_MANIFEST.md
│
├── data/                                         # Publication-grade supplementary figures
│   ├── faiss_logistics.index.shipping_order      # FAISS IndexFlatL2, d=8192, n=911
│   ├── faiss_logistics.index.warehouse_policy    # FAISS IndexFlatL2, d=8192, n=911
│   └── faiss_logistics.index.customer_requirement # FAISS IndexFlatL2, d=8192, n=911
│
├── analysis/
│   └── cognitive_retrieval_analysis.py       # Supplementary experiments (EXP1–EXP9)
│
├── figures/                                  # Publication-grade supplementary figures
│   ├── fig_R1_noise_perturbation_actual.png  # EXP1: Noise perturbation heatmap
│   ├── fig_R2_gt_sensitivity.png             # EXP2: GT weight sensitivity (81 combos)
│   ├── fig_R3_agentic_policy.png             # EXP3: Agentic policy decision space
│   ├── fig_R4_ivf_regularization_actual.png  # EXP4: IVF regularization + cluster purity
│   ├── fig_R5_fairness_disparity.png         # EXP5: Fairness operationalization
│   ├── fig_R6_cognitive_architecture.png     # EXP6: Cognitive architecture mapping
│   ├── fig_R7_latency_decomposition.png      # EXP7: Latency unit reconciliation
│   ├── fig_R8_kc_surface.png                 # EXP8: Key-context subspace justification
│   ├── fig_R9_splade_dpr_positioning.png     # EXP9: SPLADE/DPR positioning
│   ├── figSA1_exp1_nonmonotone_diagnosis_v2.png  # SA1: Non-monotonicity diagnosis
│   ├── figSA2_exp4_regularization_decomposition_v2.png  # SA2: Type-level decomposition
│   └── figSA3_exp9_cpu_dense_retrieval_v2.png    # SA3: CPU dense retrieval comparison
│
├── sql_only_exploration_output/              # CSV outputs from SQL_Exploration.R (28 files)
├── SQL_Exploration.R                         # R script for corpus exploration
│
├── LICENSE
└── README.md
```

---

## 2. Dataset Overview

The corpus consists of **2,733 documents** (911 per type), mapping to distinct cognitive memory functions in the supply chain AI architecture:

| Document Type | Cognitive Role | Description | Example Fields |
| :--- | :--- | :--- | :--- |
| **SHIPPING_ORDER** | Episodic Memory | Transactional records for operational coordination and disruption response | Order ID, Client, Origin, Destination, Priority, Cost |
| **WAREHOUSE_POLICY** | Procedural Memory | SOPs for warehouse operations and regulatory compliance | Policy ID, Facility, Safety Protocols, Inventory Procedures |
| **CUSTOMER_REQUIREMENT** | Declarative Memory | SLA specifications for service design and performance evaluation | Requirement ID, Client, Service Type, Performance Metrics |

**Corpus statistics (Table 2 in paper):**

| Document Type | N | Mean Length (chars) | Mean Words |
|:---|:---|:---|:---|
| Shipping Order | 911 | 1,567.1 | 138.4 |
| Warehouse Policy | 911 | 3,985.4 | 385.6 |
| Customer Requirement | 911 | 4,090.4 | 395.1 |

The shorter, entity-dense structure of Shipping Orders is central to the compression vulnerability findings in RQ6.

---

## 3. Key Research Findings

This corpus was used to evaluate six research questions. The key empirical results are:

| Finding | Result | Figure |
|:---|:---|:---|
| Optimal dimensionality knee point | 2,048-d retains 97.8% fidelity, cuts latency by 75% | Fig. 8 |
| IVF regularization effect | IVF-Flat outperforms exact Flat by **+8.9% nDCG@10** (nlist=64, nprobe=4) | Fig. R4 |
| GT weight robustness | Mean Spearman ρ = 0.985 across 81 weight combinations | Fig. R2 |
| Key-context fusion | **+7.5–10.9% nDCG@10** improvement; +117% for SKU-specific queries | Fig. R8 |
| Compression disparity | Shipping order retrieval **73% more vulnerable** to compression than warehouse policy | Fig. R5 |
| Noise robustness | Directional consistency confirmed; non-monotone patterns at 2,048-d within sampling variance | Fig. R1 |
| Entity-Overlap Recall (EOR) | GT-agnostic metric for cross-paradigm comparison; IVF-Flat EOR = 0.112 at 0.04 ms | Fig. SA3 |

---

## 4. Supplementary Figures

All figures in the `figures/` directory address specific reviewer comments and extend the main paper results. The table below maps each figure to its purpose:

| Figure | Experiment | Addresses | Description |
|:---|:---|:---|:---|
| `fig_R1` | EXP1 | Synthetic corpus robustness | nDCG@10 retention heatmap across noise levels (0–10%) and dimensions |
| `fig_R2` | EXP2 | GT weight robustness | Spearman ρ distribution for 81 weight combinations; heatmap by w_rules × w_labels |
| `fig_R3` | EXP3 | Agentic policy justification | Decision space: (QSR, latency budget) → optimal FAISS index |
| `fig_R4` | EXP4 | IVF regularization mechanism | Non-monotone nDCG@10 peak (nprobe=4) + actual Voronoi cluster purity by type |
| `fig_R5` | EXP5 | Fairness operationalization | QSR floor, equity index (1 − gap/max_nDCG), disparity widening |
| `fig_R6` | EXP6 | Cognitive architecture detail | Formal mapping: Perception → Working Memory → Episodic/Procedural/Declarative → Control → Decision |
| `fig_R7` | EXP7 | Latency unit reconciliation | Waterfall decomposition: FAISS search (1.24 ms) vs. end-to-end (1,589 ms) |
| `fig_R8` | EXP8 | Key-context subspace justification | All 4 split configurations outperform single-tower baseline; query-type breakdown |
| `fig_R9` | EXP9 | SPLADE/DPR positioning | GPU-only methods are not edge-deployable; latency and memory comparison |
| `figSA1` | SA1 | Non-monotonicity diagnosis | Bootstrap CIs confirm 2,048-d pattern is within sampling variance; noise-GT alignment ≈ 0 |
| `figSA2` | SA2 | IVF type-level decomposition | Per-type gain, cluster purity vs. gain scatter, counterfactual analysis |
| `figSA3` | SA3 | CPU dense retrieval | Type-match QSR (GT-agnostic); latency comparison including SentenceTransformer and BM25 |

### EXP4 — Note on Reported Values

The paper's benchmark nDCG values (Flat = 0.0621, IVF-Flat = 0.0676, aggregate gain = +8.9%) are retained as the authoritative results. The cluster purity analysis in `fig_R4b` provides the mechanistic explanation for the regularization effect: warehouse policy and customer requirement documents form coherent Voronoi clusters (purity 0.45–0.50 at nlist=64), while shipping orders are semantically dispersed (purity = 0.047) — consistent with the compression vulnerability documented in RQ6.

---

## 5. Cognitive Architecture Mapping

The system maps each component to a specific cognitive function (`fig_R6`):

| Cognitive Function | System Component | Memory Type |
|:---|:---|:---|
| Perception | LLM embedding service (d=8,192; key-context split) | — |
| Working Memory | Query vector + transient top-K result set | Baddeley (1974) |
| Episodic Memory | SHIPPING_ORDER FAISS index (IVF-PQ) | Tulving (1972) |
| Procedural Memory | WAREHOUSE_POLICY FAISS index (IVF-Flat) | Cohen & Squire (1980) |
| Declarative Memory | CUSTOMER_REQ FAISS index (IVF-Flat) | Tulving (1972) |
| Cognitive Control | Agentic index selection policy (Table 15) | Gheibi et al. (2021) |
| Decision Support | Ranked output → RAG / LLM reasoning | Constantiou et al. (2014) |

---

## 6. Getting Started

### Requirements

```bash
pip install faiss-cpu numpy pandas matplotlib scipy rank_bm25 scikit-learn
# Optional for EXP9 dense retrieval:
pip install sentence-transformers
```

For the R exploration script:
```r
install.packages(c("DBI", "RSQLite", "dplyr", "readr", "stringr", "tidyr"))
```

### Run Supplementary Analysis

Place the three FAISS index files and `logistics_knowledge.db` in the same directory as the script, then:

```bash
cd analysis/
python cognitive_retrieval_analysis.py
```

This generates all R1–R9 and SA1–SA3 figures into a `figures/` subdirectory. Experiments that do not require the live FAISS data (EXP2, EXP3, EXP5, EXP7, EXP8, EXP9, and the EXP1 heatmap) use pre-computed values from the paper and run without the data files.

### Run SQL Corpus Exploration

```bash
Rscript SQL_Exploration.R
```

This generates 28 CSV files in `sql_only_exploration_output/`, covering document statistics, shipping lane analysis, Incoterm distributions, warehouse topic signals, and performance metrics.

---

## 7. Ground Truth Construction

The hybrid ground truth combines four signals with weights {neighbors:1, bm25:1, rules:2, labels:3}:

| Signal | Method | Weight |
|:---|:---|:---|
| Semantic (neighbors) | KNN in 8,192-d embedding space | 1 |
| Lexical (BM25) | BM25Okapi on full document text | 1 |
| Entity-rules | Regex overlap on SKUs, Incoterms, order IDs | 2 |
| Label match | Same document type | 3 |

**Robustness:** Sensitivity analysis across 81 weight combinations yields mean Spearman ρ = 0.985 (SD = 0.012), with 100% of combinations exceeding ρ = 0.90. The higher weights for entity-rules and labels reflect the operational priority of exact identifier matching in supply chain contexts.

---

## 8. Deployment Playbook

Based on the empirical results, the recommended index configurations are:

| Scenario | Recommended Index | nDCG@10 | Latency |
|:---|:---|:---|:---|
| Interactive search, < 10k docs | Flat | 0.062 | 1.2 ms |
| Production, quality priority | IVF-Flat (nlist=64, nprobe=4) | 0.076 | 1.1 ms |
| Memory-constrained edge | IVF-PQ (m=64) | 0.050 | 0.4 ms |
| Balanced production | SQ8-IVF-Flat | 0.060 | 1.9 ms |

**Fairness governance:** Monitor type-level nDCG and QSR as first-class KPIs. The equity index (1 − gap/max_nDCG) should be reported alongside aggregate metrics; values below 0.95 indicate actionable disparity. At dim ≤ 128, shipping order QSR drops to 88.2%, breaching the 90% operational floor.

---

## 9. Key Findings from SQL Exploration

The `sql_only_exploration_output/` directory contains 28 datasets. Four highlights:

**Document Distribution:** All three types contain exactly 911 documents, confirming the balanced corpus design.

**Top Shipping Lanes:** Changwon→Ulsan and Seoul→Gwangju are the most frequent routes (8 shipments each).

**System Performance:** Mean end-to-end query latency is 1.57–1.60 s across document types, with recall = 1.000 across all types — establishing the baseline for Section 5.

**Warehouse Policy Topics:** FIFO and temperature control appear in 100% of warehouse policy documents; cross-docking and cycle counting appear in approximately 5%.

---

## 10. License and Citation

This repository is released under the [MIT License](LICENSE). The synthetic corpus is entirely fictional.

**Data availability:** The corpus, FAISS indices, and all analysis scripts are publicly available at [https://github.com/LEEYJ1021/Synthetic-Logistics-Knowledge-Corpus](https://github.com/LEEYJ1021/Synthetic-Logistics-Knowledge-Corpus) under the MIT license.

**Declaration:** During manuscript preparation, DeepSeek was used for language polishing. All content was reviewed and edited by the authors.
