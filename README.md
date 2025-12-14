# Synthetic Logistics Knowledge Corpus for Cognitive Retrieval Research

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Language: R](https://img.shields.io/badge/Language-R-blue.svg)]()
[![Database: SQLite](https://img.shields.io/badge/Database-SQLite-blue.svg)]()

This repository provides the synthetic dataset, analysis scripts, and exploratory results associated with the research paper:

**Title:** *Trade-off-Aware Cognitive Architectures for Supply Chain Knowledge Retrieval: From Dimensionality Reduction to Autonomous Optimization*

The primary goal of this repository is to ensure the transparency, reproducibility, and verifiability of our research findings by making the underlying data and exploration methodology publicly available.

---

## ⚠️ Critical Disclaimer: Synthetic Data

**This repository contains a dataset that is entirely synthetic and was generated programmatically using a Large Language Model (LLM).**

> The data **does not** represent any real-world logistics operations, transactions, companies, or individuals. All entities, including client names, locations, order IDs, and policy details, are fictional and were created based on a lightweight logistics ontology to simulate a realistic knowledge base for research purposes. The dataset was specifically designed to evaluate the performance of cognitive retrieval architectures as described in our paper and should not be used for any other purpose.

---

## 1. Dataset Overview

The core of this repository is the `logistics_knowledge.db`, an SQLite database that functions as the "perception and memory" layer for a cognitive supply chain AI system. As detailed in Section 3 of our paper, the corpus was generated to be semantically coherent and structurally diverse, enabling rigorous evaluation of retrieval systems.

The corpus consists of 2,733 documents, equally distributed across three distinct types that map to different aspects of supply chain resilience:

| Document Type | Description & Use Case | Example Fields |
| :--- | :--- | :--- |
| **SHIPPING_ORDER** | Transactional documents detailing specific shipments. Represents *episodic memory* for operational coordination and disruption response. | `Order ID`, `Client`, `Origin`, `Destination`, `Product Category`, `Delivery Priority`, `Cost Breakdown` |
| **WAREHOUSE_POLICY** | Procedural documents defining Standard Operating Procedures (SOPs). Represents *procedural memory* for compliance and operational consistency. | `Policy ID`, `Facility Location`, `Safety Protocols`, `Inventory Management Procedures`, `Quality Control Guidelines` |
| **CUSTOMER_REQUIREMENT** | Contractual specifications outlining Service Level Agreements (SLAs). Represents *declarative memory* for service design and performance evaluation. | `Requirement ID`, `Client Name`, `Service Type`, `Performance Metrics (e.g., On-Time Delivery)`, `Contractual Terms` |

---

## 2. Repository Structure
```text
├── logistics_knowledge.db             # The synthetic SQLite database (2,733 documents).
├── SQL_Exploration.R                  # R script for robust SQL-based data exploration and CSV report generation.
├── sql_only_exploration_output/       # Directory containing the CSV outputs from the script (28 datasets).
├── .gitignore
├── LICENSE
└── README.md                          # Project documentation.
```
---

## 3. Getting Started & Usage

### Step 1: Run the SQL-Based Exploration

`SQL_Exploration.R`, connects directly to the `logistics_knowledge.db` file, performs a deep, SQL-native analysis, and exports the results into a series of `.csv` files inside the `sql_only_exploration_output` directory.

This will generate approximately 28 CSV files containing detailed statistics, schema information, and logistics-specific analyses (e.g., top shipping lanes, Incoterm distribution, performance metrics).

### Step 2: Explore the Results Interactively

`SQL_Exploration.R`, is designed to load, analyze, and visualize the CSV files generated in the previous step. It provides a rich, user-friendly interface in the R console for exploring the data.

1.  Open `SQL_Exploration.R` in your R environment (e.g., RStudio).
2.  Modify the `db_path` variable to point to the correct location of `logistics_knowledge.db` on your system.
3.  Run the entire script.
4.  Ensure the `sql_only_exploration_output` directory exists and contains the CSV files.

The script will automatically detect the output directory, list all available datasets, and provide a summary analysis for key tables, demonstrating the structure and content of the exploration results.

Here is a summary of the key findings from the exploration results, focusing on data quality, logistics, and performance.

---

## 4. Key Findings from Enhanced SQL Exploration

The exploration generated 28 datasets, and a detailed look at four of them reveals critical insights into the dataset's composition, data quality, and system performance.

### 4-1. Document Distribution (`doc_counts_by_type`)

* **Equal Document Count:** The dataset shows an **identical count of 911** for all three document types: **Customer requirement**, **Shipping order**, and **Warehouse policy**.
    * This suggests a balanced dataset in terms of document type population.
* **Data Quality Note:** The `n_docs` column is constant across all rows, which is flagged as a quality issue, but it is expected since it reports the total document count per type, and in this case, the count is the same for all types.

| doc\_type\_label | n\_docs |
| :--- | :--- |
| Customer requirement | 911 |
| Shipping order | 911 |
| Warehouse policy | 911 |

### 4-2. Top Shipping Lanes (`shipping_top_lanes`)

This dataset identifies the most frequently used shipping routes and their characteristics.

* **Most Active Lanes (Top 2):**
    * **Changwon to Ulsan** (8 shipments)
    * **Seoul to Gwangju** (8 shipments)
* **Data Quality Warning:** The column `avg_sku_count` is **100% missing (All NA)**. This column is effectively unusable for analysis, indicating a data collection or processing failure for SKU count metrics per lane.
* **Average Priority Score:** The average priority score across the top 25 lanes ranges from **1.8 to 2.8**, with an overall mean of **2.31**. The top two lanes have scores of 2.29 and 2.25, indicating an average priority level for the most frequent routes.
    * 

### 4-3. System Performance by Document Type (`performance_summary_by_type`)

This summary indicates that performance metrics are highly consistent across the three document types.

* **Query Time:** The **mean query time** is nearly identical for all types, hovering around **1.59 seconds** (ranging from 1.57s to 1.60s).
* **F1 Score:** The **mean F1 score** is also extremely consistent, around **1.33**.
* **Recall Warning:** The `mean_recall` column is a constant value of **1** for all document types. This is highly unusual and suggests a potential issue in how the recall metric is being calculated or reported in the system, as a perfect recall across all test sets is rare.

| doc\_type\_label | n\_queries | mean\_query\_time\_s | mean\_precision | mean\_recall | mean\_f1 |
| :--- | :--- | :--- | :--- | :--- | :--- |
| Customer requirement | 51 | 1.60 | 2.00 | 1 | 1.33 |
| Shipping order | 52 | 1.57 | 1.98 | 1 | 1.33 |
| Warehouse policy | 51 | 1.60 | 1.98 | 1 | 1.33 |


### 4-4. Warehouse Policy Topic Signals (`warehouse_policy_topic_signals`)

This provides insights into the topics covered in the Warehouse Policy documents.

* **Universal Topics (100% Coverage):**
    * **fifo** (First-In, First-Out)
    * **temperature\_control**
    * All 911 Warehouse Policy documents contain signals for both of these topics.
* **Niche/Rare Topics (Low Coverage):**
    * **cross\_dock** (5.05%)
    * **cycle\_count** (5.05%)
    * **picking** (4.94%)
* **Uncovered/Zero-Signal Topics:**
    * **fefo** (First-Expired, First-Out)
    * **hazmat** (Hazardous Materials)
    * **pallet**
    * **putaway**

| topic | n | pct\_of\_warehouse\_policies |
| :--- | :--- | :--- |
| **fifo** | 911 | 100 |
| **temperature\_control** | 911 | 100 |
| cross\_dock | 46 | 5.05 |
| cycle\_count | 46 | 5.05 |
| picking | 45 | 4.94 |


---

## 5. Analysis Scripts

The full R code used for the exploration is provided below for complete transparency.

<details>
<summary><strong>Click to view `SQL_Exploration`</strong></summary>

```r
# ============================================================
# Appendix A (Robust): SQL-Focused Exploration of a Logistics SQLite DB
# Language: R (English)
# Key upgrades:
#   - Fixes "malformed JSON" by NEVER applying JSON functions to non-JSON columns
#   - Guards all json_extract/json_array_length/json_type with json_valid(metadata)=1
#   - Replaces non-portable STDDEV() with a portable SQL SD formula
#   - Adds logistics-relevant analyses (lanes, Incoterms, priority, mode, topics)
# ============================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

options(stringsAsFactors = FALSE)

# -------------------------
# 0) Configuration
# -------------------------
db_path <- "logistics_knowledge.db" # Assumes DB is in the same directory
output_dir <- "sql_only_exploration_output"

# Optional: create report tables inside the same SQLite DB (safe, prefixed with rpt_)
materialize_reports <- TRUE

# -------------------------
# 1) Utilities
# -------------------------
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("Database file not found:\n  ", path, call. = FALSE)
  }
}

write_csv_safely <- function(df, file) {
  tryCatch({
    readr::write_csv(df, file)
    message("Saved: ", file)
  }, error = function(e) {
    warning("Failed to save ", file, ": ", e$message, call. = FALSE)
  })
}

write_lines_safely <- function(lines, file) {
  tryCatch({
    writeLines(lines, con = file, useBytes = TRUE)
    message("Saved: ", file)
  }, error = function(e) {
    warning("Failed to save ", file, ": ", e$message, call. = FALSE)
  })
}

sql_query <- function(con, sql, name = NULL) {
  tryCatch({
    DBI::dbGetQuery(con, sql)
  }, error = function(e) {
    msg <- paste0(
      "SQL query failed", if (!is.null(name)) paste0(" [", name, "]") else "", ":\n",
      e$message, "\n\n--- SQL ---\n", sql, "\n"
    )
    warning(msg, call. = FALSE)
    # Save failing SQL for debugging
    if (!is.null(name)) {
      file <- file.path(output_dir, paste0("FAILED_SQL_", make.names(name), ".sql"))
      write_lines_safely(sql, file)
    }
    data.frame()
  })
}

sql_exec <- function(con, sql, name = NULL) {
  tryCatch({
    DBI::dbExecute(con, sql)
  }, error = function(e) {
    msg <- paste0(
      "SQL execute failed", if (!is.null(name)) paste0(" [", name, "]") else "", ":\n",
      e$message, "\n\n--- SQL ---\n", sql, "\n"
    )
    warning(msg, call. = FALSE)
    # Save failing SQL for debugging
    if (!is.null(name)) {
      file <- file.path(output_dir, paste0("FAILED_SQL_", make.names(name), ".sql"))
      write_lines_safely(sql, file)
    }
    invisible(NULL)
  })
}

has_table <- function(con, tbl) DBI::dbExistsTable(con, tbl)

table_cols <- function(con, tbl) {
  if (!has_table(con, tbl)) return(character())
  DBI::dbListFields(con, tbl)
}

has_col <- function(con, tbl, col) col %in% table_cols(con, tbl)

sqlite_has_json1 <- function(con) {
  # If json_valid() exists, JSON1 is available.
  ok <- tryCatch({
    x <- dbGetQuery(con, "SELECT json_valid('{\"a\": 1}') AS ok;")
    isTRUE(x$ok == 1)
  }, error = function(e) FALSE)
  ok
}

percentile <- function(x, p) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  x <- sort(x)
  idx <- ceiling(p * length(x))
  idx <- max(1, min(idx, length(x)))
  x[[idx]]
}

quantile_summary <- function(x) {
  data.frame(
    p50 = percentile(x, 0.50),
    p90 = percentile(x, 0.90),
    p95 = percentile(x, 0.95),
    p99 = percentile(x, 0.99),
    stringsAsFactors = FALSE
  )
}

doc_type_label_case <- "
CASE lower(replace(doc_type,' ','_'))
  WHEN 'shipping_order' THEN 'Shipping order'
  WHEN 'warehouse_policy' THEN 'Warehouse policy'
  WHEN 'customer_requirement' THEN 'Customer requirement'
  ELSE doc_type
END
"

# Portable (sample) SD formula in SQL for a numeric column x:
# sd = sqrt( (sum(x^2) - sum(x)^2/n) / (n-1) ), when n>1
sd_sql <- function(x) {
  paste0(
    "CASE WHEN COUNT(", x, ") > 1 THEN ",
    "sqrt( (SUM(", x, "*", x, ") - (SUM(", x, ")*SUM(", x, "))/COUNT(", x, ")) / (COUNT(", x, ")-1) ) ",
    "END"
  )
}

# -------------------------
# 2) Connect
# -------------------------
stop_if_missing(db_path)

con <- dbConnect(RSQLite::SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

dbExecute(con, "PRAGMA foreign_keys = ON;")
dbExecute(con, "PRAGMA busy_timeout = 5000;")

json1_ok <- sqlite_has_json1(con)

# -------------------------
# 3) Schema inspection (SQL)
# -------------------------
schema_objects <- sql_query(con, "
SELECT
  type,
  name AS object_name,
  tbl_name AS table_name,
  sql
FROM sqlite_master
WHERE type IN ('table','view')
ORDER BY type, name;
", name = "schema_objects")

write_csv_safely(schema_objects, file.path(output_dir, "schema_objects.csv"))

# Column dictionary for all tables
tables <- schema_objects %>% filter(type == "table") %>% pull(object_name)

schema_cols <- lapply(tables, function(t) {
  # PRAGMA table_info('table') is safe with quotes
  df <- sql_query(con, sprintf("PRAGMA table_info('%s');", t), name = paste0("pragma_", t))
  if (nrow(df) == 0) return(NULL)
  df$table_name <- t
  df
}) %>% bind_rows()

write_csv_safely(schema_cols, file.path(output_dir, "schema_columns.csv"))

# Row counts for each table (fast sanity check)
table_row_counts <- lapply(tables, function(t) {
  df <- sql_query(con, sprintf("SELECT '%s' AS table_name, COUNT(*) AS n FROM %s;", t, t),
                  name = paste0("count_", t))
  df
}) %>% bind_rows()

write_csv_safely(table_row_counts, file.path(output_dir, "table_row_counts.csv"))

# -------------------------
# 4) Create robust SQL views
# -------------------------
if (!has_table(con, "documents")) {
  stop("Required table 'documents' not found in the database.", call. = FALSE)
}

# 4.1 Base document view (no JSON usage here)
sql_exec(con, "DROP VIEW IF EXISTS v_documents_base;", "drop_v_documents_base")

sql_exec(con, paste0("
CREATE VIEW v_documents_base AS
SELECT
  doc_id,
  doc_type,
  lower(replace(doc_type,' ','_')) AS doc_type_norm,
  ", doc_type_label_case, " AS doc_type_label,
  COALESCE(content, '') AS content_safe,
  LENGTH(COALESCE(content, '')) AS content_length,
  timestamp,
  substr(CAST(timestamp AS TEXT), 1, 10) AS day_key,
  embedding_hash,
  metadata
FROM documents;
"), "create_v_documents_base")

# 4.2 Word-count approximation view (still no JSON usage)
sql_exec(con, "DROP VIEW IF EXISTS v_documents_wc;", "drop_v_documents_wc")

sql_exec(con, paste0("
CREATE VIEW v_documents_wc AS
SELECT
  doc_id,
  doc_type_norm,
  doc_type_label,
  day_key,
  content_length,
  content_ws,
  CASE
    WHEN content_ws = '' THEN 0
    ELSE (LENGTH(content_ws) - LENGTH(REPLACE(content_ws, ' ', '')) + 1)
  END AS word_count
FROM (
  SELECT
    doc_id,
    lower(replace(doc_type,' ','_')) AS doc_type_norm,
    ", doc_type_label_case, " AS doc_type_label,
    substr(CAST(timestamp AS TEXT), 1, 10) AS day_key,
    LENGTH(COALESCE(content, '')) AS content_length,
    trim(
      replace(replace(replace(
        replace(replace(replace(replace(COALESCE(content,''), char(13), ' '), char(10), ' '), char(9), ' '),
        '  ', ' '), '  ', ' '), '  ', ' '), '  ', ' ')
    ) AS content_ws
  FROM documents
);
"), "create_v_documents_wc")

# 4.3 Metadata extraction view (JSON only on metadata, guarded by json_valid)
meta_view_created <- FALSE
if (has_col(con, "documents", "metadata") && json1_ok) {
  
  sql_exec(con, "DROP VIEW IF EXISTS v_documents_meta_extracted;", "drop_v_documents_meta_extracted")
  
  sql_exec(con, paste0("
  CREATE VIEW v_documents_meta_extracted AS
  SELECT
    doc_id,
    doc_type_norm,
    doc_type_label,
    day_key,
    content_length,
    embedding_hash,
    metadata_text,
    metadata_is_valid,

    -- Common logistics identifiers (robust COALESCE across likely keys)
    COALESCE(
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.order_id') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.shipment_id') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.so_id') END
    ) AS order_id,

    COALESCE(
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.client') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.customer') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.customer_name') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.consignee') END
    ) AS client_name,

    COALESCE(
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.origin') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.ship_from') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.pickup_location') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.origin_port') END
    ) AS origin,

    COALESCE(
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.destination') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.ship_to') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.delivery_location') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.destination_port') END
    ) AS destination,

    COALESCE(
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.incoterms') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.incoterm') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.inco_terms') END
    ) AS incoterms_raw,

    COALESCE(
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.priority') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.service_level') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.urgency') END
    ) AS priority_raw,

    COALESCE(
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.mode') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.transport_mode') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.shipment_mode') END
    ) AS mode_raw,

    COALESCE(
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.carrier') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.carrier_name') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.forwarder') END
    ) AS carrier,

    COALESCE(
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.warehouse') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.warehouse_id') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.facility') END,
      CASE WHEN metadata_is_valid=1 THEN json_extract(metadata_text,'$.dc') END
    ) AS warehouse,

    -- SKU/item count: arrays OR explicit count fields
    COALESCE(
      CASE
        WHEN metadata_is_valid=1 AND json_type(metadata_text,'$.skus')='array'
          THEN json_array_length(metadata_text,'$.skus')
      END,
      CASE
        WHEN metadata_is_valid=1 AND json_type(metadata_text,'$.items')='array'
          THEN json_array_length(metadata_text,'$.items')
      END,
      CASE
        WHEN metadata_is_valid=1 AND json_type(metadata_text,'$.lines')='array'
          THEN json_array_length(metadata_text,'$.lines')
      END,
      CASE WHEN metadata_is_valid=1 THEN CAST(json_extract(metadata_text,'$.sku_count') AS INTEGER) END,
      CASE WHEN metadata_is_valid=1 THEN CAST(json_extract(metadata_text,'$.item_count') AS INTEGER) END
    ) AS sku_count

  FROM (
    SELECT
      doc_id,
      lower(replace(doc_type,' ','_')) AS doc_type_norm,
      ", doc_type_label_case, " AS doc_type_label,
      substr(CAST(timestamp AS TEXT), 1, 10) AS day_key,
      LENGTH(COALESCE(content, '')) AS content_length,
      embedding_hash,
      CAST(metadata AS TEXT) AS metadata_text,
      COALESCE(json_valid(CAST(metadata AS TEXT)), 0) AS metadata_is_valid
    FROM documents
  ) base;
  "), "create_v_documents_meta_extracted")
  
  meta_view_created <- TRUE
} else {
  warning("Metadata extraction view skipped: either 'metadata' column missing or JSON1 unavailable.", call. = FALSE)
}

# 4.4 Shipping-order enriched view (metadata + content fallback parsing)
#     This ensures you still get logistics outputs even when metadata is missing/invalid.
sql_exec(con, "DROP VIEW IF EXISTS v_shipping_orders_enriched;", "drop_v_shipping_orders_enriched")

if (meta_view_created) {
  
  sql_exec(con, "
  CREATE VIEW v_shipping_orders_enriched AS
  SELECT
    doc_id,
    day_key,
    content_length,

    -- Prefer metadata; fallback to content parsing for structured synthetic text
    NULLIF(trim(COALESCE(client_name, client_from_content)), '') AS client_name,

    NULLIF(trim(COALESCE(origin, origin_from_content)), '') AS origin,
    NULLIF(trim(COALESCE(destination, destination_from_content)), '') AS destination,

    -- Incoterms standardized to 3-letter code when possible
    NULLIF(upper(substr(trim(COALESCE(incoterms_raw, incoterms_from_content)), 1, 3)), '') AS incoterms_code,

    -- Priority normalized
    NULLIF(lower(trim(COALESCE(priority_raw, priority_from_content))), '') AS priority,

    -- Mode normalized (simple mapping)
    CASE
      WHEN lower(trim(COALESCE(mode_raw, mode_from_content))) LIKE '%air%' THEN 'air'
      WHEN lower(trim(COALESCE(mode_raw, mode_from_content))) LIKE '%sea%' OR lower(trim(COALESCE(mode_raw, mode_from_content))) LIKE '%ocean%' THEN 'ocean'
      WHEN lower(trim(COALESCE(mode_raw, mode_from_content))) LIKE '%road%' OR lower(trim(COALESCE(mode_raw, mode_from_content))) LIKE '%truck%' THEN 'road'
      WHEN lower(trim(COALESCE(mode_raw, mode_from_content))) LIKE '%rail%' THEN 'rail'
      WHEN lower(trim(COALESCE(mode_raw, mode_from_content))) LIKE '%courier%' OR lower(trim(COALESCE(mode_raw, mode_from_content))) LIKE '%parcel%' OR lower(trim(COALESCE(mode_raw, mode_from_content))) LIKE '%express%' THEN 'parcel'
      ELSE NULLIF(lower(trim(COALESCE(mode_raw, mode_from_content))), '')
    END AS mode,

    carrier,

    -- SKU count: metadata first, fallback by counting 'SKU' occurrences in content
    COALESCE(
      sku_count,
      CASE
        WHEN instr(upper(content_safe), 'SKU') > 0 THEN
          (LENGTH(upper(content_safe)) - LENGTH(REPLACE(upper(content_safe), 'SKU', ''))) / 3
      END
    ) AS sku_count,

    -- Priority score for lane-level aggregation
    CASE
      WHEN lower(trim(COALESCE(priority_raw, priority_from_content))) IN ('high','urgent','expedite','express') THEN 3
      WHEN lower(trim(COALESCE(priority_raw, priority_from_content))) IN ('medium','normal','standard') THEN 2
      WHEN lower(trim(COALESCE(priority_raw, priority_from_content))) IN ('low','economy','deferred') THEN 1
      ELSE NULL
    END AS priority_score

  FROM (
    SELECT
      b.doc_id,
      b.day_key,
      b.content_length,
      b.content_safe,
      m.client_name,
      m.origin,
      m.destination,
      m.incoterms_raw,
      m.priority_raw,
      m.mode_raw,
      m.carrier,
      m.sku_count,

      -- Content parsing helpers (extract line value after a label until newline)
      -- NOTE: These are safe string ops (no JSON); they will return NULL when label not found.

      -- Client
      NULLIF(trim(
        CASE
          WHEN instr(b.content_safe, 'Customer:') > 0 THEN
            CASE
              WHEN instr(substr(b.content_safe, instr(b.content_safe,'Customer:') + 9), char(10)) > 0 THEN
                substr(substr(b.content_safe, instr(b.content_safe,'Customer:') + 9), 1,
                       instr(substr(b.content_safe, instr(b.content_safe,'Customer:') + 9), char(10)) - 1)
              ELSE substr(b.content_safe, instr(b.content_safe,'Customer:') + 9)
            END
          WHEN instr(b.content_safe, 'Client:') > 0 THEN
            CASE
              WHEN instr(substr(b.content_safe, instr(b.content_safe,'Client:') + 7), char(10)) > 0 THEN
                substr(substr(b.content_safe, instr(b.content_safe,'Client:') + 7), 1,
                       instr(substr(b.content_safe, instr(b.content_safe,'Client:') + 7), char(10)) - 1)
              ELSE substr(b.content_safe, instr(b.content_safe,'Client:') + 7)
            END
        END
      ), '') AS client_from_content,

      -- Origin
      NULLIF(trim(
        CASE
          WHEN instr(b.content_safe, 'Origin:') > 0 THEN
            CASE
              WHEN instr(substr(b.content_safe, instr(b.content_safe,'Origin:') + 7), char(10)) > 0 THEN
                substr(substr(b.content_safe, instr(b.content_safe,'Origin:') + 7), 1,
                       instr(substr(b.content_safe, instr(b.content_safe,'Origin:') + 7), char(10)) - 1)
              ELSE substr(b.content_safe, instr(b.content_safe,'Origin:') + 7)
            END
          WHEN instr(b.content_safe, 'From:') > 0 THEN
            CASE
              WHEN instr(substr(b.content_safe, instr(b.content_safe,'From:') + 5), char(10)) > 0 THEN
                substr(substr(b.content_safe, instr(b.content_safe,'From:') + 5), 1,
                       instr(substr(b.content_safe, instr(b.content_safe,'From:') + 5), char(10)) - 1)
              ELSE substr(b.content_safe, instr(b.content_safe,'From:') + 5)
            END
        END
      ), '') AS origin_from_content,

      -- Destination
      NULLIF(trim(
        CASE
          WHEN instr(b.content_safe, 'Destination:') > 0 THEN
            CASE
              WHEN instr(substr(b.content_safe, instr(b.content_safe,'Destination:') + 12), char(10)) > 0 THEN
                substr(substr(b.content_safe, instr(b.content_safe,'Destination:') + 12), 1,
                       instr(substr(b.content_safe, instr(b.content_safe,'Destination:') + 12), char(10)) - 1)
              ELSE substr(b.content_safe, instr(b.content_safe,'Destination:') + 12)
            END
          WHEN instr(b.content_safe, 'To:') > 0 THEN
            CASE
              WHEN instr(substr(b.content_safe, instr(b.content_safe,'To:') + 3), char(10)) > 0 THEN
                substr(substr(b.content_safe, instr(b.content_safe,'To:') + 3), 1,
                       instr(substr(b.content_safe, instr(b.content_safe,'To:') + 3), char(10)) - 1)
              ELSE substr(b.content_safe, instr(b.content_safe,'To:') + 3)
            END
        END
      ), '') AS destination_from_content,

      -- Incoterms
      NULLIF(trim(
        CASE
          WHEN instr(b.content_safe, 'Incoterms:') > 0 THEN
            CASE
              WHEN instr(substr(b.content_safe, instr(b.content_safe,'Incoterms:') + 10), char(10)) > 0 THEN
                substr(substr(b.content_safe, instr(b.content_safe,'Incoterms:') + 10), 1,
                       instr(substr(b.content_safe, instr(b.content_safe,'Incoterms:') + 10), char(10)) - 1)
              ELSE substr(b.content_safe, instr(b.content_safe,'Incoterms:') + 10)
            END
        END
      ), '') AS incoterms_from_content,

      -- Priority
      NULLIF(trim(
        CASE
          WHEN instr(b.content_safe, 'Priority:') > 0 THEN
            CASE
              WHEN instr(substr(b.content_safe, instr(b.content_safe,'Priority:') + 9), char(10)) > 0 THEN
                substr(substr(b.content_safe, instr(b.content_safe,'Priority:') + 9), 1,
                       instr(substr(b.content_safe, instr(b.content_safe,'Priority:') + 9), char(10)) - 1)
              ELSE substr(b.content_safe, instr(b.content_safe,'Priority:') + 9)
            END
        END
      ), '') AS priority_from_content,

      -- Mode
      NULLIF(trim(
        CASE
          WHEN instr(b.content_safe, 'Mode:') > 0 THEN
            CASE
              WHEN instr(substr(b.content_safe, instr(b.content_safe,'Mode:') + 5), char(10)) > 0 THEN
                substr(substr(b.content_safe, instr(b.content_safe,'Mode:') + 5), 1,
                       instr(substr(b.content_safe, instr(b.content_safe,'Mode:') + 5), char(10)) - 1)
              ELSE substr(b.content_safe, instr(b.content_safe,'Mode:') + 5)
            END
          WHEN instr(b.content_safe, 'Transport Mode:') > 0 THEN
            CASE
              WHEN instr(substr(b.content_safe, instr(b.content_safe,'Transport Mode:') + 15), char(10)) > 0 THEN
                substr(substr(b.content_safe, instr(b.content_safe,'Transport Mode:') + 15), 1,
                       instr(substr(b.content_safe, instr(b.content_safe,'Transport Mode:') + 15), char(10)) - 1)
              ELSE substr(b.content_safe, instr(b.content_safe,'Transport Mode:') + 15)
            END
        END
      ), '') AS mode_from_content

    FROM v_documents_base b
    LEFT JOIN v_documents_meta_extracted m
      ON b.doc_id = m.doc_id
    WHERE b.doc_type_norm = 'shipping_order'
  );
  ", "create_v_shipping_orders_enriched")
  
} else {
  # Metadata view not available; create shipping view purely from content parsing + fallbacks
  sql_exec(con, "
  CREATE VIEW v_shipping_orders_enriched AS
  SELECT
    doc_id,
    day_key,
    content_length,
    NULL AS client_name,
    NULL AS origin,
    NULL AS destination,
    NULL AS incoterms_code,
    NULL AS priority,
    NULL AS mode,
    NULL AS carrier,
    CASE
      WHEN instr(upper(content_safe), 'SKU') > 0 THEN
        (LENGTH(upper(content_safe)) - LENGTH(REPLACE(upper(content_safe), 'SKU', ''))) / 3
    END AS sku_count,
    NULL AS priority_score
  FROM v_documents_base
  WHERE doc_type_norm = 'shipping_order';
  ", "create_v_shipping_orders_enriched_no_meta")
}

# -------------------------
# 5) Document exploration (SQL outputs)
# -------------------------

# 5.1 Document counts by type
doc_counts <- sql_query(con, "
SELECT
  doc_type_label,
  COUNT(*) AS n_docs
FROM v_documents_base
GROUP BY doc_type_norm, doc_type_label
ORDER BY n_docs DESC;
", "doc_counts_by_type")
write_csv_safely(doc_counts, file.path(output_dir, "doc_counts_by_type.csv"))

# 5.2 Content length stats by type (NO JSON HERE; fixes your error)
doc_len_stats <- sql_query(con, paste0("
SELECT
  doc_type_label,
  COUNT(*) AS n_docs,
  ROUND(AVG(content_length), 1) AS mean_chars,
  ROUND(", sd_sql("content_length"), ", 1) AS sd_chars,
  MIN(content_length) AS min_chars,
  MAX(content_length) AS max_chars
FROM v_documents_base
GROUP BY doc_type_norm, doc_type_label
ORDER BY doc_type_label;
"), "doc_length_stats_by_type")
write_csv_safely(doc_len_stats, file.path(output_dir, "doc_length_stats_by_type.csv"))

# 5.3 Word count stats by type
doc_wc_stats <- sql_query(con, paste0("
SELECT
  doc_type_label,
  COUNT(*) AS n_docs,
  ROUND(AVG(word_count), 1) AS mean_words,
  ROUND(", sd_sql("word_count"), ", 1) AS sd_words,
  MIN(word_count) AS min_words,
  MAX(word_count) AS max_words
FROM v_documents_wc
GROUP BY doc_type_norm, doc_type_label
ORDER BY doc_type_label;
"), "doc_wordcount_stats_by_type")
write_csv_safely(doc_wc_stats, file.path(output_dir, "doc_wordcount_stats_by_type.csv"))

# 5.4 Embedding coverage (logistics KB readiness)
embedding_coverage <- sql_query(con, "
SELECT
  doc_type_label,
  COUNT(*) AS n_docs,
  SUM(CASE WHEN embedding_hash IS NOT NULL AND trim(embedding_hash) <> '' THEN 1 ELSE 0 END) AS n_with_embedding_hash,
  ROUND(100.0 * SUM(CASE WHEN embedding_hash IS NOT NULL AND trim(embedding_hash) <> '' THEN 1 ELSE 0 END) / COUNT(*), 2) AS pct_with_embedding_hash
FROM v_documents_base
GROUP BY doc_type_norm, doc_type_label
ORDER BY doc_type_label;
", "embedding_coverage")
write_csv_safely(embedding_coverage, file.path(output_dir, "embedding_coverage.csv"))

# 5.5 Logistics keyword signals in content (works even without metadata)
keyword_signals <- sql_query(con, "
SELECT
  doc_type_label,
  COUNT(*) AS n_docs,
  SUM(CASE WHEN upper(content_safe) LIKE '%SKU%' THEN 1 ELSE 0 END) AS docs_with_sku,
  SUM(CASE WHEN upper(content_safe) LIKE '%ETA%' OR upper(content_safe) LIKE '%ESTIMATED TIME OF ARRIVAL%' THEN 1 ELSE 0 END) AS docs_with_eta,
  SUM(CASE WHEN upper(content_safe) LIKE '%FOB%' OR upper(content_safe) LIKE '%CIF%' OR upper(content_safe) LIKE '%DDP%' OR upper(content_safe) LIKE '%DAP%' OR upper(content_safe) LIKE '%EXW%' THEN 1 ELSE 0 END) AS docs_with_incoterms_codes,
  SUM(CASE WHEN lower(content_safe) LIKE '%warehouse%' THEN 1 ELSE 0 END) AS docs_with_warehouse_term,
  SUM(CASE WHEN lower(content_safe) LIKE '%route%' THEN 1 ELSE 0 END) AS docs_with_route_term,
  SUM(CASE WHEN lower(content_safe) LIKE '%pallet%' THEN 1 ELSE 0 END) AS docs_with_pallet_term,
  SUM(CASE WHEN lower(content_safe) LIKE '%hazmat%' OR lower(content_safe) LIKE '%dangerous goods%' THEN 1 ELSE 0 END) AS docs_with_hazmat_term
FROM v_documents_base
GROUP BY doc_type_norm, doc_type_label
ORDER BY doc_type_label;
", "keyword_signals")
write_csv_safely(keyword_signals, file.path(output_dir, "keyword_signals_by_type.csv"))

# 5.6 Activity by day (if timestamps exist)
activity_by_day <- sql_query(con, "
SELECT
  doc_type_label,
  day_key,
  COUNT(*) AS n_docs
FROM v_documents_base
WHERE day_key IS NOT NULL AND day_key <> ''
GROUP BY doc_type_norm, doc_type_label, day_key
ORDER BY day_key, doc_type_label;
", "documents_activity_by_day")
write_csv_safely(activity_by_day, file.path(output_dir, "documents_activity_by_day.csv"))

# -------------------------
# 6) Logistics-focused exploration (shipping orders)
# -------------------------

# 6.1 Field coverage for shipping orders
shipping_coverage <- sql_query(con, "
SELECT
  COUNT(*) AS n_shipping_docs,
  SUM(CASE WHEN origin IS NOT NULL AND trim(origin) <> '' THEN 1 ELSE 0 END) AS n_with_origin,
  SUM(CASE WHEN destination IS NOT NULL AND trim(destination) <> '' THEN 1 ELSE 0 END) AS n_with_destination,
  SUM(CASE WHEN incoterms_code IS NOT NULL AND trim(incoterms_code) <> '' THEN 1 ELSE 0 END) AS n_with_incoterms,
  SUM(CASE WHEN priority IS NOT NULL AND trim(priority) <> '' THEN 1 ELSE 0 END) AS n_with_priority,
  SUM(CASE WHEN mode IS NOT NULL AND trim(mode) <> '' THEN 1 ELSE 0 END) AS n_with_mode,
  SUM(CASE WHEN client_name IS NOT NULL AND trim(client_name) <> '' THEN 1 ELSE 0 END) AS n_with_client,
  SUM(CASE WHEN sku_count IS NOT NULL THEN 1 ELSE 0 END) AS n_with_sku_count
FROM v_shipping_orders_enriched;
", "shipping_field_coverage")
write_csv_safely(shipping_coverage, file.path(output_dir, "shipping_field_coverage.csv"))

# 6.2 Top lanes (origin -> destination)
shipping_top_lanes <- sql_query(con, "
SELECT
  origin,
  destination,
  COUNT(*) AS n_shipments,
  ROUND(AVG(content_length), 1) AS avg_chars,
  ROUND(AVG(sku_count), 2) AS avg_sku_count,
  ROUND(AVG(priority_score), 2) AS avg_priority_score
FROM v_shipping_orders_enriched
WHERE origin IS NOT NULL AND destination IS NOT NULL
GROUP BY origin, destination
ORDER BY n_shipments DESC
LIMIT 25;
", "shipping_top_lanes")
write_csv_safely(shipping_top_lanes, file.path(output_dir, "shipping_top_lanes.csv"))

# 6.3 Top origins and destinations
shipping_top_origins <- sql_query(con, "
SELECT origin, COUNT(*) AS n
FROM v_shipping_orders_enriched
WHERE origin IS NOT NULL
GROUP BY origin
ORDER BY n DESC
LIMIT 25;
", "shipping_top_origins")
write_csv_safely(shipping_top_origins, file.path(output_dir, "shipping_top_origins.csv"))

shipping_top_destinations <- sql_query(con, "
SELECT destination, COUNT(*) AS n
FROM v_shipping_orders_enriched
WHERE destination IS NOT NULL
GROUP BY destination
ORDER BY n DESC
LIMIT 25;
", "shipping_top_destinations")
write_csv_safely(shipping_top_destinations, file.path(output_dir, "shipping_top_destinations.csv"))

# 6.4 Incoterms distribution (3-letter)
shipping_incoterms <- sql_query(con, "
SELECT
  incoterms_code,
  COUNT(*) AS n,
  ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM v_shipping_orders_enriched WHERE incoterms_code IS NOT NULL), 2) AS pct
FROM v_shipping_orders_enriched
WHERE incoterms_code IS NOT NULL
GROUP BY incoterms_code
ORDER BY n DESC;
", "shipping_incoterms_distribution")
write_csv_safely(shipping_incoterms, file.path(output_dir, "shipping_incoterms_distribution.csv"))

# 6.5 Priority distribution
shipping_priority <- sql_query(con, "
SELECT
  priority,
  COUNT(*) AS n,
  ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM v_shipping_orders_enriched WHERE priority IS NOT NULL), 2) AS pct
FROM v_shipping_orders_enriched
WHERE priority IS NOT NULL
GROUP BY priority
ORDER BY n DESC;
", "shipping_priority_distribution")
write_csv_safely(shipping_priority, file.path(output_dir, "shipping_priority_distribution.csv"))

# 6.6 Mode distribution
shipping_modes <- sql_query(con, "
SELECT
  mode,
  COUNT(*) AS n,
  ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM v_shipping_orders_enriched WHERE mode IS NOT NULL), 2) AS pct
FROM v_shipping_orders_enriched
WHERE mode IS NOT NULL
GROUP BY mode
ORDER BY n DESC;
", "shipping_mode_distribution")
write_csv_safely(shipping_modes, file.path(output_dir, "shipping_mode_distribution.csv"))

# 6.7 Top customers (if available)
shipping_top_customers <- sql_query(con, "
SELECT
  client_name,
  COUNT(*) AS n_shipments
FROM v_shipping_orders_enriched
WHERE client_name IS NOT NULL AND trim(client_name) <> ''
GROUP BY client_name
ORDER BY n_shipments DESC
LIMIT 25;
", "shipping_top_customers")
write_csv_safely(shipping_top_customers, file.path(output_dir, "shipping_top_customers.csv"))

# 6.8 SKU count distribution
shipping_sku_dist <- sql_query(con, "
SELECT
  sku_count,
  COUNT(*) AS n_docs
FROM v_shipping_orders_enriched
WHERE sku_count IS NOT NULL
GROUP BY sku_count
ORDER BY sku_count ASC;
", "shipping_sku_count_distribution")
write_csv_safely(shipping_sku_dist, file.path(output_dir, "shipping_sku_count_distribution.csv"))

# 6.9 High-priority + high SKU examples (useful in logistics discussion)
shipping_risky_examples <- sql_query(con, "
SELECT
  doc_id,
  day_key,
  origin,
  destination,
  incoterms_code,
  priority,
  mode,
  sku_count,
  content_length
FROM v_shipping_orders_enriched
WHERE sku_count IS NOT NULL
  AND priority_score = 3
ORDER BY sku_count DESC, content_length DESC
LIMIT 30;
", "shipping_high_priority_high_sku_examples")
write_csv_safely(shipping_risky_examples, file.path(output_dir, "shipping_high_priority_high_sku_examples.csv"))

# -------------------------
# 7) Warehouse policy & customer requirement topic signals (SQL)
# -------------------------
warehouse_topics <- sql_query(con, "
WITH base AS (
  SELECT content_safe
  FROM v_documents_base
  WHERE doc_type_norm = 'warehouse_policy'
),
tot AS (SELECT COUNT(*) AS n_total FROM base),
topics AS (
  SELECT 'fifo' AS topic, SUM(CASE WHEN upper(content_safe) LIKE '%FIFO%' THEN 1 ELSE 0 END) AS n FROM base
  UNION ALL SELECT 'fefo', SUM(CASE WHEN upper(content_safe) LIKE '%FEFO%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'hazmat', SUM(CASE WHEN lower(content_safe) LIKE '%hazmat%' OR lower(content_safe) LIKE '%dangerous goods%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'temperature_control', SUM(CASE WHEN lower(content_safe) LIKE '%temperature%' OR lower(content_safe) LIKE '%cold chain%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'cycle_count', SUM(CASE WHEN lower(content_safe) LIKE '%cycle count%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'putaway', SUM(CASE WHEN lower(content_safe) LIKE '%putaway%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'picking', SUM(CASE WHEN lower(content_safe) LIKE '%picking%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'pallet', SUM(CASE WHEN lower(content_safe) LIKE '%pallet%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'cross_dock', SUM(CASE WHEN lower(content_safe) LIKE '%cross-dock%' OR lower(content_safe) LIKE '%cross dock%' THEN 1 ELSE 0 END) FROM base
)
SELECT
  topic,
  n,
  ROUND(100.0 * n / NULLIF((SELECT n_total FROM tot), 0), 2) AS pct_of_warehouse_policies
FROM topics
ORDER BY n DESC, topic ASC;
", "warehouse_policy_topic_signals")
write_csv_safely(warehouse_topics, file.path(output_dir, "warehouse_policy_topic_signals.csv"))

customer_req_topics <- sql_query(con, "
WITH base AS (
  SELECT content_safe
  FROM v_documents_base
  WHERE doc_type_norm = 'customer_requirement'
),
tot AS (SELECT COUNT(*) AS n_total FROM base),
topics AS (
  SELECT 'sla' AS topic, SUM(CASE WHEN lower(content_safe) LIKE '%sla%' OR lower(content_safe) LIKE '%service level%' THEN 1 ELSE 0 END) AS n FROM base
  UNION ALL SELECT 'otif', SUM(CASE WHEN upper(content_safe) LIKE '%OTIF%' OR lower(content_safe) LIKE '%on-time in-full%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'lead_time', SUM(CASE WHEN lower(content_safe) LIKE '%lead time%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'delivery_window', SUM(CASE WHEN lower(content_safe) LIKE '%delivery window%' OR lower(content_safe) LIKE '%time window%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'penalty', SUM(CASE WHEN lower(content_safe) LIKE '%penalt%' OR lower(content_safe) LIKE '%chargeback%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'returns', SUM(CASE WHEN lower(content_safe) LIKE '%return%' OR lower(content_safe) LIKE '%rma%' THEN 1 ELSE 0 END) FROM base
  UNION ALL SELECT 'packaging_labeling', SUM(CASE WHEN lower(content_safe) LIKE '%label%' OR lower(content_safe) LIKE '%packaging%' THEN 1 ELSE 0 END) FROM base
)
SELECT
  topic,
  n,
  ROUND(100.0 * n / NULLIF((SELECT n_total FROM tot), 0), 2) AS pct_of_customer_requirements
FROM topics
ORDER BY n DESC, topic ASC;
", "customer_requirement_topic_signals")
write_csv_safely(customer_req_topics, file.path(output_dir, "customer_requirement_topic_signals.csv"))

# -------------------------
# 8) Performance metrics exploration (SQL + small R stats)
# -------------------------
if (has_table(con, "performance_metrics")) {
  
  # 8.1 Summary by type (no STDDEV(); portable SD instead)
  perf_summary <- sql_query(con, paste0("
  WITH base AS (
    SELECT
      doc_type,
      lower(replace(doc_type,' ','_')) AS doc_type_norm,
      ", doc_type_label_case, " AS doc_type_label,
      query_time,
      precision_score,
      recall_score,
      CASE
        WHEN precision_score IS NULL OR recall_score IS NULL OR (precision_score + recall_score) = 0 THEN NULL
        ELSE 2.0 * precision_score * recall_score / (precision_score + recall_score)
      END AS f1
    FROM performance_metrics
  )
  SELECT
    doc_type_label,
    COUNT(*) AS n_queries,
    ROUND(AVG(query_time), 4) AS mean_query_time_s,
    ROUND(", sd_sql("query_time"), ", 4) AS sd_query_time_s,
    ROUND(AVG(precision_score), 4) AS mean_precision,
    ROUND(AVG(recall_score), 4) AS mean_recall,
    ROUND(AVG(f1), 4) AS mean_f1
  FROM base
  GROUP BY doc_type_norm, doc_type_label
  ORDER BY doc_type_label;
  "), "performance_summary_by_type")
  
  write_csv_safely(perf_summary, file.path(output_dir, "performance_summary_by_type.csv"))
  
  # 8.2 Time bucket distribution (server-side SQL)
  perf_time_buckets <- sql_query(con, paste0("
  WITH base AS (
    SELECT
      lower(replace(doc_type,' ','_')) AS doc_type_norm,
      ", doc_type_label_case, " AS doc_type_label,
      CASE
        WHEN query_time < 0.5 THEN '0-0.5'
        WHEN query_time < 1.0 THEN '0.5-1'
        WHEN query_time < 1.5 THEN '1-1.5'
        WHEN query_time < 2.0 THEN '1.5-2'
        WHEN query_time < 2.5 THEN '2-2.5'
        WHEN query_time < 3.0 THEN '2.5-3'
        ELSE '>3'
      END AS time_bucket
    FROM performance_metrics
    WHERE query_time IS NOT NULL
  ),
  totals AS (
    SELECT doc_type_norm, COUNT(*) AS n_total
    FROM base
    GROUP BY doc_type_norm
  )
  SELECT
    b.doc_type_label,
    b.time_bucket,
    COUNT(*) AS n,
    ROUND(100.0 * COUNT(*) / t.n_total, 2) AS pct_within_type
  FROM base b
  JOIN totals t
    ON b.doc_type_norm = t.doc_type_norm
  GROUP BY b.doc_type_norm, b.doc_type_label, b.time_bucket, t.n_total
  ORDER BY b.doc_type_label,
    CASE b.time_bucket
      WHEN '0-0.5' THEN 1
      WHEN '0.5-1' THEN 2
      WHEN '1-1.5' THEN 3
      WHEN '1.5-2' THEN 4
      WHEN '2-2.5' THEN 5
      WHEN '2.5-3' THEN 6
      ELSE 7
    END;
  "), "performance_time_buckets")
  
  write_csv_safely(perf_time_buckets, file.path(output_dir, "performance_time_buckets.csv"))
  
  # 8.3 Latency percentiles (overall + by type) computed in R for portability
  perf_raw <- sql_query(con, paste0("
  SELECT
    lower(replace(doc_type,' ','_')) AS doc_type_norm,
    ", doc_type_label_case, " AS doc_type_label,
    query_time,
    precision_score,
    recall_score
  FROM performance_metrics
  WHERE query_time IS NOT NULL;
  "), "performance_raw_for_percentiles")
  
  if (nrow(perf_raw) > 0) {
    overall_lat <- perf_raw$query_time
    overall_lat_summary <- cbind(
      data.frame(
        mean = round(mean(overall_lat, na.rm = TRUE), 4),
        min  = round(min(overall_lat, na.rm = TRUE), 4),
        max  = round(max(overall_lat, na.rm = TRUE), 4)
      ),
      round(quantile_summary(overall_lat), 4)
    )
    write_csv_safely(overall_lat_summary, file.path(output_dir, "performance_latency_percentiles_overall.csv"))
    
    by_type_lat <- perf_raw %>%
      group_by(doc_type_label) %>%
      summarise(
        mean = mean(query_time, na.rm = TRUE),
        min  = min(query_time, na.rm = TRUE),
        max  = max(query_time, na.rm = TRUE),
        p50  = percentile(query_time, 0.50),
        p90  = percentile(query_time, 0.90),
        p95  = percentile(query_time, 0.95),
        p99  = percentile(query_time, 0.99),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~round(.x, 4))) %>%
      arrange(doc_type_label)
    
    write_csv_safely(by_type_lat, file.path(output_dir, "performance_latency_percentiles_by_type.csv"))
    
    # 8.4 Correlations (query_time vs precision/recall) overall + by type
    cor_overall <- data.frame(
      scope = "overall",
      cor_time_precision = suppressWarnings(cor(perf_raw$query_time, perf_raw$precision_score, use = "complete.obs")),
      cor_time_recall    = suppressWarnings(cor(perf_raw$query_time, perf_raw$recall_score, use = "complete.obs")),
      cor_precision_recall = suppressWarnings(cor(perf_raw$precision_score, perf_raw$recall_score, use = "complete.obs"))
    )
    
    cor_by_type <- perf_raw %>%
      group_by(doc_type_label) %>%
      summarise(
        cor_time_precision = suppressWarnings(cor(query_time, precision_score, use = "complete.obs")),
        cor_time_recall    = suppressWarnings(cor(query_time, recall_score, use = "complete.obs")),
        cor_precision_recall = suppressWarnings(cor(precision_score, recall_score, use = "complete.obs")),
        .groups = "drop"
      ) %>%
      mutate(scope = doc_type_label) %>%
      select(scope, everything(), -doc_type_label)
    
    cor_all <- bind_rows(cor_overall, cor_by_type) %>%
      mutate(across(where(is.numeric), ~round(.x, 4)))
    
    write_csv_safely(cor_all, file.path(output_dir, "performance_correlations.csv"))
  }
  
  # 8.5 Slowest queries / lowest F1 (if columns exist)
  perf_cols <- table_cols(con, "performance_metrics")
  has_query_id <- "query_id" %in% perf_cols
  has_f1_col <- "f1_score" %in% perf_cols
  
  if (has_query_id) {
    slowest <- sql_query(con, "
    SELECT query_id, doc_type, query_time, precision_score, recall_score
    FROM performance_metrics
    WHERE query_time IS NOT NULL
    ORDER BY query_time DESC
    LIMIT 50;
    ", "slowest_queries")
    write_csv_safely(slowest, file.path(output_dir, "performance_slowest_queries_top50.csv"))
  }
  
  # Compute F1 in SQL to avoid relying on table f1_score column
  lowest_f1 <- sql_query(con, "
  SELECT
    query_id,
    doc_type,
    query_time,
    precision_score,
    recall_score,
    CASE
      WHEN precision_score IS NULL OR recall_score IS NULL OR (precision_score + recall_score) = 0 THEN NULL
      ELSE 2.0 * precision_score * recall_score / (precision_score + recall_score)
    END AS f1
  FROM performance_metrics
  WHERE precision_score IS NOT NULL AND recall_score IS NOT NULL
  ORDER BY f1 ASC
  LIMIT 50;
  ", "lowest_f1_queries_top50")
  if (nrow(lowest_f1) > 0) {
    write_csv_safely(lowest_f1, file.path(output_dir, "performance_lowest_f1_queries_top50.csv"))
  }
  
} else {
  warning("Table 'performance_metrics' not found; skipping performance exploration.", call. = FALSE)
}

# -------------------------
# 9) Optional: Materialize report tables in SQLite (pure SQL snapshots)
# -------------------------
if (isTRUE(materialize_reports)) {
  # Helper to create rpt_* table from a SELECT
  create_rpt <- function(tbl_name, select_sql) {
    sql_exec(con, sprintf("DROP TABLE IF EXISTS %s;", tbl_name), paste0("drop_", tbl_name))
    sql_exec(con, sprintf("CREATE TABLE %s AS %s;", tbl_name, select_sql), paste0("create_", tbl_name))
  }
  
  create_rpt("rpt_doc_counts_by_type", "
    SELECT doc_type_label, COUNT(*) AS n_docs
    FROM v_documents_base
    GROUP BY doc_type_norm, doc_type_label
    ORDER BY n_docs DESC
  ")
  
  create_rpt("rpt_doc_length_stats_by_type", paste0("
    SELECT doc_type_label,
           COUNT(*) AS n_docs,
           ROUND(AVG(content_length), 1) AS mean_chars,
           ROUND(", sd_sql("content_length"), ", 1) AS sd_chars,
           MIN(content_length) AS min_chars,
           MAX(content_length) AS max_chars
    FROM v_documents_base
    GROUP BY doc_type_norm, doc_type_label
    ORDER BY doc_type_label
  "))
  
  create_rpt("rpt_shipping_top_lanes", "
    SELECT origin, destination, COUNT(*) AS n_shipments
    FROM v_shipping_orders_enriched
    WHERE origin IS NOT NULL AND destination IS NOT NULL
    GROUP BY origin, destination
    ORDER BY n_shipments DESC
    LIMIT 25
  ")
}

# -------------------------
# 10) Human-readable recap (Markdown)
# -------------------------
recap_lines <- c(
  "# SQL-Focused Exploration Recap (v2)",
  "",
  paste0("- Database: `", db_path, "`"),
  paste0("- Output folder: `", normalizePath(output_dir, winslash = "/"), "`"),
  paste0("- JSON1 available: **", if (json1_ok) "YES" else "NO", "**"),
  paste0("- Metadata extraction view: **", if (meta_view_created) "CREATED" else "SKIPPED", "**"),
  "",
  "## Key outputs",
  "- `doc_counts_by_type.csv`, `doc_length_stats_by_type.csv`, `doc_wordcount_stats_by_type.csv`",
  "- `shipping_top_lanes.csv`, `shipping_incoterms_distribution.csv`, `shipping_priority_distribution.csv`, `shipping_mode_distribution.csv`",
  "- `warehouse_policy_topic_signals.csv`, `customer_requirement_topic_signals.csv`",
  "- If available: `performance_summary_by_type.csv`, `performance_time_buckets.csv`, latency percentiles + correlations"
)

write_lines_safely(recap_lines, file.path(output_dir, "appendix_recap.md"))

message("\nDone. All outputs saved under: ", normalizePath(output_dir, winslash = "/"))


# ============================================================
# Enhanced SQL Exploration Data Viewer (English)
# Advanced data exploration with improved error handling, 
# statistics computation, and interactive features
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(knitr)
  library(DT)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(stringr)
  library(lubridate)
  library(scales)
  library(corrplot)
  library(VIM) # for missing value visualization
})

# Configuration
config <- list(
  output_dir = "sql_only_exploration_output",
  max_display_rows = 100,
  max_unique_values_display = 20,
  numeric_threshold = 0.8, # proportion of numeric values to consider column as numeric
  date_patterns = c("%Y-%m-%d", "%Y-%m-%d %H:%M:%S", "%d/%m/%Y", "%m/%d/%Y")
)

# ============================================================
# UTILITY FUNCTIONS
# ============================================================

# Smart data type detection and conversion
smart_type_conversion <- function(df) {
  tryCatch({
    for (col in names(df)) {
      if (is.character(df[[col]])) {
        # Remove leading/trailing whitespace
        df[[col]] <- stringr::str_trim(df[[col]])
        
        # Try to convert to numeric if mostly numeric
        numeric_vals <- suppressWarnings(as.numeric(df[[col]]))
        if (sum(!is.na(numeric_vals)) / length(df[[col]]) >= config$numeric_threshold) {
          df[[col]] <- numeric_vals
          next
        }
        
        # Try to convert to date
        for (pattern in config$date_patterns) {
          date_vals <- suppressWarnings(as.Date(df[[col]], format = pattern))
          if (sum(!is.na(date_vals)) / length(df[[col]]) >= 0.5) {
            df[[col]] <- date_vals
            break
          }
        }
        
        # Try to convert to logical
        if (all(tolower(df[[col]]) %in% c("true", "false", "t", "f", "yes", "no", "y", "n", NA))) {
          df[[col]] <- as.logical(df[[col]])
        }
      }
    }
    return(df)
  }, error = function(e) {
    warning("Type conversion failed: ", e$message)
    return(df)
  })
}

# Enhanced column analysis
analyze_column <- function(x, col_name) {
  analysis <- list(
    name = col_name,
    type = class(x)[1],
    length = length(x),
    na_count = sum(is.na(x)),
    na_percentage = round(sum(is.na(x)) / length(x) * 100, 2),
    unique_count = length(unique(na.omit(x))),
    is_key = length(unique(na.omit(x))) == length(na.omit(x)) && length(na.omit(x)) > 1
  )
  
  non_na_vals <- na.omit(x)
  
  if (length(non_na_vals) == 0) {
    analysis$sample_values <- "All NA"
    return(analysis)
  }
  
  if (is.numeric(x)) {
    analysis <- c(analysis, list(
      min_val = min(non_na_vals),
      max_val = max(non_na_vals),
      mean_val = round(mean(non_na_vals), 3),
      median_val = median(non_na_vals),
      std_dev = round(sd(non_na_vals), 3),
      zeros_count = sum(non_na_vals == 0),
      outliers_count = length(boxplot.stats(non_na_vals)$out)
    ))
  } else if (is.character(x) || is.factor(x)) {
    val_lengths <- nchar(as.character(non_na_vals))
    analysis <- c(analysis, list(
      min_length = min(val_lengths),
      max_length = max(val_lengths),
      avg_length = round(mean(val_lengths), 2),
      empty_strings = sum(non_na_vals == "")
    ))
  }
  
  # Sample values
  if (analysis$unique_count <= config$max_unique_values_display) {
    analysis$sample_values <- paste(unique(head(non_na_vals, config$max_unique_values_display)), collapse = ", ")
  } else {
    analysis$sample_values <- paste(paste(head(unique(non_na_vals), 3), collapse = ", "), "... (", analysis$unique_count, " unique values)")
  }
  
  return(analysis)
}

# Data quality assessment
assess_data_quality <- function(df) {
  quality_issues <- list()
  
  # Check for completely empty columns
  empty_cols <- names(df)[sapply(df, function(x) all(is.na(x)))]
  if (length(empty_cols) > 0) {
    quality_issues$empty_columns <- empty_cols
  }
  
  # Check for high missing data columns
  high_na_cols <- names(df)[sapply(df, function(x) sum(is.na(x)) / length(x) > 0.8)]
  if (length(high_na_cols) > 0) {
    quality_issues$high_missing_columns <- high_na_cols
  }
  
  # Check for potential duplicates
  if (nrow(df) > 0 && anyDuplicated(df) > 0) {
    quality_issues$duplicate_rows <- sum(duplicated(df))
  }
  
  # Check for constant columns
  constant_cols <- names(df)[sapply(df, function(x) length(unique(na.omit(x))) <= 1)]
  if (length(constant_cols) > 0) {
    quality_issues$constant_columns <- constant_cols
  }
  
  return(quality_issues)
}

# Enhanced statistics computation
compute_enhanced_statistics <- function(df) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  if (length(numeric_cols) == 0) {
    return(list(message = "No numeric columns found"))
  }
  
  tryCatch({
    stats_df <- df %>%
      select(all_of(numeric_cols)) %>%
      summarise(across(everything(), list(
        count = ~sum(!is.na(.)),
        mean = ~round(mean(., na.rm = TRUE), 3),
        median = ~round(median(., na.rm = TRUE), 3),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        std_dev = ~round(sd(., na.rm = TRUE), 3),
        q25 = ~quantile(., 0.25, na.rm = TRUE),
        q75 = ~quantile(., 0.75, na.rm = TRUE),
        zeros = ~sum(. == 0, na.rm = TRUE),
        negatives = ~sum(. < 0, na.rm = TRUE)
      ), .names = "{.col}__{.fn}")) %>%
      pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
      separate(stat, into = c("column", "statistic"), sep = "__") %>%
      pivot_wider(names_from = statistic, values_from = value) %>%
      arrange(column)
    
    return(stats_df)
  }, error = function(e) {
    # Fallback to basic statistics
    basic_stats <- data.frame(
      column = numeric_cols,
      count = sapply(df[numeric_cols], function(x) sum(!is.na(x))),
      mean = sapply(df[numeric_cols], function(x) round(mean(x, na.rm = TRUE), 3)),
      median = sapply(df[numeric_cols], function(x) round(median(x, na.rm = TRUE), 3)),
      min = sapply(df[numeric_cols], function(x) min(x, na.rm = TRUE)),
      max = sapply(df[numeric_cols], function(x) max(x, na.rm = TRUE)),
      std_dev = sapply(df[numeric_cols], function(x) round(sd(x, na.rm = TRUE), 3))
    )
    return(basic_stats)
  })
}

# ============================================================
# MAIN DISPLAY FUNCTIONS
# ============================================================

# Enhanced dataset display function
display_enhanced_dataset <- function(file_path, max_rows = config$max_display_rows) {
  tryCatch({
    # Read and convert data
    df_raw <- readr::read_csv(file_path, show_col_types = FALSE)
    df <- smart_type_conversion(df_raw)
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Header
    cat("\n", strrep("=", 100), "\n", sep = "")
    cat("📊 ENHANCED DATASET ANALYSIS: ", toupper(file_name), "\n")
    cat(strrep("=", 100), "\n")
    
    # Basic info
    cat("📁 File:", file_path, "\n")
    cat("📐 Dimensions:", nrow(df), "rows ×", ncol(df), "columns\n")
    cat("📅 Generated:", format(file.info(file_path)$mtime, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("💾 File Size:", format(file.info(file_path)$size, units = "auto"), "\n")
    
    # Handle empty datasets
    if (nrow(df) == 0) {
      cat("\n⚠️  EMPTY DATASET - No rows to analyze\n")
      cat("Column structure:\n")
      print(data.frame(Column = names(df), Type = sapply(df, class)))
      return(invisible(NULL))
    }
    
    cat(strrep("-", 100), "\n\n")
    
    # Data quality assessment
    quality_issues <- assess_data_quality(df)
    if (length(quality_issues) > 0) {
      cat("⚠️  DATA QUALITY ISSUES:\n")
      for (issue_type in names(quality_issues)) {
        cat("  •", str_replace_all(issue_type, "_", " "), ":", 
            paste(quality_issues[[issue_type]], collapse = ", "), "\n")
      }
      cat("\n")
    } else {
      cat("✅ DATA QUALITY: No major issues detected\n\n")
    }
    
    # Enhanced column analysis
    cat("📋 COLUMN ANALYSIS:\n")
    col_analyses <- lapply(names(df), function(col) analyze_column(df[[col]], col))
    col_summary <- do.call(rbind, lapply(col_analyses, function(x) {
      data.frame(
        Column = x$name,
        Type = x$type,
        Non_NA = x$length - x$na_count,
        NA_Pct = paste0(x$na_percentage, "%"),
        Unique = x$unique_count,
        Is_Key = ifelse(x$is_key, "✓", ""),
        Sample_Values = substr(x$sample_values, 1, 50),
        stringsAsFactors = FALSE
      )
    }))
    
    print(col_summary, row.names = FALSE)
    cat("\n")
    
    # Enhanced statistics
    cat("📊 STATISTICAL SUMMARY:\n")
    enhanced_stats <- compute_enhanced_statistics(df)
    if (is.data.frame(enhanced_stats)) {
      print(enhanced_stats, row.names = FALSE)
    } else {
      cat(enhanced_stats$message, "\n")
    }
    cat("\n")
    
    # Data preview
    cat("👀 DATA PREVIEW (first ", min(max_rows, nrow(df)), " rows):\n")
    print(head(df, min(max_rows, nrow(df))))
    
    if (nrow(df) > max_rows) {
      cat("\n📝 Note: Showing first", max_rows, "of", nrow(df), "rows\n")
      cat("💡 Use read_full_dataset('", file_name, "') to load complete data\n")
    }
    
    # Missing values pattern (if any missing data)
    total_na <- sum(is.na(df))
    if (total_na > 0) {
      cat("\n🔍 MISSING DATA PATTERN:\n")
      na_summary <- df %>%
        summarise(across(everything(), ~sum(is.na(.)))) %>%
        pivot_longer(everything(), names_to = "Column", values_to = "NA_Count") %>%
        filter(NA_Count > 0) %>%
        arrange(desc(NA_Count))
      
      print(na_summary, row.names = FALSE)
    }
    
    invisible(df)
    
  }, error = function(e) {
    cat("❌ ERROR reading file:", file_path, "\n")
    cat("Error details:", e$message, "\n\n")
    return(NULL)
  })
}

# Create interactive visualization
create_data_visualization <- function(df, dataset_name) {
  if (nrow(df) == 0) return(NULL)
  
  tryCatch({
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    if (length(numeric_cols) >= 2) {
      # Correlation heatmap for numeric columns
      cor_matrix <- cor(df[numeric_cols], use = "complete.obs")
      
      # Create correlation plot
      corrplot(cor_matrix, method = "color", type = "upper", 
               order = "hclust", tl.cex = 0.8, tl.col = "black")
      title(paste("Correlation Matrix -", dataset_name))
    }
    
    # Distribution plots for key numeric columns
    for (col in head(numeric_cols, 4)) {
      if (length(unique(na.omit(df[[col]]))) > 1) {
        p <- ggplot(df, aes_string(x = col)) +
          geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
          labs(title = paste("Distribution of", col), 
               subtitle = dataset_name) +
          theme_minimal()
        print(p)
      }
    }
    
  }, error = function(e) {
    message("Visualization creation failed: ", e$message)
  })
}

# Enhanced interactive table
create_enhanced_interactive_table <- function(file_path, max_rows = 1000) {
  tryCatch({
    df <- readr::read_csv(file_path, show_col_types = FALSE)
    df <- smart_type_conversion(df)
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data to display"), 
        caption = paste("Dataset:", file_name, "(Empty)")
      ))
    }
    
    display_df <- head(df, max_rows)
    
    # Format numeric columns
    numeric_cols <- which(sapply(display_df, is.numeric))
    
    DT::datatable(
      display_df,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-size: 16px; font-weight: bold;",
        paste("📊 Dataset:", file_name, "(", nrow(df), "rows ×", ncol(df), "cols)")
      ),
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50, 100),
        scrollX = TRUE,
        scrollY = "500px",
        fixedHeader = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', text = 'Copy'),
          list(extend = 'csv', filename = file_name),
          list(extend = 'excel', filename = file_name),
          'colvis'
        ),
        columnDefs = list(
          list(className = 'dt-center', targets = numeric_cols - 1)
        )
      ),
      filter = 'top',
      class = 'display compact hover',
      extensions = c('Buttons', 'FixedHeader', 'ColReorder'),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = numeric_cols, digits = 3)
    
  }, error = function(e) {
    return(DT::datatable(
      data.frame(Error = paste("Failed to create table:", e$message)),
      caption = "Error"
    ))
  })
}

# Advanced search functionality
advanced_search <- function(search_terms, search_type = "any", case_sensitive = FALSE) {
  cat("🔍 ADVANCED SEARCH RESULTS\n")
  cat("Search terms:", paste(search_terms, collapse = ", "), "\n")
  cat("Search type:", search_type, "| Case sensitive:", case_sensitive, "\n")
  cat(strrep("=", 80), "\n\n")
  
  results <- list()
  csv_files <- list.files(config$output_dir, pattern = "\\.csv$", full.names = TRUE)
  
  for (file in csv_files) {
    tryCatch({
      df <- readr::read_csv(file, show_col_types = FALSE)
      file_name <- tools::file_path_sans_ext(basename(file))
      
      matches <- list()
      
      # Search in column names
      for (term in search_terms) {
        col_matches <- grep(term, names(df), ignore.case = !case_sensitive, value = TRUE)
        if (length(col_matches) > 0) {
          matches$columns <- c(matches$columns, col_matches)
        }
      }
      
      # Search in data
      for (col in names(df)) {
        for (term in search_terms) {
          if (is.character(df[[col]]) || is.factor(df[[col]])) {
            row_matches <- which(grepl(term, as.character(df[[col]]), ignore.case = !case_sensitive))
            if (length(row_matches) > 0) {
              matches$data[[col]] <- c(matches$data[[col]], row_matches)
            }
          }
        }
      }
      
      if (length(matches) > 0) {
        results[[file_name]] <- matches
        cat("📄 Dataset:", file_name, "\n")
        if (!is.null(matches$columns)) {
          cat("  📋 Column matches:", paste(unique(matches$columns), collapse = ", "), "\n")
        }
        if (!is.null(matches$data)) {
          cat("  📝 Data matches in:", paste(names(matches$data), collapse = ", "), "\n")
        }
        cat("\n")
      }
      
    }, error = function(e) {
      next
    })
  }
  
  if (length(results) == 0) {
    cat("❌ No matches found\n")
  }
  
  invisible(results)
}

# Dataset comparison function
compare_datasets <- function(dataset1, dataset2) {
  cat("🔄 DATASET COMPARISON\n")
  cat(strrep("=", 80), "\n")
  
  file1 <- file.path(config$output_dir, paste0(dataset1, ".csv"))
  file2 <- file.path(config$output_dir, paste0(dataset2, ".csv"))
  
  if (!file.exists(file1) || !file.exists(file2)) {
    cat("❌ One or both datasets not found\n")
    return(invisible(NULL))
  }
  
  df1 <- smart_type_conversion(readr::read_csv(file1, show_col_types = FALSE))
  df2 <- smart_type_conversion(readr::read_csv(file2, show_col_types = FALSE))
  
  comparison <- data.frame(
    Metric = c("Rows", "Columns", "Numeric Columns", "Character Columns", "Date Columns", "Total NA Values"),
    Dataset1 = c(
      nrow(df1), ncol(df1),
      sum(sapply(df1, is.numeric)),
      sum(sapply(df1, is.character)),
      sum(sapply(df1, function(x) inherits(x, "Date"))),
      sum(is.na(df1))
    ),
    Dataset2 = c(
      nrow(df2), ncol(df2),
      sum(sapply(df2, is.numeric)),
      sum(sapply(df2, is.character)),
      sum(sapply(df2, function(x) inherits(x, "Date"))),
      sum(is.na(df2))
    )
  )
  
  comparison$Difference <- comparison$Dataset2 - comparison$Dataset1
  
  names(comparison)[2:3] <- c(dataset1, dataset2)
  
  print(comparison, row.names = FALSE)
  
  # Common columns
  common_cols <- intersect(names(df1), names(df2))
  unique_to_1 <- setdiff(names(df1), names(df2))
  unique_to_2 <- setdiff(names(df2), names(df1))
  
  cat("\n📋 COLUMN COMPARISON:\n")
  cat("Common columns (", length(common_cols), "):", paste(head(common_cols, 10), collapse = ", "))
  if (length(common_cols) > 10) cat(" ...")
  cat("\nUnique to", dataset1, "(", length(unique_to_1), "):", paste(head(unique_to_1, 5), collapse = ", "))
  if (length(unique_to_1) > 5) cat(" ...")
  cat("\nUnique to", dataset2, "(", length(unique_to_2), "):", paste(head(unique_to_2, 5), collapse = ", "))
  if (length(unique_to_2) > 5) cat(" ...")
  cat("\n")
}

# ============================================================
# HELPER FUNCTIONS FOR USER INTERACTION
# ============================================================

# Load specific dataset
load_dataset <- function(dataset_name) {
  file_path <- file.path(config$output_dir, paste0(dataset_name, ".csv"))
  if (!file.exists(file_path)) {
    stop("Dataset '", dataset_name, "' not found in ", config$output_dir)
  }
  smart_type_conversion(readr::read_csv(file_path, show_col_types = FALSE))
}

# List available datasets
list_available_datasets <- function() {
  csv_files <- list.files(config$output_dir, pattern = "\\.csv$")
  dataset_names <- tools::file_path_sans_ext(csv_files)
  
  cat("📁 AVAILABLE DATASETS (", length(dataset_names), "):\n")
  cat(strrep("=", 50), "\n")
  
  for (i in seq_along(dataset_names)) {
    file_path <- file.path(config$output_dir, csv_files[i])
    file_info <- file.info(file_path)
    df_preview <- readr::read_csv(file_path, show_col_types = FALSE, n_max = 1)
    
    cat(sprintf("%2d. %-35s (%dx%d) %s\n", 
                i, dataset_names[i], 
                nrow(readr::read_csv(file_path, show_col_types = FALSE)),
                ncol(df_preview),
                format(file_info$size, units = "auto")))
  }
  
  invisible(dataset_names)
}

# Export processed dataset
export_dataset <- function(dataset_name, output_format = "csv", output_path = NULL) {
  df <- load_dataset(dataset_name)
  
  if (is.null(output_path)) {
    output_path <- paste0("processed_", dataset_name, ".", output_format)
  }
  
  switch(output_format,
         "csv" = readr::write_csv(df, output_path),
         "xlsx" = openxlsx::write.xlsx(df, output_path),
         "rds" = saveRDS(df, output_path),
         stop("Unsupported format. Use: csv, xlsx, rds"))
  
  cat("✅ Dataset exported to:", output_path, "\n")
}

# ============================================================
# MAIN EXECUTION
# ============================================================

# Check if output directory exists
if (!dir.exists(config$output_dir)) {
  stop("❌ Output directory not found: ", config$output_dir, 
       "\nPlease run the main analysis script first.")
}

# Get list of CSV files
csv_files <- list.files(config$output_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("❌ No CSV files found in output directory")
}

cat("🚀 ENHANCED SQL EXPLORATION DATA VIEWER\n")
cat(strrep("=", 80), "\n")
cat("📁 Output directory:", normalizePath(config$output_dir), "\n")
cat("📊 Found", length(csv_files), "datasets\n\n")


# Display available datasets
dataset_names <- list_available_datasets()

# Quick demo with key datasets
cat("\n\n🎯 QUICK ANALYSIS - Key Logistics Datasets:\n")
cat(strrep("=", 80), "\n")

key_datasets <- c("doc_counts_by_type", "shipping_top_lanes", 
                  "performance_summary_by_type", "warehouse_policy_topic_signals")

for (ds in key_datasets) {
  file_path <- file.path(config$output_dir, paste0(ds, ".csv"))
  if (file.exists(file_path)) {
    display_enhanced_dataset(file_path, max_rows = 10)
  }
}

cat("\n\n💡 USAGE EXAMPLES:\n")
cat(strrep("-", 50), "\n")
cat("• display_enhanced_dataset('path/to/dataset.csv')  # Enhanced analysis\n")
cat("• load_dataset('dataset_name')                     # Load for analysis\n")
cat("• advanced_search(c('shipping', 'priority'))       # Advanced search\n")
cat("• compare_datasets('dataset1', 'dataset2')         # Compare datasets\n")



cat("• create_enhanced_interactive_table('path')        # Interactive table\n")
cat("• export_dataset('dataset_name', 'xlsx')           # Export processed data\n")

cat("\n✅ Enhanced exploration system ready!\n")
