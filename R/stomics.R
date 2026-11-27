#' Create a Stomics API client configuration
#'
#' @param api_key API key for authenticated access. Falls back to the
#'   `STOMICS_API_KEY` environment variable. Omit for public (unauthenticated)
#'   access.
#' @param base_url Base URL of the Stomics API. Defaults to
#'   `"https://api.stomics.io"`.
#'
#' @return A list with class `"stomics_client"` containing `api_key` and
#'   `base_url`.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-your-key")
#' }
stomics_client <- function(api_key = NULL, base_url = "https://api.stomics.io") {
  key <- api_key %||% Sys.getenv("STOMICS_API_KEY", unset = NA_character_)
  if (is.na(key) || !nzchar(key)) key <- NULL
  structure(
    list(api_key = key, base_url = sub("/+$", "", base_url)),
    class = "stomics_client"
  )
}

# Internal: build an httr2 request with auth header if available
.stomics_req <- function(client, path) {
  req <- httr2::request(paste0(client$base_url, path))
  if (!is.null(client$api_key)) {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", client$api_key))
  }
  req
}

# Internal: perform a request and return parsed JSON body
.stomics_perform <- function(req) {
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

# Internal: NULL-coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y


#' Search published datasets
#'
#' @param client A `stomics_client` object created with [stomics_client()].
#' @param query Free-text search term matched against title and abstract.
#' @param organism Organism IDs (optional): an integer vector, or a
#'   comma-separated string such as `"1,2"`.
#' @param tissue Tissue IDs (optional), in the same forms.
#' @param disease Disease IDs (optional), in the same forms.
#' @param technology Technology IDs (optional), in the same forms.
#' @param page Page number (1-based). Default `1`.
#' @param per_page Results per page (max 100). Default `20`.
#'
#' @return A list with elements `items` (list of dataset records), `total`,
#'   `page`, and `per_page`.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' results <- stomics_search(client, query = "brain")
#' cat(results$total, "datasets found\n")
#' }
stomics_search <- function(
    client,
    query = NULL,
    organism = NULL,
    tissue = NULL,
    disease = NULL,
    technology = NULL,
    page = 1L,
    per_page = 20L) {
  req <- .stomics_req(client, "/api/v1/datasets")
  params <- list(page = page, per_page = per_page)
  if (!is.null(query)) params[["q"]] <- query
  if (!is.null(organism)) params[["organism_ids"]] <- .stomics_ids(organism)
  if (!is.null(tissue)) params[["tissue_ids"]] <- .stomics_ids(tissue)
  if (!is.null(disease)) params[["disease_ids"]] <- .stomics_ids(disease)
  if (!is.null(technology)) params[["technology_ids"]] <- .stomics_ids(technology)
  # The API takes each ID as a repeated parameter (`organism_ids=1&organism_ids=2`).
  # A single comma-joined value is rejected with 422, which is what this sent before.
  req <- do.call(httr2::req_url_query, c(list(req, .multi = "explode"), params))
  .stomics_perform(req)
}


# Internal: normalise an ID filter to an integer vector. Accepts integers or a
# comma-separated string, the form the documentation used to ask for.
.stomics_ids <- function(x) {
  if (is.character(x)) x <- unlist(strsplit(x, ",", fixed = TRUE))
  as.integer(trimws(x))
}


#' Get dataset metadata
#'
#' @param client A `stomics_client` object.
#' @param accession Dataset accession number, e.g. `"STD0001"`.
#'
#' @return A list of dataset metadata fields.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' ds <- stomics_get_dataset(client, "STD0001")
#' cat(ds$title, "\n")
#' }
stomics_get_dataset <- function(client, accession) {
  req <- .stomics_req(client, paste0("/api/v1/datasets/", accession))
  .stomics_perform(req)
}


#' List files for a dataset
#'
#' Without filters this returns every file. With any of `q`, `file_types` or
#' `sample_id` it walks the API's paged file listing and returns the files
#' matching all of them.
#'
#' @param client A `stomics_client` object.
#' @param accession Dataset accession number.
#' @param q Only files whose name contains this, ignoring case, e.g. a GEO
#'   sample accession such as `"GSM5"` (optional).
#' @param file_types Only these types, e.g. `c("expression_matrix",
#'   "tissue_image")` (optional).
#' @param sample_id Only the files of this sample (optional).
#' @param version Files from a specific version snapshot (optional).
#'
#' @return A list of file records. Each record includes a `download_url` field
#'   containing a pre-signed S3 URL valid for 1 hour.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' files <- stomics_list_files(client, "STD0001")
#' matrices <- stomics_list_files(client, "STD0716", q = "GSM5",
#'                                file_types = "expression_matrix")
#' }
stomics_list_files <- function(client, accession, q = NULL, file_types = NULL,
                               sample_id = NULL, version = NULL) {
  if (is.null(q) && is.null(file_types) && is.null(sample_id)) {
    req <- .stomics_req(client, paste0("/api/v1/datasets/", accession, "/files"))
    if (!is.null(version)) req <- httr2::req_url_query(req, version = version)
    return(.stomics_perform(req))
  }
  .stomics_file_pages(client, accession, q, file_types, sample_id, version)
}

# Internal: every file matching the filters, from the paged listing
.stomics_file_pages <- function(client, accession, q = NULL, file_types = NULL,
                                sample_id = NULL, version = NULL) {
  files <- list()
  page <- 1L
  repeat {
    req <- .stomics_req(client, paste0("/api/v1/datasets/", accession, "/files/page"))
    req <- httr2::req_url_query(req, page = page, per_page = 200L, q = q,
                                sample_id = sample_id, version = version)
    if (!is.null(file_types)) {
      req <- httr2::req_url_query(req, .multi = "explode", file_type = as.character(file_types))
    }
    data <- .stomics_perform(req)
    files <- c(files, data[["items"]])
    if (page * data[["per_page"]] >= data[["total"]]) break
    page <- page + 1L
  }
  files
}


#' Download every file of a dataset into a directory
#'
#' Downloads each file, or each matching the filters, one at a time. This is
#' the route for datasets too large for a ZIP bundle (over 100 GB). A file
#' already on disk at its full size is skipped, so an interrupted run can be
#' started again; each file gets a fresh download link just before it starts.
#' A file interrupted part-way is downloaded again from the start.
#'
#' @inheritParams stomics_list_files
#' @param dest_dir Directory to write into; created if missing.
#'
#' @return The paths of every file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' stomics_download_dataset(client, "STD0716", "STD0716",
#'                          file_types = "expression_matrix")
#' }
stomics_download_dataset <- function(client, accession, dest_dir, file_types = NULL,
                                     q = NULL, sample_id = NULL) {
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  files <- .stomics_file_pages(client, accession, q, file_types, sample_id)
  seen <- character()
  paths <- character()
  for (f in files) {
    name <- f[["filename"]]
    # Two samples can carry files of the same name; keep both.
    if (name %in% seen) name <- paste0(f[["id"]], "_", name)
    seen <- c(seen, name)
    path <- file.path(dest_dir, name)
    paths <- c(paths, path)
    size <- f[["size_bytes"]]
    if (!is.null(size) && file.exists(path) && file.size(path) == size) next
    part <- paste0(path, ".part")
    stomics_download_file(client, accession, f[["id"]], part)
    file.rename(part, path)
  }
  invisible(paths)
}


#' Download a file to disk
#'
#' Fetches the pre-signed download URL for the given file and streams it to
#' `dest_path`.
#'
#' @param client A `stomics_client` object.
#' @param accession Dataset accession number.
#' @param file_id File ID as returned by [stomics_list_files()].
#' @param dest_path Local path to write the downloaded file to.
#'
#' @return `dest_path` invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-key")
#' stomics_download_file(client, "STD0001", 42L, "matrix.h5ad")
#' }
stomics_download_file <- function(client, accession, file_id, dest_path) {
  path <- paste0("/api/v1/datasets/", accession, "/files/", file_id, "/download")
  req <- .stomics_req(client, path)
  data <- .stomics_perform(req)
  url <- data[["url"]]
  dest_dir <- dirname(dest_path)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
  invisible(dest_path)
}


#' Get per-spot expression values for a gene
#'
#' @param client A `stomics_client` object.
#' @param accession Dataset accession number.
#' @param gene Gene symbol, e.g. `"BRCA1"`.
#' @param sample_id Sample ID to restrict results to (optional).
#' @param layer `"normalised"` (the default) or `"counts"`: raw integer counts,
#'   for datasets whose [stomics_expression_manifest()] lists that layer.
#'   Other datasets answer 409.
#'
#' @return A `data.frame` with columns `x`, `y`, and `expression` (one row per
#'   spot).
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' df <- stomics_get_expression(client, "STD0001", gene = "BRCA1")
#' counts <- stomics_get_expression(client, "STD0001", gene = "BRCA1", layer = "counts")
#' head(df)
#' }
stomics_get_expression <- function(client, accession, gene, sample_id = NULL,
                                   layer = "normalised") {
  req <- .stomics_req(client, paste0("/api/v1/datasets/", accession, "/expression"))
  params <- list(gene = gene)
  if (!is.null(sample_id)) params[["sample_id"]] <- sample_id
  if (!identical(layer, "normalised")) params[["layer"]] <- layer
  req <- do.call(httr2::req_url_query, c(list(req), params))
  data <- .stomics_perform(req)
  spots <- data[["spots"]]
  if (length(spots) == 0L) {
    return(data.frame(x = numeric(0), y = numeric(0), expression = numeric(0)))
  }
  do.call(rbind, lapply(spots, function(s) {
    # The API names the value `value`; `expression` is this function's column.
    data.frame(x = s[["x"]], y = s[["y"]], expression = s[["value"]] %||% s[["expression"]])
  }))
}


#' What a dataset's processed expression holds
#'
#' @param client A `stomics_client` object.
#' @param accession Dataset accession number.
#' @param sample_id Sample ID (optional).
#'
#' @return A list: `layers` (`"normalised"`, plus `"counts"` when the submitted
#'   matrix was raw counts), `transform`, `n_spots`, `n_genes`, `obs` (one
#'   entry per per-spot metadata field, each with `key`, `name`, `kind`
#'   categorical or numeric, `source` submitted or qc), `label_columns` and
#'   `embeddings`. Pass any `obs` key to [stomics_get_obs()].
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' manifest <- stomics_expression_manifest(client, "STD0001")
#' vapply(manifest$obs, function(f) f$key, character(1))
#' }
stomics_expression_manifest <- function(client, accession, sample_id = NULL) {
  req <- .stomics_req(client, paste0("/api/v1/datasets/", accession, "/expression/manifest"))
  if (!is.null(sample_id)) req <- httr2::req_url_query(req, sample_id = sample_id)
  .stomics_perform(req)
}


#' The standard analysis pipeline's state for a sample
#'
#' Every sample with expression data is analysed by the stomics.io standard
#' pipeline: QC flags, Leiden clusters at four resolutions, a UMAP, marker
#' genes and Moran's I for every gene.
#'
#' @param client A `stomics_client` object.
#' @param accession Dataset accession number.
#' @param sample_id Sample ID (optional).
#'
#' @return A list: `status` (`"ready"`, `"not_run"`, `"pending"`, `"running"`,
#'   `"failed"` or `"unsupported"`, with `error`) and, when ready,
#'   `provenance`, `resolutions` and `n_clusters`. The clusters are also obs
#'   fields (`"standard:leiden_1.0"`, ...) for [stomics_get_obs()].
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' run <- stomics_get_standard_analysis(client, "STD0001", sample_id = 12)
#' run$status
#' }
stomics_get_standard_analysis <- function(client, accession, sample_id = NULL) {
  .stomics_analysis(client, accession, "", list(sample_id = sample_id))
}


#' Marker genes of the standard pipeline's clusters
#'
#' Wilcoxon rank-sum, each cluster against all other spots.
#'
#' @inheritParams stomics_get_standard_analysis
#' @param resolution Leiden resolution: 0.3, 0.6, 1.0 or 1.5.
#' @param cluster One cluster's markers only (optional).
#' @param limit Genes per cluster, at most 100.
#'
#' @return A `data.frame` with `cluster`, `rank`, `gene`, `score`,
#'   `log_fold_change`, `pval_adj`, `pct_in` and `pct_out`.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' markers <- stomics_get_markers(client, "STD0001", sample_id = 12, resolution = 0.6)
#' }
stomics_get_markers <- function(client, accession, sample_id = NULL, resolution = 1.0,
                                cluster = NULL, limit = 10) {
  data <- .stomics_analysis(
    client, accession, "/markers",
    list(sample_id = sample_id, resolution = resolution, cluster = cluster, limit = limit)
  )
  .stomics_rows(data[["markers"]], c(
    "cluster", "rank", "gene", "score", "log_fold_change", "pval_adj", "pct_in", "pct_out"
  ))
}


#' The top marker genes of every cluster as a dot plot
#'
#' @inheritParams stomics_get_markers
#'
#' @return A list of two genes x clusters matrices: `mean` (normalised
#'   expression) and `fraction` (share of spots expressing).
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' dp <- stomics_get_dotplot(client, "STD0001", sample_id = 12)
#' dp$mean["Gfap", ]
#' }
stomics_get_dotplot <- function(client, accession, sample_id = NULL, resolution = 1.0) {
  data <- .stomics_analysis(
    client, accession, "/dotplot", list(sample_id = sample_id, resolution = resolution)
  )
  genes <- as.character(unlist(data[["genes"]]))
  clusters <- as.character(unlist(data[["clusters"]]))
  as_matrix <- function(rows) {
    m <- matrix(as.numeric(unlist(rows)), nrow = length(genes), ncol = length(clusters), byrow = TRUE)
    dimnames(m) <- list(genes, clusters)
    m
  }
  list(mean = as_matrix(data[["mean"]]), fraction = as_matrix(data[["fraction"]]))
}


#' Spatially variable genes by Moran's I and SPARK-X, on every spot
#'
#' The standard pipeline computes Moran's I and SPARK-X for every gene over
#' every spot. [stomics_spatially_variable_genes()] returns the top genes by
#' Moran's I alone.
#'
#' @inheritParams stomics_get_standard_analysis
#' @param rank_by `"consensus"` (the mean of the two ranks), `"moran"` or
#'   `"sparkx"`.
#' @param limit,offset A page of genes, best first.
#'
#' @return A `data.frame` with `gene`, `rank` (consensus), `moran_i`,
#'   `moran_rank`, `sparkx_rank` and `pval_adj` (SPARK-X's Benjamini-Hochberg
#'   FDR).
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' svgs <- stomics_get_spatially_variable_genes(client, "STD0001", sample_id = 12)
#' }
stomics_get_spatially_variable_genes <- function(client, accession, sample_id = NULL,
                                                 rank_by = "consensus", limit = 50, offset = 0) {
  data <- .stomics_analysis(
    client, accession, "/spatially-variable",
    list(sample_id = sample_id, rank_by = rank_by, limit = limit, offset = offset)
  )
  .stomics_rows(data[["genes"]], c("gene", "rank", "moran_i", "moran_rank", "sparkx_rank", "pval_adj"))
}


#' Download one sample as an .h5ad
#'
#' Raw counts in `X` (values as submitted when they were not counts), a
#' `log1p_cp10k` layer, every submitted and QC column plus the standard
#' pipeline's clusters in `obs`, positions in `obsm$spatial` with every
#' embedding, spatially variable gene statistics in `var` and provenance in
#' `uns$stomics`. Read it with `zellkonverter::readH5AD()` (a
#' `SingleCellExperiment`) or `anndata::read_h5ad()`.
#'
#' @inheritParams stomics_get_standard_analysis
#' @param dest Path to write the file to.
#'
#' @return `dest`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' path <- stomics_download_h5ad(client, "STD0001", "STD0001_12.h5ad", sample_id = 12)
#' sce <- zellkonverter::readH5AD(path)
#' }
stomics_download_h5ad <- function(client, accession, dest, sample_id = NULL) {
  data <- .stomics_analysis(client, accession, "/h5ad", list(sample_id = sample_id))
  download.file(data[["url"]], destfile = dest, mode = "wb", quiet = TRUE)
  invisible(dest)
}


#' Save a view of the spatial viewer
#'
#' A short link and an embed for a viewer state. Requires an API key.
#'
#' @param client A `stomics_client` object.
#' @param accession Dataset accession number.
#' @param state The viewer's state (version 1): a list with `g` genes (up to
#'   four) and optionally `s` sample id, `c` colour mode, `m` metadata field,
#'   `p` palette, `sc` colour range and `z` zoom.
#' @param sample_id,title Optional.
#'
#' @return The saved view, as a list with `slug`, `url` and `embed_url`.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-...")
#' view <- stomics_save_view(client, "STD0001", list(v = 1, g = list("Gfap")), title = "Astrocytes")
#' view$url
#' }
stomics_save_view <- function(client, accession, state, sample_id = NULL, title = NULL) {
  body <- Filter(Negate(is.null), list(accession = accession, state = state, sample_id = sample_id, title = title))
  req <- httr2::req_body_json(.stomics_req(client, "/api/v1/views"), body)
  .stomics_perform(req)
}


#' A dataset's saved views
#'
#' @inheritParams stomics_save_view
#'
#' @return A list of saved views, featured first, then the newest.
#' @export
stomics_list_views <- function(client, accession) {
  .stomics_perform(.stomics_req(client, paste0("/api/v1/datasets/", accession, "/views")))[["items"]]
}


# Internal: GET a standard pipeline endpoint, dropping NULL parameters.
.stomics_analysis <- function(client, accession, path, params) {
  req <- .stomics_req(client, paste0("/api/v1/datasets/", accession, "/analysis/standard", path))
  params <- Filter(Negate(is.null), params)
  if (length(params) > 0) req <- do.call(httr2::req_url_query, c(list(req), params))
  .stomics_perform(req)
}


# Internal: a list of JSON objects as a data.frame with the given columns.
.stomics_rows <- function(rows, columns) {
  out <- lapply(columns, function(col) unlist(lapply(rows, function(r) r[[col]])))
  names(out) <- columns
  if (length(rows) == 0) out <- lapply(out, function(x) logical(0))
  as.data.frame(out, stringsAsFactors = FALSE)
}


#' Every spot's value of one metadata field
#'
#' @param client A `stomics_client` object.
#' @param accession Dataset accession number.
#' @param key A field key from [stomics_expression_manifest()]: a submitted
#'   obs column such as `"leiden"`, or computed QC (`"qc:n_genes"`,
#'   `"qc:n_counts"`, `"qc:pct_mt"`, `"qc:pct_ribo"`).
#' @param sample_id Sample ID (optional).
#'
#' @return A `data.frame` with `barcode` and a column named `key`: character
#'   for categorical fields, numeric (`NA` where a spot has no value) for
#'   numeric ones.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client()
#' clusters <- stomics_get_obs(client, "STD0001", "leiden")
#' }
stomics_get_obs <- function(client, accession, key, sample_id = NULL) {
  req <- .stomics_req(client, paste0("/api/v1/datasets/", accession, "/expression/obs"))
  params <- list(key = key)
  if (!is.null(sample_id)) params[["sample_id"]] <- sample_id
  req <- do.call(httr2::req_url_query, c(list(req), params))
  data <- .stomics_perform(req)
  numeric <- identical(data[["field"]][["kind"]], "numeric")
  values <- vapply(
    data[["values"]],
    function(v) if (is.null(v)) NA_character_ else as.character(v),
    character(1)
  )
  out <- data.frame(barcode = as.character(unlist(data[["barcodes"]])), stringsAsFactors = FALSE)
  out[[key]] <- if (numeric) as.numeric(values) else values
  out
}


#' Load a dataset into a Seurat object
#'
#' Downloads the expression matrix and spatial coordinates for a dataset and
#' constructs a `Seurat` object with the `@images` slot populated from the
#' spatial coordinates.
#'
#' Files are cached in `~/.stomics/cache/{accession}/` to avoid re-downloading.
#'
#' @param client A `stomics_client` object.
#' @param accession Dataset accession number.
#' @param sample_id Restrict to a specific sample (optional).
#'
#' @return A `Seurat` object.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-key")
#' seurat_obj <- stomics_load_seurat(client, "STD0001")
#' }
#' Run differential expression between two spatial regions of interest
#'
#' Submits a differential expression request for two spatial regions defined
#' by spot barcodes. Requires an authenticated client.
#'
#' @param client A `stomics_client` object with a valid API key.
#' @param accession Dataset accession number.
#' @param sample_id Sample ID to run DE on.
#' @param region_a Character vector of spot barcodes defining region A.
#' @param region_b Character vector of spot barcodes for region B, or the
#'   string `"rest"` (default) to compare against all spots not in region A.
#' @param method Statistical test: `"wilcoxon"` (default) or `"t-test"`.
#' @param n_top_genes Maximum number of top genes to return. Default `50`.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{genes}{A `data.frame` with columns `gene`, `mean_a`, `mean_b`,
#'       `log2fc`, `pvalue`, and `padj`, sorted by adjusted p-value.}
#'     \item{n_spots_a}{Integer. Number of spots in region A.}
#'     \item{n_spots_b}{Integer. Number of spots in region B.}
#'     \item{method}{Character. The statistical method used.}
#'     \item{subsampled}{Logical. Whether region B was subsampled.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-key")
#' region_a <- c("BARCODE_r00_c01", "BARCODE_r00_c02", "BARCODE_r01_c01")
#' result <- stomics_differential_expression(client, "STD0001", sample_id = 1L,
#'                                           region_a = region_a)
#' head(result$genes)
#' }
stomics_differential_expression <- function(
    client,
    accession,
    sample_id,
    region_a,
    region_b = "rest",
    method = "wilcoxon",
    n_top_genes = 50L) {
  path <- paste0("/api/v1/datasets/", accession, "/samples/", sample_id,
                 "/differential-expression")
  req <- .stomics_req(client, path)
  body <- list(
    region_a = as.list(region_a),
    region_b = if (identical(region_b, "rest")) "rest" else as.list(region_b),
    method = method,
    n_top_genes = as.integer(n_top_genes)
  )
  req <- httr2::req_body_json(req, body)
  req <- httr2::req_method(req, "POST")
  data <- .stomics_perform(req)

  genes_list <- data[["genes"]]
  if (length(genes_list) == 0L) {
    genes_df <- data.frame(
      gene = character(0), mean_a = numeric(0), mean_b = numeric(0),
      log2fc = numeric(0), pvalue = numeric(0), padj = numeric(0)
    )
  } else {
    genes_df <- do.call(rbind, lapply(genes_list, function(g) {
      data.frame(
        gene    = g[["gene"]],
        mean_a  = g[["mean_a"]],
        mean_b  = g[["mean_b"]],
        log2fc  = g[["log2fc"]],
        pvalue  = g[["pvalue"]],
        padj    = g[["padj"]],
        stringsAsFactors = FALSE
      )
    }))
  }

  list(
    genes      = genes_df,
    n_spots_a  = data[["n_spots_a"]],
    n_spots_b  = data[["n_spots_b"]],
    method     = data[["method"]],
    subsampled = isTRUE(data[["subsampled"]])
  )
}


stomics_load_seurat <- function(client, accession, sample_id = NULL) {
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Seurat is required. Install with: install.packages('Seurat')", call. = FALSE)
  }

  cache_dir <- file.path(path.expand("~"), ".stomics", "cache", accession)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  files <- stomics_list_files(client, accession)

  .matches_sample <- function(f) {
    is.null(sample_id) || identical(as.character(f[["sample_id"]]), as.character(sample_id))
  }

  matrix_file <- NULL
  for (f in files) {
    if (identical(f[["file_type"]], "expression_matrix") && .matches_sample(f)) {
      matrix_file <- f
      break
    }
  }

  if (is.null(matrix_file)) {
    stop(sprintf(
      "No expression matrix found for accession=%s, sample_id=%s",
      accession, sample_id %||% "NULL"
    ), call. = FALSE)
  }

  matrix_path <- file.path(cache_dir, matrix_file[["filename"]])
  if (!file.exists(matrix_path)) {
    dl_url <- matrix_file[["download_url"]]
    if (!is.null(dl_url) && nzchar(dl_url)) {
      download.file(dl_url, destfile = matrix_path, mode = "wb", quiet = TRUE)
    } else {
      stomics_download_file(client, accession, matrix_file[["id"]], matrix_path)
    }
  }

  counts <- Seurat::Read10X_h5(matrix_path)
  seurat_obj <- Seurat::CreateSeuratObject(counts = counts)

  coords_file <- NULL
  for (f in files) {
    if (identical(f[["file_type"]], "spatial_coordinates") && .matches_sample(f)) {
      coords_file <- f
      break
    }
  }

  if (!is.null(coords_file)) {
    coords_path <- file.path(cache_dir, coords_file[["filename"]])
    if (!file.exists(coords_path)) {
      dl_url <- coords_file[["download_url"]]
      if (!is.null(dl_url) && nzchar(dl_url)) {
        download.file(dl_url, destfile = coords_path, mode = "wb", quiet = TRUE)
      } else {
        stomics_download_file(client, accession, coords_file[["id"]], coords_path)
      }
    }
    tryCatch({
      coords <- utils::read.csv(coords_path)
      if (all(c("x", "y") %in% names(coords))) {
        tissue_coords <- data.frame(
          imagerow = coords[["x"]],
          imagecol = coords[["y"]],
          row.names = if ("barcode" %in% names(coords)) coords[["barcode"]] else rownames(coords)
        )
        seurat_obj@images[["slice1"]] <- new(
          "VisiumV1",
          coordinates = tissue_coords,
          scale.factors = list(fiducial = 1, hires = 1, lowres = 1),
          image = array(0, dim = c(1, 1, 3)),
          spot.radius = 0.5
        )
      }
    }, error = function(e) {
      message("Warning: could not attach spatial coordinates: ", conditionMessage(e))
    })
  }

  seurat_obj
}


# ---------------------------------------------------------------------------
# Gene Sets and Module Score (Feature 16)
# ---------------------------------------------------------------------------

#' List built-in gene sets from the Stomics catalog
#'
#' Returns a searchable catalog of gene sets from MSigDB Hallmark, GO Biological
#' Process, KEGG, and custom sources.
#'
#' @param client A `stomics_client` object.
#' @param q Optional search term matched against gene set name (case-insensitive).
#' @param source Optional filter by source. One of `"MSigDB_Hallmark"`,
#'   `"GO_BP"`, `"KEGG"`, or `"custom"`.
#' @param limit Maximum number of results. Default `50`, max `200`.
#' @param offset Pagination offset. Default `0`.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{gene_sets}{A list of gene set objects, each with `id`, `name`,
#'       `source`, `gene_symbols` (character vector), and `description`.}
#'     \item{total}{Integer. Total number of matching gene sets.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-key")
#' sets   <- stomics_gene_sets(client, q = "hallmark")
#' cat("Found", sets$total, "gene sets\n")
#' cat("First set:", sets$gene_sets[[1]]$name, "\n")
#' }
stomics_gene_sets <- function(
    client,
    q      = NULL,
    source = NULL,
    limit  = 50L,
    offset = 0L) {
  params <- list(limit = as.integer(limit), offset = as.integer(offset))
  if (!is.null(q))      params[["q"]]      <- q
  if (!is.null(source)) params[["source"]] <- source

  path <- "/api/v1/gene-sets"
  if (length(params) > 0L) {
    qs   <- paste(names(params), unlist(params), sep = "=", collapse = "&")
    path <- paste0(path, "?", qs)
  }
  req  <- .stomics_req(client, path)
  data <- .stomics_perform(req)

  gene_sets <- lapply(data[["gene_sets"]], function(gs) {
    list(
      id           = gs[["id"]],
      name         = gs[["name"]],
      source       = gs[["source"]],
      gene_symbols = unlist(gs[["gene_symbols"]]),
      description  = gs[["description"]]
    )
  })

  list(gene_sets = gene_sets, total = data[["total"]])
}


#' Compute per-spot module scores for a gene set
#'
#' Calculates a pathway / gene set activity score for every spot in a sample
#' using the Seurat `AddModuleScore` approach: the mean expression of matched
#' gene set genes minus the mean of a random same-size background gene set.
#'
#' Exactly one of `gene_set_id` or `custom_genes` must be provided.
#'
#' @param client A `stomics_client` object.
#' @param accession Dataset accession number.
#' @param sample_id Sample ID to score.
#' @param gene_set_id Integer. ID of a built-in gene set from
#'   [stomics_gene_sets()]. Mutually exclusive with `custom_genes`.
#' @param custom_genes Character vector of gene symbols. Mutually exclusive with
#'   `gene_set_id`.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{spots}{A `data.frame` with columns `barcode`, `x`, `y`, `score`.}
#'     \item{method}{Character. Scoring method (`"mean_normalized"`).}
#'     \item{gene_set_name}{Character. Name of the gene set used.}
#'     \item{n_genes_total}{Integer. Total genes in the gene set.}
#'     \item{n_genes_matched}{Integer. Genes found in this dataset.}
#'     \item{matched_genes}{Character vector of matched gene symbols.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' client  <- stomics_client(api_key = "sk-key")
#' sets    <- stomics_gene_sets(client, q = "hallmark_inflam")
#' gs_id   <- sets$gene_sets[[1]]$id
#'
#' # Built-in gene set
#' result  <- stomics_module_score(client, "STD0001", sample_id = 1L,
#'                                  gene_set_id = gs_id)
#'
#' # Custom gene list
#' custom  <- stomics_module_score(client, "STD0001", sample_id = 1L,
#'                                  custom_genes = c("MKI67", "TOP2A", "PCNA"))
#' head(result$spots)
#' cat("Matched", result$n_genes_matched, "/", result$n_genes_total, "genes\n")
#' }
stomics_module_score <- function(
    client,
    accession,
    sample_id,
    gene_set_id  = NULL,
    custom_genes = NULL) {
  if (is.null(gene_set_id) && is.null(custom_genes)) {
    stop("Exactly one of gene_set_id or custom_genes must be provided.", call. = FALSE)
  }
  if (!is.null(gene_set_id) && !is.null(custom_genes)) {
    stop("Provide gene_set_id or custom_genes, not both.", call. = FALSE)
  }

  path <- paste0("/api/v1/datasets/", accession, "/samples/", sample_id,
                 "/module-score")
  req  <- .stomics_req(client, path)
  body <- list()
  if (!is.null(gene_set_id))  body[["gene_set_id"]]  <- as.integer(gene_set_id)
  if (!is.null(custom_genes)) body[["custom_genes"]] <- as.list(custom_genes)
  req  <- httr2::req_body_json(req, body)
  req  <- httr2::req_method(req, "POST")
  data <- .stomics_perform(req)

  spots_list <- data[["spots"]]
  if (length(spots_list) == 0L) {
    spots_df <- data.frame(
      barcode = character(0), x = numeric(0), y = numeric(0), score = numeric(0)
    )
  } else {
    spots_df <- do.call(rbind, lapply(spots_list, function(s) {
      data.frame(
        barcode = s[["barcode"]],
        x       = s[["x"]],
        y       = s[["y"]],
        score   = s[["score"]],
        stringsAsFactors = FALSE
      )
    }))
  }

  list(
    spots          = spots_df,
    method         = data[["method"]],
    gene_set_name  = data[["gene_set_name"]],
    n_genes_total  = data[["n_genes_total"]],
    n_genes_matched = data[["n_genes_matched"]],
    matched_genes  = unlist(data[["matched_genes"]])
  )
}


# ---------------------------------------------------------------------------
# One-Click Launch to Cloud Analysis Platforms (Feature 20)
# ---------------------------------------------------------------------------

#' Generate a cloud-platform launch URL for a dataset
#'
#' Constructs a URL that opens the dataset directly in Terra, Galaxy, or
#' Seqera Platform — no manual file transfer required.
#'
#' @param client A `stomics_client` object created with [stomics_client()].
#' @param accession Dataset accession, e.g. `"STD0001"`.
#' @param platform Target platform: `"terra"`, `"galaxy"`, or `"seqera"`.
#' @param file_types Optional character vector of file types to include, e.g.
#'   `c("expression_matrix", "spatial_coordinates")`. When `NULL` (default)
#'   all files are included.
#' @param workflow_id Optional Seqera pipeline ID (only used when
#'   `platform = "seqera"`).
#'
#' @return A list with elements `launch_url` (character) and `platform`
#'   (character). Open `launch_url` in a browser to launch the analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-your-key")
#' result <- stomics_launch_to_platform(client, "STD0001", platform = "terra")
#' browseURL(result$launch_url)
#' }
stomics_launch_to_platform <- function(client, accession, platform,
                                       file_types = NULL,
                                       workflow_id = NULL) {
  path <- paste0("/api/v1/datasets/", accession, "/launch-bundle")
  req  <- .stomics_req(client, path)
  body <- list(platform = platform)
  if (!is.null(file_types)) body[["file_types"]] <- as.list(file_types)
  if (!is.null(workflow_id)) body[["workflow_id"]] <- workflow_id
  req  <- httr2::req_body_json(req, body)
  req  <- httr2::req_method(req, "POST")
  data <- .stomics_perform(req)
  list(launch_url = data[["launch_url"]], platform = data[["platform"]])
}


#' Download an aria2c URL manifest for parallel file download
#'
#' Writes a plain-text file formatted for `aria2c -i <dest_path> -x 16 -s 16`.
#' This is a power-user download option that uses parallel connections and is
#' faster than a ZIP bundle for large datasets.
#'
#' @param client A `stomics_client` object created with [stomics_client()].
#' @param accession Dataset accession, e.g. `"STD0001"`.
#' @param dest_path Local file path to write the manifest to. Defaults to
#'   `"<accession>-aria2c.txt"` in the current working directory.
#' @param file_types Optional character vector of file types to include.
#'   When `NULL` (default) all files are included.
#'
#' @return Invisibly returns `dest_path`.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-your-key")
#' stomics_download_aria2c_manifest(client, "STD0001", "STD0001-aria2c.txt")
#' # Then in a terminal: aria2c -i STD0001-aria2c.txt -x 16 -s 16
#' }
stomics_download_aria2c_manifest <- function(client, accession,
                                             dest_path = paste0(accession, "-aria2c.txt"),
                                             file_types = NULL) {
  path <- paste0("/api/v1/datasets/", accession, "/aria2c-manifest")
  req  <- .stomics_req(client, path)
  if (!is.null(file_types)) {
    req <- httr2::req_url_query(req, .multi = "explode",
                                file_types = as.character(file_types))
  }
  resp <- httr2::req_perform(req)
  content <- httr2::resp_body_string(resp)
  writeLines(content, dest_path)
  invisible(dest_path)
}

#' Find spatially variable genes for a sample
#'
#' Identifies genes whose expression is spatially structured across the tissue
#' using Moran's I spatial autocorrelation. Results are cached server-side for
#' 24 hours; repeated calls with the same parameters return almost instantly.
#'
#' @param client A `stomics_client` object created with [stomics_client()].
#' @param accession Dataset accession, e.g. `"STD0001"`.
#' @param sample_id Integer sample ID.
#' @param method Spatial autocorrelation method. Currently `"moran_i"`
#'   (Moran's I, default).
#' @param n_top_genes Integer. Number of top-ranked genes to return (1–500,
#'   default `50`).
#' @param min_expr_fraction Numeric (0–1). Minimum fraction of spots expressing
#'   a gene (default `0.1`). Genes below this threshold are excluded before
#'   testing.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`genes`}{A data frame with columns `gene`, `morans_i`, `pvalue`,
#'       `padj`, `mean_expr`, `n_spots_expressed`, sorted by `morans_i`
#'       descending.}
#'     \item{`method`}{Character. Method used.}
#'     \item{`n_genes_tested`}{Integer. Total number of genes tested.}
#'     \item{`sample_id`}{Integer. Sample ID.}
#'     \item{`subsampled`}{Logical. `TRUE` if the sample was subsampled to
#'       5,000 spots.}
#'     \item{`n_spots`}{Integer. Number of spots used in the analysis.}
#'     \item{`cached`}{Logical. `TRUE` if the result was served from cache.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-your-key")
#' result <- stomics_spatially_variable_genes(client, "STD0001", sample_id = 1L)
#' cat(sprintf("Tested %d genes across %d spots\n",
#'             result$n_genes_tested, result$n_spots))
#' head(result$genes[, c("gene", "morans_i", "padj")], 10)
#' }
stomics_spatially_variable_genes <- function(client, accession, sample_id,
                                             method = "moran_i",
                                             n_top_genes = 50L,
                                             min_expr_fraction = 0.1) {
  path <- paste0("/api/v1/datasets/", accession, "/samples/",
                 as.integer(sample_id), "/spatially-variable-genes")
  req  <- .stomics_req(client, path)
  body <- list(
    method             = method,
    n_top_genes        = as.integer(n_top_genes),
    min_expr_fraction  = as.numeric(min_expr_fraction)
  )
  req  <- httr2::req_body_json(req, body)
  req  <- httr2::req_method(req, "POST")
  data <- .stomics_perform(req)

  genes_list <- data[["genes"]]
  if (length(genes_list) == 0L) {
    genes_df <- data.frame(
      gene              = character(0),
      morans_i          = numeric(0),
      pvalue            = numeric(0),
      padj              = numeric(0),
      mean_expr         = numeric(0),
      n_spots_expressed = integer(0)
    )
  } else {
    genes_df <- do.call(rbind, lapply(genes_list, function(g) {
      data.frame(
        gene              = g[["gene"]],
        morans_i          = g[["morans_i"]],
        pvalue            = g[["pvalue"]],
        padj              = g[["padj"]],
        mean_expr         = g[["mean_expr"]],
        n_spots_expressed = as.integer(g[["n_spots_expressed"]]),
        stringsAsFactors  = FALSE
      )
    }))
  }

  list(
    genes          = genes_df,
    method         = data[["method"]],
    n_genes_tested = as.integer(data[["n_genes_tested"]]),
    sample_id      = as.integer(data[["sample_id"]]),
    subsampled     = isTRUE(data[["subsampled"]]),
    n_spots        = as.integer(data[["n_spots"]]),
    cached         = isTRUE(data[["cached"]])
  )
}

# ---------------------------------------------------------------------------
# Gene-Centric Cross-Dataset Expression Search (Feature 26)
# ---------------------------------------------------------------------------

#' Search for datasets expressing a gene across the entire catalogue
#'
#' Queries the gene expression index to find all datasets and samples where
#' a given gene is expressed above the specified threshold.
#'
#' @param client A Stomics client created with \code{\link{stomics_client}}.
#' @param gene Gene symbol to search for (e.g. \code{"GAPDH"}).
#'   Case-insensitive.
#' @param organism_id Optional ontology-term integer ID to restrict results to
#'   a specific organism.
#' @param tissue_id Optional ontology-term integer ID to restrict results to a
#'   specific tissue.
#' @param technology_id Optional integer ID to restrict results to a specific
#'   assay technology.
#' @param min_pct_expressed Minimum fraction of spots that must express the
#'   gene (0–1). Defaults to \code{0.1} (10 \%).
#' @param sort_by Sort results by \code{"mean_expr"} (default) or
#'   \code{"pct_spots_expressed"}.
#' @param page Page number (1-based). Default \code{1L}.
#' @param per_page Results per page. Default \code{20L}.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{\code{items}}{A data frame with columns \code{dataset_accession},
#'       \code{dataset_title}, \code{sample_id}, \code{organism},
#'       \code{tissue}, \code{technology}, \code{mean_expr},
#'       \code{pct_spots_expressed}, \code{max_expr}.}
#'     \item{\code{total}}{Total number of matching records.}
#'     \item{\code{page}}{Current page number.}
#'     \item{\code{per_page}}{Page size.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-your-key")
#' result <- stomics_search_by_gene(client, "GAPDH", min_pct_expressed = 0.2)
#' cat(sprintf("Found %d datasets expressing GAPDH\n", result$total))
#' head(result$items[, c("dataset_accession", "pct_spots_expressed")])
#' }
stomics_search_by_gene <- function(client, gene,
                                   organism_id        = NULL,
                                   tissue_id          = NULL,
                                   technology_id      = NULL,
                                   min_pct_expressed  = 0.1,
                                   sort_by            = "mean_expr",
                                   page               = 1L,
                                   per_page           = 20L) {
  base_req   <- .stomics_req(client, "/api/v1/search/genes")
  query_args <- list(
    q                 = gene,
    min_pct_expressed = as.numeric(min_pct_expressed),
    sort_by           = sort_by,
    page              = as.integer(page),
    per_page          = as.integer(per_page)
  )
  if (!is.null(organism_id))   query_args$organism_id   <- as.integer(organism_id)
  if (!is.null(tissue_id))     query_args$tissue_id     <- as.integer(tissue_id)
  if (!is.null(technology_id)) query_args$technology_id <- as.integer(technology_id)

  req  <- do.call(httr2::req_url_query, c(list(.req = base_req), query_args))
  data <- .stomics_perform(req)

  .str1 <- function(x) if (is.null(x) || length(x) == 0L) NA_character_ else as.character(x)

  items_list <- data[["items"]]
  if (length(items_list) == 0L) {
    items_df <- data.frame(
      dataset_accession   = character(0),
      dataset_title       = character(0),
      sample_id           = integer(0),
      organism            = character(0),
      tissue              = character(0),
      technology          = character(0),
      mean_expr           = numeric(0),
      pct_spots_expressed = numeric(0),
      max_expr            = numeric(0),
      stringsAsFactors    = FALSE
    )
  } else {
    items_df <- do.call(rbind, lapply(items_list, function(r) {
      data.frame(
        dataset_accession   = r[["dataset_accession"]],
        dataset_title       = r[["dataset_title"]],
        # NULL for a dataset-level expression store (a dataset with no samples).
        sample_id           = if (is.null(r[["sample_id"]])) NA_integer_ else as.integer(r[["sample_id"]]),
        organism            = .str1(r[["organism"]]),
        tissue              = .str1(r[["tissue"]]),
        technology          = .str1(r[["technology"]]),
        mean_expr           = as.numeric(r[["mean_expr"]]),
        pct_spots_expressed = as.numeric(r[["pct_spots_expressed"]]),
        max_expr            = as.numeric(r[["max_expr"]]),
        stringsAsFactors    = FALSE
      )
    }))
  }

  list(
    items    = items_df,
    total    = as.integer(data[["total"]]),
    page     = as.integer(data[["page"]]),
    per_page = as.integer(data[["per_page"]])
  )
}

#' Autocomplete gene symbol suggestions
#'
#' Returns gene symbols matching a given prefix that appear in published
#' datasets in the catalogue.
#'
#' @param client A Stomics client created with \code{\link{stomics_client}}.
#' @param prefix Partial gene symbol (e.g. \code{"GAP"}).
#' @param per_page Maximum number of suggestions to return. Default \code{10L}.
#'
#' @return A data frame with columns \code{gene_symbol} and
#'   \code{dataset_count}.
#' @export
#'
#' @examples
#' \dontrun{
#' client <- stomics_client(api_key = "sk-your-key")
#' suggestions <- stomics_gene_autocomplete(client, "FOX")
#' suggestions$gene_symbol
#' }
stomics_gene_autocomplete <- function(client, prefix, per_page = 10L) {
  req  <- .stomics_req(client, "/api/v1/search/genes/autocomplete")
  req  <- httr2::req_url_query(req, q = prefix, per_page = as.integer(per_page))
  data <- .stomics_perform(req)

  if (length(data) == 0L) {
    return(data.frame(
      gene_symbol   = character(0),
      dataset_count = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(data, function(s) {
    data.frame(
      gene_symbol   = s[["gene_symbol"]],
      dataset_count = as.integer(s[["dataset_count"]]),
      stringsAsFactors = FALSE
    )
  }))
}
