library(testthat)
library(stomics)

# ---------------------------------------------------------------------------
# stomics_client
# ---------------------------------------------------------------------------

test_that("stomics_client stores api_key and base_url", {
  client <- stomics_client(api_key = "sk-test", base_url = "https://api.example.com")
  expect_equal(client$api_key, "sk-test")
  expect_equal(client$base_url, "https://api.example.com")
  expect_s3_class(client, "stomics_client")
})

test_that("stomics_client strips trailing slash from base_url", {
  client <- stomics_client(api_key = "k", base_url = "https://api.example.com/")
  expect_equal(client$base_url, "https://api.example.com")
})

test_that("stomics_client reads api_key from env var", {
  withr::with_envvar(list(STOMICS_API_KEY = "sk-from-env"), {
    client <- stomics_client()
    expect_equal(client$api_key, "sk-from-env")
  })
})

test_that("stomics_client api_key is NULL when env var not set", {
  withr::with_envvar(list(STOMICS_API_KEY = ""), {
    client <- stomics_client()
    expect_null(client$api_key)
  })
})

test_that("explicit api_key takes precedence over env var", {
  withr::with_envvar(list(STOMICS_API_KEY = "sk-env"), {
    client <- stomics_client(api_key = "sk-explicit")
    expect_equal(client$api_key, "sk-explicit")
  })
})

# ---------------------------------------------------------------------------
# .stomics_req — internal helper
# ---------------------------------------------------------------------------

test_that(".stomics_req builds request with auth header when api_key present", {
  client <- stomics_client(api_key = "sk-abc", base_url = "https://api.example.com")
  req <- stomics:::.stomics_req(client, "/api/v1/datasets")
  expect_true(grepl("https://api.example.com/api/v1/datasets", req$url))
  expect_false(is.null(req$headers[["Authorization"]]))
})

test_that(".stomics_req builds request without auth header when api_key NULL", {
  client <- stomics_client(base_url = "https://api.example.com")
  client$api_key <- NULL
  req <- stomics:::.stomics_req(client, "/api/v1/datasets")
  expect_null(req$headers[["Authorization"]])
})

# ---------------------------------------------------------------------------
# stomics_search — mocked
# ---------------------------------------------------------------------------

test_that("stomics_search constructs correct request and parses response", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  webmockr::stub_request("get", uri_regex = "https://api.example.com/api/v1/datasets") |>
    webmockr::to_return(
      body = '{"items":[{"accession":"STD0001","title":"Brain Atlas","status":"published","download_count":5}],"total":1,"page":1,"per_page":20}',
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-test", base_url = "https://api.example.com")
  result <- stomics_search(client)

  expect_equal(result$total, 1)
  expect_equal(length(result$items), 1)
  expect_equal(result$items[[1]]$accession, "STD0001")
})

test_that("stomics_search passes query parameters", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  webmockr::stub_request("get", uri_regex = "https://api.example.com/api/v1/datasets.*") |>
    webmockr::to_return(
      body = '{"items":[],"total":0,"page":1,"per_page":5}',
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "k", base_url = "https://api.example.com")
  result <- stomics_search(
    client,
    query = "brain",
    organism = "1",
    tissue = "2",
    disease = "3",
    technology = "4",
    page = 1L,
    per_page = 5L
  )

  expect_equal(result$total, 0)
})

test_that("stomics_search sends each ID as a repeated parameter", {
  captured <- NULL
  httr2::local_mocked_responses(function(req) {
    captured <<- req$url
    httr2::response_json(body = list(items = list(), total = 0, page = 1, per_page = 20))
  })

  client <- stomics_client(api_key = "k", base_url = "https://api.example.com")
  stomics_search(client, organism = "1, 2", tissue = c(3L, 4L))

  expect_match(captured, "organism_ids=1&organism_ids=2", fixed = TRUE)
  expect_match(captured, "tissue_ids=3&tissue_ids=4", fixed = TRUE)
  expect_false(grepl("%2C", captured, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# stomics_get_dataset — mocked
# ---------------------------------------------------------------------------

test_that("stomics_get_dataset returns dataset metadata", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  webmockr::stub_request("get", "https://api.example.com/api/v1/datasets/STD0001") |>
    webmockr::to_return(
      body = '{"accession":"STD0001","title":"Brain Atlas","status":"published","download_count":42}',
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "k", base_url = "https://api.example.com")
  ds <- stomics_get_dataset(client, "STD0001")

  expect_equal(ds$accession, "STD0001")
  expect_equal(ds$title, "Brain Atlas")
  expect_equal(ds$download_count, 42)
})

# ---------------------------------------------------------------------------
# stomics_list_files — mocked
# ---------------------------------------------------------------------------

test_that("stomics_list_files returns file list with download_url", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  webmockr::stub_request("get", "https://api.example.com/api/v1/datasets/STD0001/files") |>
    webmockr::to_return(
      body = '[{"id":1,"dataset_id":1,"filename":"matrix.h5ad","file_type":"expression_matrix","upload_status":"complete","created_at":"2025-01-01T00:00:00Z","download_url":"https://s3.example.com/matrix.h5ad"}]',
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "k", base_url = "https://api.example.com")
  files <- stomics_list_files(client, "STD0001")

  expect_equal(length(files), 1)
  expect_equal(files[[1]]$filename, "matrix.h5ad")
  expect_equal(files[[1]]$download_url, "https://s3.example.com/matrix.h5ad")
})

# ---------------------------------------------------------------------------
# stomics_download_file — mocked
# ---------------------------------------------------------------------------

test_that("stomics_download_file downloads file to dest_path", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  presigned <- "https://s3.example.com/file.h5ad"

  webmockr::stub_request("get", "https://api.example.com/api/v1/datasets/STD0001/files/1/download") |>
    webmockr::to_return(
      body = paste0('{"url":"', presigned, '"}'),
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  dest <- withr::local_tempfile(fileext = ".h5ad")
  client <- stomics_client(api_key = "k", base_url = "https://api.example.com")

  # mock download.file so we don't actually fetch the presigned URL
  mockery <- tryCatch(loadNamespace("mockery"), error = function(e) NULL)
  if (!is.null(mockery)) {
    mockery::stub(stomics_download_file, "download.file", function(url, destfile, mode, quiet) {
      writeLines("fake-content", destfile)
    })
    stomics_download_file(client, "STD0001", 1L, dest)
    expect_true(file.exists(dest))
  } else {
    skip("mockery package not available")
  }
})

# ---------------------------------------------------------------------------
# stomics_get_expression — mocked
# ---------------------------------------------------------------------------

test_that("stomics_get_expression returns a data.frame of spots", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  webmockr::stub_request("get", uri_regex = "https://api.example.com/api/v1/datasets/STD0001/expression.*") |>
    webmockr::to_return(
      body = '{"spots":[{"x":1,"y":2,"expression":3.5},{"x":3,"y":4,"expression":0.0}]}',
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "k", base_url = "https://api.example.com")
  df <- stomics_get_expression(client, "STD0001", gene = "BRCA1")

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_true(all(c("x", "y", "expression") %in% names(df)))
  expect_equal(df$expression[1], 3.5)
})

test_that("stomics_get_expression returns empty data.frame when no spots", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  webmockr::stub_request("get", uri_regex = "https://api.example.com/api/v1/datasets/STD0001/expression.*") |>
    webmockr::to_return(
      body = '{"spots":[]}',
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "k", base_url = "https://api.example.com")
  df <- stomics_get_expression(client, "STD0001", gene = "NOTEXIST")

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
  expect_true(all(c("x", "y", "expression") %in% names(df)))
})

test_that("stomics_get_expression passes sample_id parameter", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  webmockr::stub_request("get", uri_regex = "https://api.example.com/api/v1/datasets/STD0001/expression.*") |>
    webmockr::to_return(
      body = '{"spots":[]}',
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "k", base_url = "https://api.example.com")
  df <- stomics_get_expression(client, "STD0001", gene = "TP53", sample_id = "sample_1")
  expect_s3_class(df, "data.frame")
})

# ---------------------------------------------------------------------------
# stomics_load_seurat
# ---------------------------------------------------------------------------

test_that("stomics_load_seurat errors when Seurat not installed", {
  client <- stomics_client(api_key = "k", base_url = "https://api.example.com")
  # Only run this assertion if Seurat is actually absent
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    expect_error(
      stomics_load_seurat(client, "STD0001"),
      "Seurat is required"
    )
  } else {
    skip("Seurat is installed; cannot test absent-Seurat path")
  }
})

test_that("stomics_load_seurat errors when no expression matrix found", {
  skip_if_not_installed("webmockr")
  skip_if_not_installed("Seurat")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  webmockr::stub_request("get", "https://api.example.com/api/v1/datasets/STD0001/files") |>
    webmockr::to_return(
      body = "[]",
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "k", base_url = "https://api.example.com")
  expect_error(
    stomics_load_seurat(client, "STD0001"),
    "No expression matrix found"
  )
})

# ---------------------------------------------------------------------------
# %||% operator
# ---------------------------------------------------------------------------

test_that("%||% returns left value when non-NULL", {
  expect_equal("left" %||% "right", "left")
})

test_that("%||% returns right value when left is NULL", {
  expect_equal(NULL %||% "right", "right")
})

# ---------------------------------------------------------------------------
# stomics_differential_expression
# ---------------------------------------------------------------------------

test_that("stomics_differential_expression returns data.frame of genes with correct columns", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  de_payload <- jsonlite::toJSON(list(
    genes = list(
      list(gene = "MYH7", mean_a = 60.0, mean_b = 5.0, log2fc = 3.58, pvalue = 0.001, padj = 0.01),
      list(gene = "GAPDH", mean_a = 35.0, mean_b = 30.0, log2fc = 0.22, pvalue = 0.5, padj = 0.8)
    ),
    n_spots_a = 10L,
    n_spots_b = 50L,
    method = "wilcoxon",
    subsampled = FALSE
  ), auto_unbox = TRUE)

  webmockr::stub_request(
    "post",
    "https://api.example.com/api/v1/datasets/STD0001/samples/1/differential-expression"
  ) |>
    webmockr::to_return(
      body = de_payload,
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_differential_expression(
    client, "STD0001",
    sample_id = 1L,
    region_a = c("BARCODE_r00_c01", "BARCODE_r00_c02")
  )

  expect_type(result, "list")
  expect_s3_class(result$genes, "data.frame")
  expect_equal(nrow(result$genes), 2L)
  expect_named(result$genes, c("gene", "mean_a", "mean_b", "log2fc", "pvalue", "padj"))
  expect_equal(result$genes$gene[1], "MYH7")
  expect_equal(result$genes$log2fc[1], 3.58)
  expect_equal(result$n_spots_a, 10L)
  expect_equal(result$n_spots_b, 50L)
  expect_equal(result$method, "wilcoxon")
  expect_false(result$subsampled)
})

test_that("stomics_differential_expression passes t-test method correctly", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  de_payload <- jsonlite::toJSON(list(
    genes = list(),
    n_spots_a = 5L, n_spots_b = 20L, method = "t-test", subsampled = FALSE
  ), auto_unbox = TRUE)

  webmockr::stub_request(
    "post",
    "https://api.example.com/api/v1/datasets/STD0001/samples/2/differential-expression"
  ) |>
    webmockr::to_return(
      body = de_payload,
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_differential_expression(
    client, "STD0001",
    sample_id = 2L,
    region_a = c("BARCODE_r00_c01"),
    method = "t-test"
  )
  expect_equal(result$method, "t-test")
  expect_equal(nrow(result$genes), 0L)
})

test_that("stomics_differential_expression handles empty genes response", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  de_payload <- jsonlite::toJSON(list(
    genes = list(),
    n_spots_a = 1L, n_spots_b = 5L, method = "wilcoxon", subsampled = FALSE
  ), auto_unbox = TRUE)

  webmockr::stub_request(
    "post",
    "https://api.example.com/api/v1/datasets/STD0001/samples/1/differential-expression"
  ) |>
    webmockr::to_return(
      body = de_payload,
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_differential_expression(client, "STD0001", 1L, region_a = c("B1"))
  expect_s3_class(result$genes, "data.frame")
  expect_equal(nrow(result$genes), 0L)
  expect_named(result$genes, c("gene", "mean_a", "mean_b", "log2fc", "pvalue", "padj"))
})

test_that("stomics_differential_expression marks subsampled correctly", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  de_payload <- jsonlite::toJSON(list(
    genes = list(),
    n_spots_a = 10L, n_spots_b = 5000L, method = "wilcoxon", subsampled = TRUE
  ), auto_unbox = TRUE)

  webmockr::stub_request(
    "post",
    "https://api.example.com/api/v1/datasets/STD0001/samples/1/differential-expression"
  ) |>
    webmockr::to_return(
      body = de_payload,
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_differential_expression(client, "STD0001", 1L, region_a = c("B1"))
  expect_true(result$subsampled)
})

# ---------------------------------------------------------------------------
# stomics_gene_sets — mocked (Feature 16)
# ---------------------------------------------------------------------------

test_that("stomics_gene_sets returns gene_sets list and total", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  payload <- jsonlite::toJSON(list(
    gene_sets = list(
      list(id = 1L, name = "HALLMARK_INFLAMMATORY_RESPONSE", source = "MSigDB_Hallmark",
           gene_symbols = list("PTPRC", "CD68", "CD4"), description = "Inflammatory response"),
      list(id = 2L, name = "GO_MYELINATION", source = "GO_BP",
           gene_symbols = list("MBP", "PLP1"), description = NULL)
    ),
    total = 2L
  ), auto_unbox = TRUE, null = "null")

  webmockr::stub_request("get", uri_regex = "https://api.example.com/api/v1/gene-sets.*") |>
    webmockr::to_return(
      body = payload,
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_gene_sets(client)

  expect_type(result, "list")
  expect_equal(result$total, 2L)
  expect_equal(length(result$gene_sets), 2L)
  gs1 <- result$gene_sets[[1]]
  expect_equal(gs1$id, 1L)
  expect_equal(gs1$name, "HALLMARK_INFLAMMATORY_RESPONSE")
  expect_equal(gs1$source, "MSigDB_Hallmark")
  expect_equal(gs1$gene_symbols, c("PTPRC", "CD68", "CD4"))
  expect_equal(gs1$description, "Inflammatory response")
})

test_that("stomics_gene_sets passes q and source parameters", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  payload <- jsonlite::toJSON(list(gene_sets = list(), total = 0L), auto_unbox = TRUE)

  webmockr::stub_request("get", uri_regex = "https://api.example.com/api/v1/gene-sets.*") |>
    webmockr::to_return(
      body = payload,
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_gene_sets(client, q = "hallmark", source = "MSigDB_Hallmark", limit = 10L, offset = 0L)

  expect_equal(result$total, 0L)
  expect_equal(length(result$gene_sets), 0L)
})

test_that("stomics_gene_sets handles empty catalog", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  payload <- jsonlite::toJSON(list(gene_sets = list(), total = 0L), auto_unbox = TRUE)

  webmockr::stub_request("get", uri_regex = "https://api.example.com/api/v1/gene-sets.*") |>
    webmockr::to_return(
      body = payload,
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_gene_sets(client)

  expect_equal(result$total, 0L)
  expect_equal(length(result$gene_sets), 0L)
})

# ---------------------------------------------------------------------------
# stomics_module_score — mocked (Feature 16)
# ---------------------------------------------------------------------------

test_that("stomics_module_score returns spots data.frame with correct columns", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  score_payload <- jsonlite::toJSON(list(
    spots = list(
      list(barcode = "BAR_A", x = 10.0, y = 20.0, score = 1.2),
      list(barcode = "BAR_B", x = 30.0, y = 40.0, score = -0.5)
    ),
    method = "mean_normalized",
    gene_set_name = "HALLMARK_INFLAMMATORY_RESPONSE",
    n_genes_total = 3L,
    n_genes_matched = 2L,
    matched_genes = list("PTPRC", "CD68")
  ), auto_unbox = TRUE)

  webmockr::stub_request(
    "post",
    "https://api.example.com/api/v1/datasets/STD0001/samples/1/module-score"
  ) |>
    webmockr::to_return(
      body = score_payload,
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_module_score(client, "STD0001", sample_id = 1L, gene_set_id = 1L)

  expect_type(result, "list")
  expect_s3_class(result$spots, "data.frame")
  expect_equal(nrow(result$spots), 2L)
  expect_named(result$spots, c("barcode", "x", "y", "score"))
  expect_equal(result$spots$barcode[1], "BAR_A")
  expect_equal(result$spots$score[1], 1.2)
  expect_equal(result$spots$score[2], -0.5)
  expect_equal(result$method, "mean_normalized")
  expect_equal(result$gene_set_name, "HALLMARK_INFLAMMATORY_RESPONSE")
  expect_equal(result$n_genes_total, 3L)
  expect_equal(result$n_genes_matched, 2L)
  expect_equal(result$matched_genes, c("PTPRC", "CD68"))
})

test_that("stomics_module_score works with custom_genes", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  score_payload <- jsonlite::toJSON(list(
    spots = list(
      list(barcode = "BAR_C", x = 5.0, y = 10.0, score = 0.8)
    ),
    method = "mean_normalized",
    gene_set_name = "Custom gene set",
    n_genes_total = 2L,
    n_genes_matched = 1L,
    matched_genes = list("MKI67")
  ), auto_unbox = TRUE)

  webmockr::stub_request(
    "post",
    "https://api.example.com/api/v1/datasets/STD0001/samples/2/module-score"
  ) |>
    webmockr::to_return(
      body = score_payload,
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_module_score(client, "STD0001", sample_id = 2L,
                                  custom_genes = c("MKI67", "TOP2A"))

  expect_equal(result$gene_set_name, "Custom gene set")
  expect_equal(result$matched_genes, c("MKI67"))
  expect_equal(nrow(result$spots), 1L)
})

test_that("stomics_module_score raises error when neither param provided", {
  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  expect_error(
    stomics_module_score(client, "STD0001", sample_id = 1L),
    "Exactly one of gene_set_id or custom_genes must be provided"
  )
})

test_that("stomics_module_score raises error when both params provided", {
  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  expect_error(
    stomics_module_score(client, "STD0001", sample_id = 1L,
                          gene_set_id = 1L, custom_genes = c("MKI67")),
    "Provide gene_set_id or custom_genes, not both"
  )
})

test_that("stomics_module_score returns empty spots data.frame when no spots", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  score_payload <- jsonlite::toJSON(list(
    spots = list(),
    method = "mean_normalized",
    gene_set_name = "HALLMARK_INFLAMMATORY_RESPONSE",
    n_genes_total = 3L,
    n_genes_matched = 0L,
    matched_genes = list()
  ), auto_unbox = TRUE)

  webmockr::stub_request(
    "post",
    "https://api.example.com/api/v1/datasets/STD0001/samples/1/module-score"
  ) |>
    webmockr::to_return(
      body = score_payload,
      headers = list("Content-Type" = "application/json"),
      status = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_module_score(client, "STD0001", sample_id = 1L, gene_set_id = 1L)

  expect_s3_class(result$spots, "data.frame")
  expect_equal(nrow(result$spots), 0L)
  expect_named(result$spots, c("barcode", "x", "y", "score"))
  expect_equal(result$n_genes_matched, 0L)
})


# ---------------------------------------------------------------------------
# stomics_launch_to_platform (Feature 20)
# ---------------------------------------------------------------------------

test_that("stomics_launch_to_platform returns launch_url and platform for terra", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  payload <- jsonlite::toJSON(list(
    launch_url = "https://app.terra.bio/#import-data?url=https%3A%2F%2Fs3.example.com%2Fmanifest.json",
    platform   = "terra"
  ), auto_unbox = TRUE)

  webmockr::stub_request(
    "post",
    "https://api.example.com/api/v1/datasets/STD0001/launch-bundle"
  ) |>
    webmockr::to_return(
      body    = payload,
      headers = list("Content-Type" = "application/json"),
      status  = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_launch_to_platform(client, "STD0001", platform = "terra")

  expect_type(result$launch_url, "character")
  expect_type(result$platform,   "character")
  expect_equal(result$platform, "terra")
  expect_true(grepl("terra.bio", result$launch_url, fixed = TRUE))
})

test_that("stomics_launch_to_platform returns galaxy URL", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  payload <- jsonlite::toJSON(list(
    launch_url = "https://usegalaxy.eu/?tool_id=upload1&URL=https%3A%2F%2Fs3.example.com%2Ff.csv",
    platform   = "galaxy"
  ), auto_unbox = TRUE)

  webmockr::stub_request(
    "post",
    "https://api.example.com/api/v1/datasets/STD0001/launch-bundle"
  ) |>
    webmockr::to_return(
      body    = payload,
      headers = list("Content-Type" = "application/json"),
      status  = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_launch_to_platform(client, "STD0001", platform = "galaxy")
  expect_equal(result$platform, "galaxy")
  expect_true(grepl("usegalaxy.eu", result$launch_url, fixed = TRUE))
})

test_that("stomics_launch_to_platform passes file_types and workflow_id", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  payload <- jsonlite::toJSON(list(
    launch_url = "https://cloud.seqera.io/runs/run-abc",
    platform   = "seqera"
  ), auto_unbox = TRUE)

  webmockr::stub_request(
    "post",
    "https://api.example.com/api/v1/datasets/STD0001/launch-bundle"
  ) |>
    webmockr::to_return(
      body    = payload,
      headers = list("Content-Type" = "application/json"),
      status  = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_launch_to_platform(
    client, "STD0001",
    platform    = "seqera",
    file_types  = c("expression_matrix"),
    workflow_id = "wf-999"
  )
  expect_equal(result$platform, "seqera")
  expect_true(grepl("seqera.io", result$launch_url, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# stomics_download_aria2c_manifest (Feature 20)
# ---------------------------------------------------------------------------

test_that("stomics_download_aria2c_manifest writes file and returns dest_path", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  aria2c_content <- paste0(
    "https://s3.example.com/matrix.csv?sig=abc\n",
    "  out=matrix.csv"
  )

  webmockr::stub_request(
    "get",
    "https://api.example.com/api/v1/datasets/STD0001/aria2c-manifest"
  ) |>
    webmockr::to_return(
      body    = aria2c_content,
      headers = list("Content-Type" = "text/plain"),
      status  = 200
    )

  tmp_file <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp_file), add = TRUE)

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  returned <- stomics_download_aria2c_manifest(client, "STD0001", dest_path = tmp_file)

  expect_equal(returned, tmp_file)
  expect_true(file.exists(tmp_file))
  content <- readLines(tmp_file, warn = FALSE)
  expect_true(any(grepl("matrix.csv", content)))
})

test_that("stomics_download_aria2c_manifest uses default dest_path", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit({
    webmockr::disable()
    unlink("STD0001-aria2c.txt", force = TRUE)
  }, add = TRUE)

  webmockr::stub_request(
    "get",
    "https://api.example.com/api/v1/datasets/STD0001/aria2c-manifest"
  ) |>
    webmockr::to_return(
      body    = "https://s3.example.com/f.csv\n  out=f.csv",
      headers = list("Content-Type" = "text/plain"),
      status  = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  stomics_download_aria2c_manifest(client, "STD0001")
  expect_true(file.exists("STD0001-aria2c.txt"))
})

# ---------------------------------------------------------------------------
# Gene-Centric Cross-Dataset Expression Search (Feature 26)
# ---------------------------------------------------------------------------

test_that("stomics_search_by_gene returns items data.frame with correct columns", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  response_body <- list(
    items = list(
      list(
        dataset_accession   = "STD0001",
        dataset_title       = "Brain Atlas",
        sample_id           = 1L,
        organism            = "Homo sapiens",
        tissue              = "brain cortex",
        technology          = "10x Visium",
        mean_expr           = 4.82,
        pct_spots_expressed = 0.91,
        max_expr            = 12.3,
        thumbnail_url       = NULL
      )
    ),
    total    = 1L,
    page     = 1L,
    per_page = 20L
  )

  webmockr::stub_request(
    "get",
    uri_regex = "https://api.example.com/api/v1/search/genes.*"
  ) |>
    webmockr::to_return(
      body    = jsonlite::toJSON(response_body, auto_unbox = TRUE),
      headers = list("Content-Type" = "application/json"),
      status  = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_search_by_gene(client, "GAPDH")

  expect_equal(result$total, 1L)
  expect_equal(result$page, 1L)
  expect_equal(result$per_page, 20L)
  expect_s3_class(result$items, "data.frame")
  expect_equal(nrow(result$items), 1L)
  expect_equal(result$items$dataset_accession, "STD0001")
  expect_equal(result$items$dataset_title, "Brain Atlas")
  expect_equal(result$items$sample_id, 1L)
  expect_equal(result$items$organism, "Homo sapiens")
  expect_equal(result$items$tissue, "brain cortex")
  expect_equal(result$items$technology, "10x Visium")
  expect_equal(result$items$mean_expr, 4.82)
  expect_equal(result$items$pct_spots_expressed, 0.91)
  expect_equal(result$items$max_expr, 12.3)
})

test_that("stomics_search_by_gene returns empty data.frame when no results", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  response_body <- list(items = list(), total = 0L, page = 1L, per_page = 20L)

  webmockr::stub_request(
    "get",
    uri_regex = "https://api.example.com/api/v1/search/genes.*"
  ) |>
    webmockr::to_return(
      body    = jsonlite::toJSON(response_body, auto_unbox = TRUE),
      headers = list("Content-Type" = "application/json"),
      status  = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_search_by_gene(client, "NONEXISTENT")

  expect_equal(result$total, 0L)
  expect_s3_class(result$items, "data.frame")
  expect_equal(nrow(result$items), 0L)
  expect_equal(colnames(result$items),
               c("dataset_accession", "dataset_title", "sample_id",
                 "organism", "tissue", "technology",
                 "mean_expr", "pct_spots_expressed", "max_expr"))
})

test_that("stomics_search_by_gene sends q and default params", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  empty_resp <- list(items = list(), total = 0L, page = 1L, per_page = 20L)

  webmockr::stub_request(
    "get",
    uri_regex = "https://api.example.com/api/v1/search/genes.*"
  ) |>
    webmockr::to_return(
      body    = jsonlite::toJSON(empty_resp, auto_unbox = TRUE),
      headers = list("Content-Type" = "application/json"),
      status  = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_search_by_gene(client, "GAPDH")

  # Verify call succeeded and returned correct structure
  expect_equal(result$total, 0L)
  expect_s3_class(result$items, "data.frame")
  expect_equal(nrow(result$items), 0L)
})

test_that("stomics_search_by_gene handles null organism/tissue/technology", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  response_body <- list(
    items = list(
      list(
        dataset_accession   = "STD0002",
        dataset_title       = "Test",
        sample_id           = 1L,
        organism            = NULL,
        tissue              = NULL,
        technology          = NULL,
        mean_expr           = 1.0,
        pct_spots_expressed = 0.2,
        max_expr            = 3.0,
        thumbnail_url       = NULL
      )
    ),
    total = 1L, page = 1L, per_page = 20L
  )

  webmockr::stub_request(
    "get",
    uri_regex = "https://api.example.com/api/v1/search/genes.*"
  ) |>
    webmockr::to_return(
      body    = jsonlite::toJSON(response_body, auto_unbox = TRUE),
      headers = list("Content-Type" = "application/json"),
      status  = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_search_by_gene(client, "GAPDH")

  expect_true(is.na(result$items$organism))
  expect_true(is.na(result$items$tissue))
  expect_true(is.na(result$items$technology))
})

test_that("stomics_gene_autocomplete returns data.frame with gene_symbol and dataset_count", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  autocomplete_response <- list(
    list(gene_symbol = "GAPDH", dataset_count = 5L),
    list(gene_symbol = "GATA3", dataset_count = 2L)
  )

  webmockr::stub_request(
    "get",
    uri_regex = "https://api.example.com/api/v1/search/genes/autocomplete.*"
  ) |>
    webmockr::to_return(
      body    = jsonlite::toJSON(autocomplete_response, auto_unbox = TRUE),
      headers = list("Content-Type" = "application/json"),
      status  = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_gene_autocomplete(client, "GA")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_equal(result$gene_symbol[1], "GAPDH")
  expect_equal(result$dataset_count[1], 5L)
  expect_equal(result$gene_symbol[2], "GATA3")
  expect_equal(result$dataset_count[2], 2L)
})

test_that("stomics_gene_autocomplete returns empty data.frame when no suggestions", {
  skip_if_not_installed("webmockr")
  webmockr::enable(adapter = "httr2")
  on.exit(webmockr::disable(), add = TRUE)

  webmockr::stub_request(
    "get",
    uri_regex = "https://api.example.com/api/v1/search/genes/autocomplete.*"
  ) |>
    webmockr::to_return(
      body    = "[]",
      headers = list("Content-Type" = "application/json"),
      status  = 200
    )

  client <- stomics_client(api_key = "sk-key", base_url = "https://api.example.com")
  result <- stomics_gene_autocomplete(client, "XYZ")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_equal(colnames(result), c("gene_symbol", "dataset_count"))
})
