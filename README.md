# stomics

R client for [Stomics.io](https://stomics.io), a public repository of spatial transcriptomics
datasets. Search the catalogue, download files and load datasets into Seurat.

## Install

```r
install.packages("remotes")
remotes::install_github("ITGA3/stomics-r")
# a specific release
remotes::install_github("ITGA3/stomics-r@v0.1.1")
```

Seurat is optional and needed only for `stomics_load_seurat()`.

## Quick start

```r
library(stomics)

client <- stomics_client()  # reads STOMICS_API_KEY; public data needs no key
results <- stomics_search(client, organism = "Homo sapiens")
seurat_obj <- stomics_load_seurat(client, "STD0100")

# What the processed expression holds, then any per-spot field and raw counts
manifest <- stomics_expression_manifest(client, "STD0100")
clusters <- stomics_get_obs(client, "STD0100", "leiden")

# Clusters, markers and spatially variable genes from the stomics.io standard pipeline
run <- stomics_get_standard_analysis(client, "STD0100", sample_id = 12)
leiden <- stomics_get_obs(client, "STD0100", "standard:leiden_1.0", sample_id = 12)
markers <- stomics_get_markers(client, "STD0100", sample_id = 12, resolution = 1.0)
dp <- stomics_get_dotplot(client, "STD0100", sample_id = 12)
svgs <- stomics_get_spatially_variable_genes(client, "STD0100", sample_id = 12)
counts <- stomics_get_expression(client, "STD0100", gene = "GAPDH", layer = "counts")

# Files: filter by name, type or sample, or download them all (complete files are skipped on a rerun)
matrices <- stomics_list_files(client, "STD0716", q = "GSM5", file_types = "expression_matrix")
stomics_download_dataset(client, "STD0716", "STD0716")
```

Create an API key at <https://stomics.io/account/api-keys> for anything tied to your account. The
client talks to `https://api.stomics.io` unless you pass `base_url`.

## Documentation

- SDK reference (R tab): <https://stomics.io/docs/sdk>
- REST API: <https://api.stomics.io/api/v1/docs>
- Help Center: <https://stomics.io/help>

## Support

Report bugs in this repository's issues or write to support@stomics.io. Pull requests are welcome;
CI must pass, including the 100% test coverage gate.

## License

MIT. Copyright (c) 2026 The Allman Institute for Personalized Medicine. See [LICENSE.md](LICENSE.md).
