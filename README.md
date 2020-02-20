
<!-- README.md is generated from README.Rmd. Please edit that file -->

# taxadbcache

<!-- badges: start -->

[![R build
status](https://github.com/boettiger-lab/taxadb-cache/workflows/R-CMD-check/badge.svg)](https://github.com/boettiger-lab/taxadb-cache/actions)
<!-- badges: end -->

`taxadbcache` provides methods used by `taxadb` in pre-processing
taxonomic name provider data. This is not intended as a general-use
package, these functions exist only to support a pipeline of creating
annual releases of the taxadb backend data. Providers may be added or
deprecated.

Additionally, caches of the processed data are stored through the
“releases” mechanism.
