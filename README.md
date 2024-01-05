generalbfxpkg v0.0.0
================
Artemio Sison III
2024-01-03

<!-- README.md is generated from README.Rmd. Please edit that file -->

# bfxpkg

<!-- badges: start -->

[![R-CMD-check](https://github.com/miosisoniii/generalbfxpkg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/miosisoniii/generalbfxpkg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `generalbfxpkg` is to streamline and optimize existing my Bioinformatics and clinical data workflows and establish consistency across code-writing for future teams I am working with.

## Installation

You can install the development version of bfxpkg from
[GitHub](https://github.com/) with:

``` r
## install devtools() package if you don't have it already
# install.packages("devtools")
## or you can install with remotes()
# install.packages("remotes")
```

This is a private repository, so you will need your **Personal Access
Token**, or PAT.

`TOKEN`

``` r
## install this development version from Github (we don't want bfxpkg on CRAN)
# devtools::install_github("miosisoniii/generalbfxpkg",
#                          auth_token = "TOKEN")
# remotes::install_github("miosisoniii/generalbfxpkg", 
#                         auth_token = "TOKEN")
```

Here is how is how I created a PAT for you to use when you install:

- Click on your profile picture in the top-right corner of the GitHub
  page.
- Click on *Developer Settings* at the bottom of the left sidebar
- Click on *Personal Access Tokens* in the left sidebar
- Click on “Generate New token” at the bottom of the page
- After generating the token, **Copy** it, and then paste the token into
  the `auth_token =` arg in the call to `install_github()` with
  `devtools` or `remotes`.
