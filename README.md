# ggret

<img align="right" width="113" height="130" src="man/figures/ggret_logo.png">
<!-- badges: start -->
<a style="border-width:0" href="https://doi.org/10.21105/joss.07773">
  <img src="https://joss.theoj.org/papers/10.21105/joss.07773/status.svg" alt="DOI badge" >
</a>
<!-- badges: end -->

ggret is an R package for the visualization and annotation of Ancestral
Recombination Graphs (ARGs) and other tree-based phylogenetic networks.
It extends the functionality of ggtree and ggplot2.

## Installation

`ggret` requires `ggplot2`, `ggtree` and `ape` to function.

``` r
#install ggplot2
install.packages("ggplot2")

#install ggtree
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("ggtree")

#install ggret
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::install_github("grdspcht/ggret", dependencies = TRUE, build_vignettes = TRUE)
```

## Documentation
### In the Web
A website version of the documentation, containing function references and an in-depth vignette about customizing phylogenetic networks (see 'Articles') can be found on: https://grdspcht.github.io/ggret/



### In R

Function documentation can be accessed by typing `?function_name`.
Additionally, you can find multiple examples on how to use `ggret`
effectively in the vignette. To access it call:

``` r
vignette(topic = "intro_to_ggret", package = "ggret")
```


## Contributing & Reporting bugs
Make sure to check out our Contribution guidelines (see `CONTRIBUTING.md`) for instructions on how to report bugs, suggest improvements or contributing new features to ggret.

## Citing ggret

If you use `ggret`, please cite the associated publication as well as
the original `ggtree` and `ape` publication:

1.  **Specht G**, Schmid C, Kühnert D, Kocher A (2025). ggret: An R package for visualising and manipulating tree‑based phylogenetic networks. Journal of Open Source Software, 10(110), 7773, https://doi.org/10.21105/joss.07773
2.  **G Yu**, DK Smith, H Zhu, Y Guan, TTY Lam<sup>\*</sup>. “ggtree: an
    R package for visualization and annotation of phylogenetic trees
    with their covariates and other associated data”. ***Methods in
    Ecology and Evolution***. 2017, 8(1):28-36. doi:
    [10.1111/2041-210X.12628](https://doi.org/10.1111/2041-210X.12628).
3.  **Paradis E, Schliep K** . “ape 5.0: an environment for modern
    phylogenetics and evolutionary analyses in R.” ***Bioinformatics***,
    2019, *35*, 526-528. doi:
    [10.1093/bioinformatics/bty633](URL:%20https://doi.org/10.1093/bioinformatics/bty633).
