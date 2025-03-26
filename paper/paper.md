---
# (This front matter is required by JOSS)
# (See here for formatting & submission guidelines: https://joss.readthedocs.io/en/latest/submitting.html#how-should-my-paper-be-formatted)
title: "`ggret`: An R package for visualising and manipulating tree‑based phylogenetic networks"

tags:
  - R
  - phylogenetic network
  - ancestral recombination graph
  - arg
  - phylogenetics
  - reticulated evolution
  - visualisation
  - plotting

authors:
  - name: Gerd Specht
    orcid: 0009-0001-3320-0557
    affiliation: "1,2"
    corresponding: true
  - name: Clemens Schmid
    orcid: 0000-0003-3448-5715
    affiliation: "3"
  - name: Denise Kühnert
    orcid: 0000-0002-5657-018X
    affiliation: "2,4"
  - name: Arthur Kocher
    orcid: 0000-0002-9499-6472
    affiliation: "2,3"
    corresponding: true
affiliations:
 
 - name: Faculty of Mathematics and Computer Science, Friedrich Schiller University Jena, Germany
   index: 1
 - name: Transmission, Infection, Diversification & Evolution Group, Max Planck Institute of Geoanthropology, Jena, Germany
   index: 2
 - name: Department of Archaeogenetics, Max Planck Institute for Evolutionary Anthropology, Leipzig, Germany
   index: 3
 - name: Centre for Artificial Intelligence in Public Health Research, Robert Koch Institute, Wildau, Germany
   index: 4

date: 21st October 2024
bibliography: paper.bib
---

```{=html}
<!---
- 250-1000 words

- A summary describing the high-level functionality and purpose of the software for a diverse, non-specialist audience.

- A statement of need section that clearly illustrates the research purpose of the software and places it in the context of related work.

- A list of key references, including to other software addressing related needs. Note that the references should include full names of venues, e.g., journals and conferences, not abbreviations only understood in the context of a specific discipline.

- Mention (if applicable) a representative set of past or ongoing research projects using the software and recent scholarly publications enabled by it.

- Acknowledgement of any financial support.
-->
```

# Summary

Evolutionary relationships of biological entities are most often modeled with phylogenetic trees. Phylogenetic trees consist of branches (or edges) representing direct lines of descent or genetic flow from ancestor to offspring (*i.e.* lineages), and nodes representing evolutionary "splits" through which a parental lineage gives rise to multiple child lineages. This vertical model of evolution has provided immense insights into the evolutionary history and processes underlying observed biological diversity. However, it fails to account for "horizontal" modes of evolution, whereby genetic material can be exchanged between contemporaneous organisms through a variety of mechanisms across the tree of life [@perez-losada_recombination_2015; @arnold2022horizontal; @keeling2024horizontal]. In recent years, advances in sequencing technologies and computational methods have made it increasingly possible to integrate horizontal evolutionary events into reticulated phylogenetic trees (or phylogenetic networks; [@huson_application_2006; @chen_hybridnet_2010; @vaughan2017inferring; @muller2020bayesian; @muller2022bayesian]). While phylogenetic networks have the potential to provide more comprehensive and accurate evolutionary pictures for many biological groups, the development of specific tools is required for their manipulation and visualisation. Here, we present `ggret`, an R package building upon the popular `ggtree` package [@yu_ggtree_2017] for the manipulation and visualisation of phylogenetic networks. `ggret` provides novel functions for parsing extended Newick and NEXUS files and introduces the `geom_ret` object for visualisation of tree-based phylogenetic networks.  

# Statement of need

The R language is commonly used for phylogenetic analysis and visualisation with packages such as `ape` and `ggtree` providing important functionalities for handling and visualizing classic phylogenetic trees [@yu_ggtree_2017; @paradis_ape_2019]. In contrast, R packages for the manipulation and plotting of phylogenetic networks are largely lacking. The `ape` and `phangorn` packages [@paradis_ape_2019; @schliep_phangorn_2011] provide basic plotting functions for explicit networks and split networks. These have been extended in the `tanggle` package to allow more flexibility using the `ggplot2` syntax [@wickham_ggplot2_2016], building upon `ggtree`. The `ggret` package presented here complements these existing tools by providing alternative modes of visualization of explicit phylogenetic networks as well as additional functionalities. In particular, `ggret` enables the parsing of phylogenetic networks in NEXUS formats such as those produced by the popular *BEAST2* software [@bouckaert2019beast], and can handle associated metadata via `treedata` objects, allowing to leverage the versatile `ggtree` annotation and visualisation methods. 

# Usage

`ggret` is available on [GitHub](https://github.com/grdspcht/ggret) and can be installed by using `remotes'` `install_github` function.

```r
# Install from GitHub
if (!requireNamespace("remotes", quietly=TRUE))
  install.packages("remotes")
remotes::install_github("grdspcht/ggret", dependencies = TRUE, 
                         build_vignettes = TRUE)
```

`ggret` provides functions for reading extended Newick and (*BEAST2*) NEXUS format [@cardona_extended_2008]. This allows the user to visualise phylogenetic networks inferred from various programs. Here we parsed a (summary) phylogenetic network simulated with the *BEAST2* package *Bacter* [@vaughan2017inferring] containing various node metadata, using the `read_beast_retnet` function. Note that the resulting retnet `treedata` object has been included in the package for the sake of reproducibility.

``` r
retfile <- system.file("extdata", "retnet.nexus", package = "ggret")
retnet <- ggret::read_beast_retnet(retfile)    
```

`ggret` is the central function of this package. In a simple call without additional arguments it plots a rudimentary tree-based network without labeling. Reticulated edges are drawn as black dashed lines by default, but `ggret` contains various arguments to change their aspects (e.g. colour, linetype, etc.) (\autoref{fig:arg1}).

``` r
library(ggret)

# Simple network
p1 <- ggret::ggret(retnet_treedata)
# Reticulation edges displayed as red dotted lines, in a "snake" shape
p2 <- ggret::ggret(retnet_treedata, retcol = "red", retlinetype = 3)
# Reticulation edges displayed as blue solid lines, in a straight shape and 
# with arrow heads
p3 <- ggret::ggret(retnet_treedata, retcol = "blue", retlinetype = 1, arrows = T,
                   rettype = "straight") 
```

![A simple tree-based networks plotted with `ggret` using different formats for reticulation edges. \label{fig:arg1}](rudarg.png){width="100%"}

We can rotate some of the nodes to avoid crossing of reticulation edges and improve visualisation using the `ggtree` `rotate` function. Note that one can initially visualise node indices using `ggret(retnet) + geom_nodelab(aes(label=node))` to make this easier (\autoref{fig:arg2}).

``` r
# Rotate nodes to improve vizualisation
ggtree::rotate(p1, node = 31) %>%
  ggtree::rotate(node = 30) %>%
  ggtree::rotate(node = 26) %>%
  ggtree::rotate(node = 37) ->
  p1
```


![An example network before rotation (left) and the same network plotted after node rotation (right) to limit the number of edge crossings.\label{fig:arg2}](rotate_compare.png){width="100%"}

Annotations can be added using `ggtree` functions, such as `geom_tiplab`, `geom_nodelab` and `geom_range`. In addition, a time axis may be added by using the `theme_tree2` theme (\autoref{fig:arg3}).

``` r
# Get the tMRCA of the tree and define time points to display
# the time axis in years BP
tmrca <- phytools::nodeHeights(retnet_treedata@phylo) %>% max
xticks_BP <- c(20000, 15000, 10000, 5000, 0)

# Add tip and node labels (we expand the x axis limits 
# so that tip labels still fit).
p1 <- p1 +
  ggtree::geom_tiplab() +
  ggtree::geom_nodelab(aes(label = round(posterior, 2)), 
          vjust = -0.25, hjust = 1.3, size = 3) +
  ggplot2::expand_limits(x = 22000)

# Adding a time axis and node bars indicating node heights' 95% 
# highest posterior density interval.
p1 <- p1 +
  ggtree::theme_tree2() +
  ggtree::geom_range(aes(range = "height_95_HPD"), color = "grey50", 
          alpha = .6, size = 1.5) +
  ggtree::scale_x_continuous(breaks = tmrca - xticks_BP, labels = xticks_BP) +
  ggplot2::xlab("Years before present")

```

![Annotated phylogenetic network. Tip labels are plotted and internal nodes are labelled with their posterior probability. The x-axis represents a timeline in years before present. The 95% highest probability density intervals of nodes' ages are indicated by grey bars. \label{fig:arg3}](labels.png){width="100%"}

The `group_clade` can be used to define clades within a network and color them accordingly. `group_clade` assigns clade information to all nodes descending from the MRCA of tips specified in the `nodes` argument (\autoref{fig:arg4}).

``` r
# Define clades using the groupClade function
retnet_treedata %>%
  ggret::group_clade(nodes = c("taxon_10", "taxon_20"), cladename = "A", 
         addtotreedata = T) %>%
  ggret::group_clade(nodes = c("taxon_11", "taxon_15"), cladename = "B", 
         addtotreedata = T) %>%
  ggret::group_clade(nodes = c("taxon_1", "taxon_16"), cladename = "C", 
         addtotreedata = T) ->
  retnet_clade

# Plot network with colored clades
ggret::ggret(retnet_clade, aes(color = clade))

```

![Phylogenetic network with colored based on clade information. Deep branches not belonging to any defined clade are labelled as NA. \label{fig:arg4}](colored.png){width="100%"}

For additional information, refer to `ggret's` internal documentation or to [https://github.com/grdspcht/ggret](https://github.com/grdspcht/ggret) which also gives users the opportunity to open issues, pull requests or report bugs. 

```{=html}
<!---
# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

-->
```
```{=html}
<!---
# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }
-->
```

# Acknowledgements

This work was funded by the Max Planck Society.

# References

<!--- Auto-generated, no need to add something here -->
