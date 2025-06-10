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

Evolutionary relationships of biological entities are most often modelled with phylogenetic trees. Phylogenetic trees consist of branches (or edges) representing direct lines of descent or genetic flow from ancestor to offspring (*i.e.* lineages), and nodes representing evolutionary "splits" through which a parental lineage gives rise to multiple child lineages. This vertical model of evolution has provided immense insights into the evolutionary history and processes underlying observed biological diversity. However, it fails to account for "horizontal" modes of evolution, whereby genetic material can be exchanged between contemporaneous organisms through a variety of mechanisms across the tree of life [@perez-losada_recombination_2015; @arnold2022horizontal; @keeling2024horizontal]. In recent years, advances in sequencing technologies and computational methods have made it increasingly possible to integrate horizontal evolutionary events into reticulated phylogenetic trees (or phylogenetic networks; [@huson_application_2006; @chen_hybridnet_2010; @vaughan2017inferring; @muller2020bayesian; @muller2022bayesian]). While phylogenetic networks have the potential to provide more comprehensive and accurate evolutionary pictures for many biological groups, the development of specific tools is required for their manipulation and visualisation. Here, we present `ggret`, an R package building upon the popular `ggtree` package [@yu_ggtree_2017] for the manipulation and visualisation of phylogenetic networks. `ggret` provides novel functions for parsing extended Newick and NEXUS files and introduces the `geom_ret` object for visualisation of tree-based phylogenetic networks.  

# Statement of need

The R language is commonly used for phylogenetic analysis and visualisation with packages such as `ape` and `ggtree` providing important functionalities for handling and visualising classic phylogenetic trees [@yu_ggtree_2017; @paradis_ape_2019]. In contrast, R packages for the manipulation and plotting of phylogenetic networks are largely lacking. The `ape` and `phangorn` packages [@paradis_ape_2019; @schliep_phangorn_2011] provide basic plotting functions for explicit networks and split networks. These have been extended in the `tanggle` package to allow more flexibility using the `ggplot2` syntax [@wickham_ggplot2_2016], building upon `ggtree`. The `ggret` package presented here complements these existing tools by providing alternative modes of visualisation of explicit phylogenetic networks as well as additional functionalities. In particular, `ggret` enables the parsing of phylogenetic networks in NEXUS formats such as those produced by the popular *BEAST2* software [@bouckaert2019beast], and can handle associated metadata via `treedata` objects, allowing to leverage the versatile `ggtree` annotation and visualisation methods. 

# Usage

`ggret` is available on [GitHub](https://github.com/grdspcht/ggret). All instructions for installation and usage can be found in the package vignette and [website](https://grdspcht.github.io/ggret/articles/intro_to_ggret.html).

Below are some examples of phylogenetic network visualisation with `ggret` (\autoref{fig:arg1}). The phylogenetic network was simulated with the *BEAST2* package *Bacter* [@vaughan2017inferring] and parsed together with node metadata using the `read_beast_retnet` function. The resulting retnet `treedata` object has been included in the package for the sake of reproducibility.

![Examples of phylogenetic network visualisation with *ggret*. **a.** Reticulation edges can be plotted in different shapes and colors. **b.** Annotated phylogenetic network. Tip labels are plotted and internal nodes are labelled with their posterior probability. The x-axis represents a timeline in years before present. The 95% highest probability density intervals of nodes’ ages are indicated by grey bars. **c.** Phylogenetic network with colored based on clade information. Deep branches not belonging to any defined clade are labelled as NA \label{fig:arg1}](combined_plot.png){width="100%"}


# Acknowledgements

This work was funded by the Max Planck Society.

# References

<!--- Auto-generated, no need to add something here -->
