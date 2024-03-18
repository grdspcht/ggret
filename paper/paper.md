---
# (This front matter is required by JOSS)
# (See here for formatting & submission guidelines: https://joss.readthedocs.io/en/latest/submitting.html#how-should-my-paper-be-formatted)
title: "`ggarg`: Visualisation of tree‑based phylogenetic networks"

tags:
  - R
  - phylogenomics
  - visualisation
  - ancestral recombination graph
  - arg
  - phylogenetic network

authors:
  - name: Gerd Specht
    affiliation: "1,2"
  - name: Arthur Kocher
    orcid: 0000-0002-9499-6472
    affiliation: "2,3"
  - name: Clemens Schmid
    orcid: 0000-0003-3448-571
    affiliation: 2
  - name: Denise Kühnert
    orcid: 0000-0002-5657-018X
    corresponding: true
    affiliation: "2,3,4"


affiliations:
 
 - name: Faculty of Mathematics and Computer Science, Friedrich Schiller University Jena, Germany
   index: 1
 - name: Transmission, Infection, Diversification & Evolution Group, Max Planck Institute of Geoanthropology, Jena, Germany
   index: 2
 - name: Department of Archogenetics, Max Planck Institute for Evolutionary Anthropology, Leipzig, Germany
   index: 3
 - name: Department of Phylogenomics, Robert Koch Institute, Wildau, Germany
   index: 4

date: 1 March 2024
bibliography: paper.bib
---

<!---
- 250-1000 words

- A summary describing the high-level functionality and purpose of the software for a diverse, non-specialist audience.

- A Statement of need section that clearly illustrates the research purpose of the software and places it in the context of related work.

- A list of key references, including to other software addressing related needs. Note that the references should include full names of venues, e.g., journals and conferences, not abbreviations only understood in the context of a specific discipline.

- Mention (if applicable) a representative set of past or ongoing research projects using the software and recent scholarly publications enabled by it.

- Acknowledgement of any financial support.
-->

# Summary

Evolutionary history is most often visualised in the form of a bifurcating phylogenetic tree. Trees are models for a series of vertical evolutionary events and are constructed from branches or edges which describe a flow of genetic material from direct ancestor to offspring and nodes or vertices which represent ancestral states if they are internal nodes and extant species or individuals if they are external nodes (more commonly called tips or leaves) [@yang_phylogeny_2014]. While this mode of representation offers much insight into evolutionary processes and relationships it lacks the ability to visualise evolutionary events that do not fit this vertical model. So-called horizontal evolutionary events break with the common concept that genetic material is only transferred from ancestor to offspring and have gained scientific interest in the last decades. Advances in sequencing and computational methods have made it possible to identify horizontal evolutionary events and helped to reveal a more complex image of evolutionary history. Due to horizontal or reticulate evolution it may be in many instances preferable to represent a group's evolutionary history as a phylogenetic network which requires an efficient visualisation tool that can be integrated into automated workflows.


# Statement of need
`ggarg` is an R package for the visualisation of tree-based phylogenetic networks (i.e. phylogenetic trees with additional horizontal edges). The R language is commonly used for phylogenetic analysis and visualisation with packages such as `ape` and `ggtree` providing important functionalities for phylogenetic research [@paradis_ape_2019; @yu_ggtree_2017]. `ggarg` aims to extend these functionalities by building upon `ggtree's` extensive visualisation capabilites and expanding them with methods for handling tree-based phylogenetic networks while maintaining compatibility between the packages. 

# Usage
`ggarg` is available on [GitHub](https://github.com/grdspcht/ggarg) and can be installed by using `devtools` `install_github` function.

```r
# Install from GitHub
install.packages('devtools') # this may take a minute
library(devtools)
install_github('grdspcht/ggarg')
library(ggarg)
```


`ggarg` provides functions for reading extended Newick and (Beast2) NEXUS format. This allows the user to visualise phylogenetic networks inferred from various programs. 

For the sake of reproducibility we showcase some of the visualisation features with the `retnet` object that comes with `ggarg`.

```r
p <- ggplot(retnet) + geom_arg() + theme_tree()
plot(p)
```
This creates a rudimentary tree-based network without any labelling. Reticulate edges are drawn as dashed lines. 

![A rudimentary tree-based network plotted with `ggarg`\label{fig:arg1}](rudarg.pdf){width=66%}

Annotations can be easily added by appending them to the previous plot object.

```r
# Adding tip and node labels
p <- geom_tiplab() + geom_nodelab(aes(vjust=1))
plot(p)
```

![An explicit phylogenetic network with added tip labels
(A-D) and node labels (1-8). Note that node labels for reticulation edges (in dashed
lines) include a ”#” per extended Newick definition.
\label{fig:arg2}](labels.pdf){width=66%}

As a last step, we visually distinguish the clade of tips A and B by defining a list and
passing it to `ggtree's` `groupOTU()` which applies group information to the network object.

```r
groups <- list(AB=c("A", "B"))
tr <- groupOTU(retnet, groups, "Groups")
pg <- (ggplot(tr, aes(colour=Groups)) + geom_arg()
+ theme_tree() + geom_tiplab()
+ geom_nodelab(aes(vjust=1)))

plot(pg)
```

![Phylogenetic network with added branch colours based on clade information.
\label{fig:arg3}](colored.pdf){width=66%}


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

<!---
# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }
-->

# Acknowledgements

# References
<!--- Auto-generated, no need to add something here -->
