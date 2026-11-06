# cmAnalysis News

## cmAnalysis 1.1.0

* Improved consistency between cluster assignments, silhouette scores and CMDS plots. Fixed cluster counts are now evaluated directly and the silhouette plot shows the requested solution.
* Refined input validation and handling of small datasets and cluster ranges. Unsorted items (missing or blank stack IDs) remain supported and contribute no co-occurrences.
* Statement selection now retains untestable statements with a warning and records test status, p-values and retention decisions in the `selectionDiagnostics` attribute.
* Network plots retain the existing cluster colours and extend the fixed palette to 25 colours, with a warning when colours must be reused. Networks without co-grouping connections now produce a clear message.
* `sorterMapping()` adjusts automatic cluster ranges to the number of sorters and supports groups containing a single sorter. Group validation is reported independently of `verbose`, and `graph = FALSE` avoids opening a graphics device.
* Card simulations now preserve the requested number of sorters when allocating attribute weights. `pCorrect` now represents the actual probability of a correct assignment: an error selects only an alternative category. This changes simulated results relative to earlier versions.
* `crossClusterMap()` now optionally aligns cluster labels between maps, including maps with different numbers of clusters. Alignment uses Dice similarity by default; raw overlap counts remain available with `similarity = "count"`. Plot data and the label mapping can be returned for inspection.
* Added `getSorterClusterMembership()` to extract sorter-group membership without repeating the analysis.

## cmAnalysis 1.0.3

* Improved `conceptMapping()` so the cluster range now adjusts automatically to the number of statements, with a message when this happens.

## cmAnalysis 1.0.2

* Fixed bug in `checkConceptMapData()`: The validation now correctly counts unique (sorterID, stackID) combinations instead of just unique stackID values. This ensures that multiple sorters can each create their own stacks with the same stackID number.

## cmAnalysis 1.0.1

* Added reference to the published paper:  
  Kampen, J.K., Hageman, J.A., Breuer, M., & Tobi, H. (2025).  
  *The validity of concept mapping: let's call a spade a spade.*  
  Qual Quant. <doi:10.1007/s11135-025-02351-z>
* Updated DESCRIPTION and main function documentation accordingly.

## cmAnalysis 1.0.0

* Initial CRAN release.
* Provides functions to process and visualize concept mapping data:
  - Calculation of similarity and co-occurrence matrices
  - Multidimensional scaling
  - Cluster analysis (Ward, k-means, hierarchical)
  - Visualization via point cluster maps, dendrograms, heatmaps, network plots
