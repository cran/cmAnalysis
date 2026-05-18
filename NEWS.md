# cmAnalysis News

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
