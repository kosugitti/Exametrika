
# Exametrika

<!-- badges: start -->
<!-- badges: end -->

Exametrika package is for Test Data Engineering and allows for various analyses such as 1PL, 2PL, 3PL, 4PL-IRT,Latent Rank Analysis, Biclustering, and Bayesian Network.

Shojima, Kojiro (2022) Test Data Engineering: Latent Rank Analysis, Biclustering, and Bayesian Network (Behaviormetrics: Quantitative Approaches to Human Behavior, 13),Springer,(URL)[https://amzn.to/42eNArd]

## Installation

You can install the development version of Exametrika from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kosugitti/Exametrika")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Exametrika)
## basic example code
```

## Exametrika Data Format
Exametrika conducts analysis on data matrices composed solely of 0 or 1. In this matrix, 0 represents an incorrect answer, and 1 indicates a correct answer.
For more details, refer to Shojima (2022) regarding the format.

### Data Matrix Format

- The data matrix must be in a matrix or data frame format.
- It's permissible to include NA as missing values.
- Specific values (for instance, -99) can be designated as missing values.
- Along with the data, you may also provide a missing value index matrix that discerns the presence or absence of missing values.

### Examinee ID Column
It is possible to incorporate a column of examinee IDs into the data matrix you provide. By default, the first column is presumed to be the examinee ID column.

### Item Weight Vector
Item weights can be specified by the item weight vector, w. If not given, all elements' weights are set to 1.

### Item label Vector
Data column names (colnames) are available as item labels. If not specified, a sequential number is assigned.

### Data Formatting Function
Before any analysis, the dataFormat function decomposes the provided data into an ID vector,Item label vector, data matrix U, missing value index matrix Z, and item weight vector w.