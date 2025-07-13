# coglyphr: Compute Glyph Centroids from Image Data

## Overview

`coglyphr` is an R package for computing the center of gravity (COG) of character-like binary images using various methods. It is particularly useful for analyzing glyph structures in image-based research on cognition and perception.  

The current version supports the following COG computation methods:

* Stroke-based COG (`COG_stroke`)  
* Contour-based COG (`COG_contour`), based on the method proposed by Kotani and colleagues (2004, 2011)  
* Potential energy-based COG (`COG_potential`), based on the method proposed by Kotani and colleagues (2006)


## Installation  

You can install `coglyphr` from GitHub using `devtools`:  

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

# Install coglyphr
devtools::install_github("mutopsy/coglyphr")
```

## Dependencies  

This package requires:  
* R (>= 4.4.0)  
* `dplyr`  
* `tidyr`  
* `imager`  
* `sp`

## Usage

After installation, you can load this package using the `library` function.

```r
library(coglyphr)
```
## Version History

### v0.0.0.9000 (2025-07-13)   
- Developer version.

