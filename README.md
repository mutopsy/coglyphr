# coglyphr: Compute Glyph Centers of Gravity from Image Data

## Overview

`coglyphr` is an R package for computing the center of gravity (COG) of character-like binary images using three different methods. This package provides functions for estimating stroke-based, contour-based, and potential energy-based COG. It is useful for analyzing glyph structure in areas such as visual cognition research and font development.

The current version supports the following COG computation methods:

* Stroke-based COG (`cog_stroke`)  
* Contour-based COG (`cog_contour`), based on the method proposed by Kotani and colleagues (2004, 2011)  
* Potential energy-based COG (`cog_potential`), based on the method proposed by Kotani and colleagues (2006)

For details on each function, refer to [the function references](https://mutopsy.github.io/coglyphr/reference/).

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
* `imager`  
* `sp`

## Usage

After installation, you can load this package using the `library` function.

```r
library(coglyphr)
```
## Version History

See the [Changelog on pkgdown site](https://mutopsy.github.io/coglyphr/news/) for the full changelog.

## References

* Kotani, A. (2011). Contour-based evaluation method of center of gravity on characters and its application to font development. Memoirs of Shonan Institute of Technology, 45(1), 23–33. https://shonan-it.repo.nii.ac.jp/records/2000243
* Kotani, A., Asai, Y., Nakamura, Y., Otuka, M., Mituyama, Y., & Onoye, T. (2004). Contour-based evaluation method of center of gravity on “LCFONT.” IPSJ SIG Technical Report, 115, 63–70. https://ipsj.ixsq.nii.ac.jp/records/36793
* Kotani, A., Tanemura, Y., Mitsuyama, Y., Asai, Y., Nakamura, Y., & Onoye, T. (2006). Potential energy-based center of gravity evaluation of characters. The Journal of the Institute of Image Electronics Engineers of Japan, 35(4), 296–305. https://doi.org/10.11371/iieej.35.296
