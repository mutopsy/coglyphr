## Test environments
* Local: Windows 11, R 4.4.3
* win-builder: R-release
* win-builder: R-devel

## R CMD check results
0 errors | 0 warnings | 0 notes

## Changes in this update
* Refactored image I/O to make the imager package optional rather than mandatory.
* Declared optional image-format dependencies (png, jpeg, tiff, bmp) in Suggests.
* Ensured all examples run without optional dependencies installed.

## Downstream dependencies
* None.
