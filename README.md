# saccadr
An R package to extract (micro)saccades from gaze samples via an ensemble of methods approach. Because there is no single agreed upon definition of a saccade, there are numerous algorithms that extract saccades based on various heuristics. As they differ in the assumptions (about velocity, acceleration, etc.) that they make, the package uses these methods to label individual samples and then applies a majority vote approach to identify saccades. The package includes three methods (see _Implemented Methods_ vignette) but can be extended via custom methods (see _Using custom methods_ vignette). When using this package, please cite both the package and individual methods.

## Installation
To install from github

```r
library("devtools")
install_github("alexander-pastukhov/saccadr", dependencies=TRUE)
```

## Usage
The main function is `extract_saccades()`. Minimally, it  takes gaze samples, sampling rate, list of methods to use for voting as well as options for individual methods, optional trial index, etc. 

```{r eval = FALSE}
data("single_trial")
saccades <- extract_saccades(single_trial$x, single_trial$y, sample_rate = 500)
```
