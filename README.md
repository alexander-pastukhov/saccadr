# saccadr
A modular and extendable R package to extract (micro)saccades from gaze samples via an ensemble of methods approach. Although there is an agreement about a general definition of a [saccade](https://en.wikipedia.org/wiki/Saccade), the more specific details are harder to agree upon. Therefore, there are numerous algorithms that extract saccades based on various heuristics, which differ in the assumptions about velocity, acceleration, etc. The package uses these methods to label individual samples and then applies a majority vote approach to identify saccades. The package includes three methods (see _Implemented Methods_ vignette) but can be extended via custom methods (see _Using custom methods_ vignette). It also uses a modular approach to compute velocity and acceleration from noisy samples (see _Velocity computation_ vignette). Finally, you can obtain methods votes per gaze sample instead of saccades (see _Using sample votes_ vignette).

When using this package, please cite both the package and individual methods.

## Installation
To install from github

```r
library("devtools")
install_github("alexander-pastukhov/saccadr", dependencies=TRUE)
```

## Usage
The main function is `extract_saccades()`. Minimally, it  takes gaze samples, and sampling rate returning a table with extracted saccades.

```{r eval = FALSE}
data("single_trial")
saccades <- extract_saccades(single_trial$x, single_trial$y, sample_rate = 500)
```

, list of methods to use for voting as well as options for individual methods, optional trial index, etc. 
