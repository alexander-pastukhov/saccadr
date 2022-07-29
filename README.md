# saccadr
An R package to extract (micro)saccades from gaze samples via an ensemble of methods approach. Because there is no single agreed upon definition of a saccade, there are numerous algorithms that extract saccades based on various heuristics. As they differ in the assumptions (about velocity, acceleration, etc.) that they make, the package uses these methods to label individual samples and then applies a majority vote approach to identify saccades. The package includes three methods (see _Implemented Methods_ vignette) but can be extended via custom methods (see _Using custom methods_ vignette). When using this package, please cite both the package and individual methods.

## Installation
To install from github

```r
library("devtools")
install_github("alexander-pastukhov/saccadr", dependencies=TRUE)
```

## Usage
The main function is `extract_saccades()` that takes following arguments

* `x`, `y` vectors (for monocular data) or matrices / data.frames (for binocular data) with gaze samples in **degrees of visual angle**. The units are important as some methods use specific (physiologically plausible) velocity and acceleration thresholds.
* `sample_rate` in Hz. It is assumed to be common for the entire time series. If the time series contains chunks (trials) that were recorded using different acquisition rate (e.g., SR Research Eyelink allows to set different acquisition rate for each recording / trial), you would need to split the time series and analyse them separately.
* `trial` An optional vector with trial ID for each sample. If omitted, all samples are assumed to belong to a single trial. Velocity, acceleration, and saccades themselves are computed respecting trial borders.  
* `methods` A _list_ (not a vector!) with names of package methods (strings, see _Implemented Methods_ vignette) or _external functions_ (see _Using custom methods_ vignette) to label individual samples as belonging to a saccade. Package methods include Engbret & Kliegl (2003) (`"ek"`), Otero-Millan et al. (`"om"`), Nyström and Holmqvist (2010) (`"nh"`).
* `options` A named list with options for a specific method, _Implemented Methods_ vignette.
* `velocity_time_window` A time window in _milliseconds_ that is used for computing velocity and acceleration.
* `binocular` Specifies how a binocular data is treated. Options are `"cyclopean"` (binocular data is converted to an average cyclopean image before saccades are extracted), `"monocular"` (saccades are extracted independently for each eye), `"merge"` (default, methods votes on monocular data for each eye but the votes are averaged across eyes before saccades extracted).
`vote_threshold` Value between 0..1 defining a vote threshold for a saccade. By default, all but one method ($threshold = \frac{N-1}{N}$ where N is number of methods used) must agree for a sample to be considered for a saccade. Threshold of $1$ is used if a single method is used.
* `return_votes` An option to retun votes for each sample and method instead of saccades.

