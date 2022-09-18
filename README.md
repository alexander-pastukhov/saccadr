# saccadr
**saccadr** is a modular and extendable R package to extract (micro)saccades from gaze samples via an ensemble of methods approach.

Although there is an agreement about a general definition of a [saccade](https://en.wikipedia.org/wiki/Saccade), the more specific details are harder to agree upon. Therefore, there are numerous algorithms that extract saccades based on various heuristics, which differ in the assumptions about velocity, acceleration, etc. The package uses these methods to label individual samples and then applies a majority vote approach to identify saccades. The package includes three methods (see _Implemented Methods_ vignette) but can be extended via custom methods (see _Using custom methods_ vignette). It also uses a modular approach to compute velocity and acceleration from noisy samples (see _Velocity computation_ vignette). Finally, you can obtain methods votes per gaze sample instead of saccades (see _Using sample votes_ vignette).

The `extract_saccades()` function uses several methods to label individual samples as belonging to a saccade, classifies a sample as a potential saccade if its proportion of votes exceeds a preset threshold, and then identifies saccades based on minimal saccade duration and minimal time between the saccades. For binocular data, samples can be averaged _before_ velocity computation, votes can be merged so that methods returns binocular saccades, or saccades are extracted for each eye separately.

Currently, the library implements saccade detection using on the following saccade detection methods. When using this package, please cite both the package and individual methods.

* `method_ek` : Engbert, R., & Kliegl, R. (2003). Microsaccades uncover the orientation of covert attention. Vision Research, 43(9), 1035–1045. https://doi.org/10.1016/S0042-6989(03)00084-1
* `method_om` : Otero-Millan, J., Castro, J. L. A., Macknik, S. L., & Martinez-Conde, S. (2014). Unsupervised clustering method to detect microsaccades. Journal of Vision, 14(2), 18–18. https://doi.org/10.1167/14.2.18
* `method_nh` : Nyström, M., & Holmqvist, K. (2010). An adaptive algorithm for fixation, saccade, and glissade detection in eyetracking data. Behavior Research Methods, 42(1), 188–204. https://doi.org/10.3758/BRM.42.1.188

## Installation
To install from github

```r
library("devtools")
install_github("alexander-pastukhov/saccadr", dependencies=TRUE)
```

## Usage
The main function is `extract_saccades()`. Minimally, it  takes x and y gaze samples, and sampling rate returning a table with extracted saccades. Note that the function expects that units of the gaze samples are \strong{degrees of visual angle}, as some methods use physiologically plausible velocity and acceleration thresholds.
```r
data("single_trial")
saccades <- extract_saccades(single_trial$x, single_trial$y, sample_rate = 500)
```


### Multiple trials
When the recording spans multiple trials, you need to specify this via `trial` parameter. This way velocity computation and saccade detection methods respect trial boundaries.

```r
data(monocular_ten_trials)
saccades <- extract_saccades(monocular_ten_trials$x
                             monocular_ten_trials$y, 
                             500,
                             trial = monocular_ten_trials$trial)
```

### Binocular data

There are three ways in which binocular data can be treated based on the value of `binocular` parameter:

* `binocular = "merge"` (default): sample votes are obtained from both eyes and for all methods and then averaged. This way only binocular saccades (i.e., eye movements with a sufficient temporal overlap between eyes) are detected. `Eye = "Binocular"` in saccade description.
* `binocular = "cyclopean"` : binocular data is converted to an average cyclopean image before voting and saccades detection. `Eye = "Cyclopean"` in saccade description.
* `binocular = "monocular"` : saccades are extracted independently for each eye. `Eye = "Left"` or `Eye = "Right"` in saccade description.

```r
data("single_trial_binocular")
# binocular saccades only
saccades_b <- saccadr::extract_saccades(single_trial_binocular[, c('xL', 'xR')],
                                      single_trial_binocular[, c('yL', 'yR')],
                                      sample_rate = 1000)

# cyclopean saccades from binocular data
saccades_c <- saccadr::extract_saccades(single_trial_binocular[, c('xL', 'xR')],
                                      single_trial_binocular[, c('yL', 'yR')],
                                      sample_rate = 1000,
                                      binocular = "cyclopean")

# monocular saccades from binocular data
saccades_m <- saccadr::extract_saccades(single_trial_binocular[, c('xL', 'xR')],
                                      single_trial_binocular[, c('yL', 'yR')],
                                      sample_rate = 1000,
                                      binocular = "monocular")
```

### Specifying methods
By default, all implemented methods are used for saccade detection but, if necessary, you can use their subset or even a single method. Note that you can also supply your own saccade detection function, please see _Using custom methods_ vignette.

```r
# Using a single method
saccades <- extract_saccades(single_trial$x, single_trial$y, 500, methods = method_om)

# Using two methods
saccades <- extract_saccades(single_trial$x, single_trial$y, 500, methods = list(method_ek, method_om))
```

Individual methods have their own parameters that are passed via `options` argument, which is a named list with `<parameter-name> = <value>` pairs. You can find information on specific parameters and their default values in _Implemented Methods_ vignette. Here is an example of modifying a velocity threshold, measured in units of standard deviation, for Engbert & Kliegl (2003) method. The default value is 6 but we can make it stricter

```r
saccades <- extract_saccades(single_trial$x, single_trial$y, 500, options = list("ek_velocity_threshold" = 8))
```

### Altering voting threshold
The voting threshold is the number of methods that must label a sample as a potential saccade. By default, all but one method must agree for a sample to be considered for a saccade (`vote_threshold = length(methods) - 1`) but is 1, if only a single method was passed to the function. You can make voting more or less restrictive via `vote_threshold` parameter.

```r
# A strict unanimous decision threshold
saccades <- extract_saccades(single_trial$x, single_trial$y, 500, vote_threshold = 3)

# A slacker criterion that at least one of the three methods must label sample as a saccade
saccades <- extract_saccades(single_trial$x, single_trial$y, 500, vote_threshold = 1)
```

### Specifying velocity computation method
Because the gaze samples tend to be noisy, different methods use various approaches for computing velocity from noisy samples. Methods by Engbert & Kliegl (2003) and  Otero-Millan et al. (2014) used the same approach based on averaging over multiple samples to compute velocity, whereas Nyström & Holmqvist (2010) compute a simple derivative and then filter it. By default, package uses the former approach (`velocity_function = diff_ek`) but you can also use the latter (`velocity_function = diff_nh`) or implement a custom method (see _Velocity computation_ vignette). Acceleration is computed the same way but from velocity samples. Here is an example of using Nyström & Holmqvist (2010) velocity computation

```r
saccades <- extract_saccades(single_trial$x, single_trial$y, 500, velocity_function = diff_nh)
```

### Specifying saccade temporal properties
Once the votes are in, saccades detection is based on their minimal duration (`minimal_duration_ms` parameter, defaults to 12 ms) and minimal time between the saccades (`minimal_separation_ms`, defaults to 12 ms).

```r
# Only longish saccades are extracted
saccades <- extract_saccades(single_trial$x, single_trial$y, 500, minimal_duration_ms = 20)
```

## Return values
### Saccade description table
The `extract_saccades()` function returns a table with following columns:

* `Trial` Trial index.
* `Eye` "Monocular" for monocular inputs. "Cyclopean" for binocular data that was averaged before applying algorithms. "Binocular" for binocular data with votes averaged after applying algorithms. "Left" or "Right" for binocular data when eyes are processed independently.
* `OnsetSample` Index of the first sample.
* `OffsetSample` Index of the last sample.
* `Onset` Onset time relative to the trial start in milliseconds.
* `Offset` Offset time relative to the trial start in milliseconds.
* `Duration` Duration in milliseconds.
* `DisplacementX` Horizontal displacement measured from the first to the last sample.
* `DisplacementY` Vertical displacement measured from the first to the last sample.
* `Displacement` Displacement magnitude measured from the first to the last sample.
* `DisplacementPhi` Displacement direction measured from the first to the last sample.
* `AmplitudeX` Horizontal displacement measured from the leftmost to the rightmost sample.
* `AmplitudeY` Vertical displacement measured from the lowest to the uppermost sample.
* `Amplitude` Displacement magnitude measured from the most extreme samples.
* `AmplitudePhi` Displacement direction measured from the most extreme samples.
* `VelocityPeak` Peak velocity.
* `VelocityAvg` Average velocity.
* `AccelerationPeak` Peak acceleration.
* `AccelerationAvg` Average acceleration.
* `AccelerationStart` Peak acceleration before peak velocity was reached.
* `AccelerationStop` Peak acceleration after peak velocity was reached.

### Sample votes
Alternatively, if you use parameter `return_votes = TRUE` the function can return votes per sample and method (and eye, for binocular data). Please see _Using sample votes_ vignette for details.

