---
title: 'saccadr: A Modular and Extendable R Package to Extract (Micro)Saccades from Gaze Samples via an Ensemble of Methods Approach'
tags:
- R
- eye tracking
- saccades
date: "18 September 2022"
output:
  word_document: default
  pdf_document: default
  html_document:
    df_print: paged
authors:
- name: Alexander Pastukhov
  orcid: 0000-0002-8738-8591
  affiliation: 1, 2
bibliography: paper.bib
affiliations:
- name: Department of General Psychology and Methodology, University of Bamberg, Bamberg,
    Bavaria, Germany.
  index: 1
- name: Forschungsgruppe EPÆG (Ergonomics, Psychological Æsthetics, Gestalt), Bamberg,
    Bavaria, Germany.
  index: 2
---

# Summary
The package uses an ensemble of methods approach to label individual samples and then applies a majority vote approach to identify saccades. It uses several methods to label individual samples as belonging to a saccade, classifies a sample as a potential saccade if its proportion of votes exceeds a preset threshold, and then identifies saccades based on minimal saccade duration and minimal time between the saccades. Currently, the library implements saccade detection using methods proposed in @EngbertKliegl2003, @Otero-Millan2014, and @NystromHolmqvist2010. For binocular data, 1) samples can be averaged before velocity computation, 2) votes can be merged so that function returns binocular saccades, or 3) saccades are extracted for each eye separately. The package can be extended via custom methods and it also uses a modular approach to compute velocity and acceleration from noisy samples with the possibility of using custom differentiation methods. Finally, you can obtain methods votes per gaze sample instead of saccades.

# Statement of Need
Currently, there are R packages, such as _emov_ by @emov and _saccades_ by @saccades that use algorithms by @SalvucciGoldberg2000 and @EngbertKliegl2003, that implement saccade detection. However, although there is an agreement about a general definition of a saccade, the more specific details vary between researchers. Therefore, there exist numerous algorithms that extract saccades based on various heuristics, which differ in the assumptions about velocity, acceleration, etc. An ensemble approach addresses this issue by combining votes from various methods making saccade detection less dependent on specific assumptions. It also allows the user to set how liberal is the criterion for saccade detection, i.e., how many methods need to agree for a sample to be marked as a potential saccade. In addition, it will make studies easier to compare as it allows to use of individual methods separately to compare results within and across studies.

# References

