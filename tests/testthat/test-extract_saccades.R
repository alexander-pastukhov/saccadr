test_that("extract_saccades does not work for mismatching x and y", {
  # monocular x and y: sizes do not match
  expect_error(extract_saccades(x = rnorm(n = 100), y = rnorm(n = 200), sample_rate = 250))
  
  # binocular x and monocular y
  expect_error(extract_saccades(x = matrix(rnorm(n = 400), ncol = 2),
                                y = rnorm(n = 200), 
                                sample_rate = 250))
  
  # binocular x and y: sizes do not match
  expect_error(extract_saccades(x = matrix(rnorm(n = 400), ncol = 2),
                                y = matrix(rnorm(n = 200), ncol = 2), 
                                sample_rate = 250))

  # too many columns
  expect_error(extract_saccades(x = matrix(rnorm(n = 400), ncol = 4),
                                y = matrix(rnorm(n = 400), ncol = 4), 
                                sample_rate = 250))
  
})


test_that("extract_saccades does not work for mismatching x, y and trial", {
  # monocular x and y: sizes do not match with trial
  expect_error(extract_saccades(x = rnorm(n = 100),
                                y = rnorm(n = 100),
                                sample_rate = 250,
                                trial = rep(1, 50)))
  
  # binocular x and y: sizes do not match
  expect_error(extract_saccades(x = matrix(rnorm(n = 400), ncol = 2),
                                y = matrix(rnorm(n = 400), ncol = 2),
                                sample_rate = 250,
                                trial = rep(1, 50)))
})


test_that("extract_saccades does not work for wrong method", {
  # string instead of a method handle
  expect_error(extract_saccades(x = rnorm(n = 100),
                                 y = rnorm(n = 100),
                                 sample_rate = 250,
                                 methods = "method_ek"))
  
  # empty list
  expect_error(extract_saccades(x = rnorm(n = 100),
                                 y = rnorm(n = 100),
                                 sample_rate = 250,
                                 methods = NULL))
  
})


test_that("extract_saccades does not work for wrong binocular option", {
  data("single_trial_binocular")
  
  # unknown option
  expect_error(extract_saccades(x = single_trial_binocular[, c("xL", "xR")],
                                y = single_trial_binocular[, c("yL", "yR")],
                                sample_rate = 1000,
                                binocular = "cyclopian"))
})


test_that("extract_saccades does not work for wrong sample_rate", {
  # wrong type
  expect_error(extract_saccades(x = rnorm(n = 100), y = rnorm(n = 100), sample_rate = "250"))
  
  # negative value
  expect_error(extract_saccades(x = rnorm(n = 100), y = rnorm(n = 100), sample_rate = -250))
  
  # multiple values
  expect_error(extract_saccades(x = rnorm(n = 100), y = rnorm(n = 100), sample_rate = c(250, 100)))
})

test_that("extract_saccades returns correct container", {
  data("single_trial")
  data("single_trial_binocular")
  
  # data.frame for a single trial
  expect_true(is.data.frame(extract_saccades(single_trial$x, single_trial$y, sample_rate = 250)))
  
  # matrix for votes on monocular data
  expect_true(is.matrix(extract_saccades(single_trial$x, single_trial$y, sample_rate = 250, return_votes = TRUE)))
  
  # list of matrices for votes on binocular data
  expect_true(is.list(extract_saccades(x = single_trial_binocular[, c("xL", "xR")],
                                       y = single_trial_binocular[, c("yL", "yR")],
                                       sample_rate = 1000,
                                       return_votes = TRUE)))

  # correct matrix for votes on monocular data
  methods_to_use <- list(method_ek, method_om, method_nh)
  monocular_votes <- extract_saccades(x = single_trial$x,
                                      y = single_trial$y,
                                      sample_rate = 250,
                                      methods = methods_to_use,
                                      return_votes = TRUE)
  expect_true(nrow(monocular_votes) == nrow(single_trial))
  expect_true(ncol(monocular_votes) == length(methods_to_use))
})

test_that("extract_saccades returns an empty table, if no saccades were found", {
  x <- c(213, 213, 209, 123, 203, 168, 128, 224, 229, 229, 244, 200, 
         92, 272, 284, 284, 284, 567, 567, 567, 887, 613, 613, 953, 624, 
         624, 928, 631, 631, 934, 659, 659, 916, 645, 645, 945, 649, 649, 
         818, 573, 573, 813, 626, 626, 787, 620, 620, 775, 596, 596, 795, 
         566, 566, 732, 603, 783, 803, 560, 812, 823, 591, 834, 835, 520, 
         832, 819, 569, 569, 830, 578, 578, 641, 386, 386, 591, 344, 344, 
         590, 352, 352, 623, 349, 672, 675, 405, 653, 650, 405, 664, 657, 
         373, 764, 765, 356, 520, 681, 369, 536, 682, 389, 705, 710, 427, 
         696, 821, 525, 830, 782, 547, 795, 796, 533, 790, 834, 544, 829, 
         822, 524, 829, 849, 534, 832, 810, 454, 454, 720, 314, 314, 584, 
         310, 310, 549, 323, 323, 538, 307, 307, 569, 319, 616, 609, 363, 
         363, 607, 341, 341, 786, 642, 642, 1027, 928, 928, 1270, 1006, 
         1006, 1206, 1014, 1254, 1133, 1012, 1259, 1291, 1026, 1297, 1292, 
         993, 1297, 1334, 1005, 1289, 1299, 1309, 1554, 1623, 1717, 1815, 
         1873, 1762, 1860, 1874, 1695, 1843, 1844, 1668, 1836, 1819, 1678, 
         1832, 2023, 1673, 1836, 1853, 1706, 1853, 1847, 1692, 1840, 1836, 
         1695, 1846, 1837, 1669, 1822, 1826, 1661, 1849, 1878, 1757, 1918, 
         1882, 1706, 1890, 1898, 1729, 1894, 1905, 1696, 1686, 1891, 1675, 
         1888, 1881, 1692, 1876, 1874, 1692, 1872, 1876, 1682, 1867, 1856, 
         1686, 1876, 1879, 1685, 1869, 1861, 1664, 1858, 1862, 1669, 1878, 
         1879, 1710, 1897, 1888, 1758, 1853, 1860, 1703, 1864, 1858, 1681, 
         1854, 1848, 1700, 1848, 1984, 1716, 1675, 1989, 1713, 1717, 1972, 
         1727, 1875, 1881, 1881, 1756, 1867, 1867, 1765, 1859, 1859, 1735, 
         1856, 1856, 1679, 1718, 1718, 1592, 1815, 1549, 1699, 1705, 1533, 
         1696, 1701, 1541)
   y <-
    c(54, 54, 52, -88, 48, -21, -76, 52, 71, 71, 109, 28, -67, 131, 
    120, 120, 120, 304, 304, 304, 501, 501, 501, 565, 483, 483, 573, 
    476, 476, 567, 434, 434, 560, 453, 453, 575, 419, 419, 406, 338, 
    338, 462, 284, 284, 466, 245, 245, 460, 253, 253, 455, 371, 371, 
    425, 330, 443, 451, 403, 454, 461, 265, 463, 464, 421, 469, 470, 
    265, 265, 470, 277, 277, 497, 503, 503, 692, 701, 701, 947, 1003, 
    1003, 1019, 956, 1053, 1074, 934, 1054, 1047, 877, 1048, 1052, 
    918, 1130, 1132, 1028, 1037, 1062, 985, 1017, 1053, 962, 1069, 
    1045, 921, 939, 847, 652, 814, 789, 670, 803, 810, 663, 807, 
    816, 653, 819, 829, 609, 824, 824, 619, 812, 806, 735, 735, 887, 
    865, 865, 1006, 950, 950, 986, 871, 871, 1016, 840, 840, 1009, 
    907, 1029, 1033, 903, 903, 1024, 924, 924, 927, 718, 718, 631, 
    483, 483, 519, 441, 441, 491, 376, 502, 407, 437, 437, 523, 417, 
    516, 515, 392, 511, 501, 439, 470, 469, 307, 362, 305, 280, 304, 
    311, 343, 350, 366, 417, 411, 414, 411, 425, 432, 443, 423, 411, 
    436, 436, 417, 430, 418, 427, 432, 431, 429, 424, 429, 425, 439, 
    434, 441, 450, 504, 598, 852, 833, 940, 963, 917, 927, 1008, 
    994, 994, 997, 996, 1000, 1002, 1000, 1010, 996, 1011, 1007, 
    973, 999, 998, 1003, 994, 987, 981, 985, 985, 995, 1014, 1017, 
    984, 997, 1011, 1001, 991, 912, 844, 722, 569, 583, 431, 372, 
    439, 392, 419, 432, 414, 420, 430, 402, 297, 380, 274, 272, 253, 
    230, 241, 214, 201, 196, 196, 237, 190, 190, 291, 203, 203, 274, 
    279, 279, 255, 160, 160, 216, 129, 229, 176, 155, 238, 141, 153, 
    207)
  expect_true(nrow(data.frame(extract_saccades(x, y, sample_rate = 120, methods = method_ek))) == 0)
  expect_true(nrow(data.frame(extract_saccades(x, y, sample_rate = 120, methods = method_om))) == 0)
  expect_warning(extract_saccades(x, y, sample_rate = 120, methods = method_nh))
  expect_warning(saccadr::extract_saccades(x, y, sample_rate = 120))
  expect_true(nrow(data.frame(extract_saccades(x, y, sample_rate = 120, methods = list(method_ek, method_om)))) == 0)
})


