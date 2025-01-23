# ultimatch: counterfactual matching
**ultimatch** implements various score and distance based matching methods, i.e. Nearest Neighbor, Radius, Coarsened Exact, Percentile Rank and Mahalanobis, Euclidean, Haversine and Cosine Distance Matching. It implements an efficient method for distance based matching preventing inflationary increment of the runtime. Matched observations are marked individually allowing interactions between treated and counterfactuals. Different methods can be combined to improve the results and/or to impose external requirements on the matched. Among other control variables, it creates mandatory weights to provide balanced matching results, preventing distortions caused by skewed counterfactual candidate distributions, e.g. overabundance of candidates with the same score or within  the same coarsened group. It can be used to identify geographic neighborhood relations using the haversine formula based on latitude and longitude.

## Prerequisites
STATA

## Getting started
* Copy ultimatch.ado and ultimatch.sthlp into your ADO file directory (typically c:\ado).
* Call the help file within STATA: help ultimatch.
* Copy the provided examples from the help document into do-files and run them.
* Adjust the templates in the examples to your needs until you you get the hang of it.

## Version history

2024.02.12
* Better vantage point calculation using the principal direction of the data for distance based matching.
* Added cosine distance based matching.
* Added haversine distance based matching using geographical coordinates (latitude and longitude).

2021.02.02
* Distance-based matching now supports the **draw** option.
* Matching results can be reproduced with **set seed**.
* Fixed a bug in score-based matching regarding the combination of **copy** and **single**.

2020.05.19
* Small adjustments to the help file.

2020.03.06
* The reported means are displayed as rounded numbers (9th digit after the comma).

2020.01.22
* Optimized the calculation of the outside reference point for the hypersphere leeway algorithm.

2019.09.10
* Implemented Standardized Differences in Means (**SDM**) as additional similarity measurement.
* Non-Clustered standard errors are now robust.

2019.08.31
* Prevent the usage of the options **limit** and **exp** for **Coarsened Exact** matching.

2019.08.26
* Fixed a bug in Mahalanobis matching that caused partially greedy behavior in non-greedy mode.
* Included options euclid and mahalamobis to improve flexibility for distance matching.
* Included option full as sub-option to copy to enforce treated/counterfactual tuples.
* Included option radius to explicitly activate radius matching instead of the implicit activation before.
* Percentile Rank is now considered a general transformation instead of a separate matching method.
* ultimatch now supports radius matching for the distance-based matching method.
* Added an additional example.

2019.04.25 (ssc repository version)
* Initial version.

### Author
* **Thorsten Doherr** - [ZEW](https://www.zew.de/en/team/tdo/)
