# ultimatch - The Ultimate Matching Toolbox
**ultimatch** implements various score and distance based matching methods, i.e. Nearest Neighbor, Radius, Coarsened Exact, Percentile Rank and Mahalanobis, Euclidean, Haversine and Cosine Distance Matching. It implements an efficient method for distance based matching preventing inflationary increment of the runtime. Matched observations are marked individually allowing interactions between treated and counterfactuals. Different methods can be combined to improve the results and/or to impose external requirements on the matched. Among other control variables, it creates mandatory weights to provide balanced matching results, preventing distortions caused by skewed counterfactual candidate distributions, e.g. overabundance of candidates with the same score or within  the same coarsened group. It can be used to identify geographic neighborhood relations using the haversine formula based on latitude and longitude.

**syncmatch** is a wrapper for ultimatch to handle the intricacies of matching panel data especially when staggered treatment is involved. It synchronizes the cohorts of the treatment group with the control group.

**syncevent** conducts a cohort-sensitive fixed effects event study for staggered or concurrent treatment using panel data matched with syncmatch.

## Prerequisites
STATA

## Getting started
* Copy all \*.ado and \*.sthlp files into your ADO file directory (typically c:\ado).
* Call the respective help file within STATA.
* Copy the provided examples from the help document into do-files and run them.
* Adjust the templates in the examples to your needs until you you get the hang of it.

## Version history

2026.09.03
* Inital release of **syncmatch**
* Initial relase of **syncevent**
* New **perc** option allows to specify a percentile caliper applied after matching
* Reporting of t-tests is more convenient: **matched** & **unmatched** do not require specification of report variables
* Report option uses a separate <b>\*</b> character as shortcut for the matching variables, i.e., <b>report(score \* fitness)</b>
* System variable **_copy** is now 0 for matched original observations
* New help file

2025.04.09
* Fixed a bug where the **exp** and **limit** options do not work in very large datasets.

2025.02.20 (ssc repository version)
* Fixed a bug where omitting the draw parameter for score based matching would lead to ignoring the draw limit.

2024.12.02 (ssc repository version)
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
