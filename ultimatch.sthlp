{smcl}
{* 31aug2026}{...}
{hline}
help for {hi:ultimatch}
{hline}

{title:Title}

{p2colset 5 18 18 2}{...}
{p2col :{cmd:ultimatch} {hline 2}}Ultimate Matching Toolbox{break}
Keywords: nearest neighbor, radius, porpensity, distance, mahalanobis, haversine, cosine, rank, coarsened exact, hypersphere leeway{p_end}
{p2colreset}{...}

{marker syntax}{...}
{title:Syntax}

{p 8 15 2}{cmd:ultimatch} [{varlist}] [{it:if}] [{it:in}], {opth t:reated(varname)} [{opth exa:ct(varlist)}] [{opth d:raw(#)}] [{opth ca:liper(#)}] 
[{opth p:erc(#)}] [{opth earth:(#)}] [{opt su:pport}] [{opt si:ngle}] [{opt g:reedy}] [{opt b:etween}] [{opt ran:k}] [{opt rad:ius}] [{opt eu:clidean}]
[{opt m:ahalanobis}] [{opt hav:ersine}}][{opt cos:ine}}] [{opt co:py} [{opt f:ull}]] [{opth re:port(varlist)}] [{opt unm:atched}] [{opt mat:ched}]
[{opth uni:t(varlist)}] [{opth exp:(string)}] [{opth l:imit(string)}]{p_end}

{marker opt_summary}{...}
{synoptset 22 tabbed}{...}
{synopthdr}
{synoptline}
{marker matching}{...}
{syntab:Matching}
{synopt:{help varlist}}specifies the matching variables, which should be continuous or subtly graded ordinal variables:{break}
{hline 1} {cmd:one variable} initiates {hi:score-based} matching, i.e., propensity score matching{break}
{hline 1} {cmd:multiple variables} initiate {hi:distance-based} matching (see {help ultimatch##distance:mahlanobis}, {help ultimatch##distance:euclidean},
{help ultimatch##distance:haversine}, {help ultimatch##distance:cosine}){break}
{hline 1} {cmd:no variable} enforces {hi:coarsened exact} matching (see {help ultimatch##matching:exact}){p_end}
{synopt:{opth t:reated(varname)}}specifies a dummy variable marking the treated observations{p_end}
{synopt:{opth exa:ct(varlist)}}specifies a group of variables defining cells (stratums). The counterfactuals must be in the same cell as
the corresponding treated observation, therefore, the term {cmd:exact}. This option can be combined with any matching method. The
specified variables should be ordinal, categorical or binary. If this option is specified without a general {it:varlist} (a score or
distance variables), {hi:coarsened exact} matching is assumed. In this case, the {hi:_match} variable enumerates the cells containing
treated and non-treated observations in no specific order.{break}
{hi:Hint:} Coarsened exact matching can also be emulated by using a group variable based on the defined coarsened stratums as a score, which 
allows for the inclusion of missings (see {cmd:group} sub-command of {help egen}). By applying a {help ultimatch##restrictions:caliper} below 1, 
e.g. 0.5, the {hi:neighbor} matching will always draw counterfactuals within the stratum without requiring the {cmd:exact} option. All options of
{hi:neighbor} matching are available including {help ultimatch##general:single} for random assignment of counterfactuals and
{help ultimatch##general:copy} for direct associations with the treated observations (see: {help ultimatch##example_1:Example 1}).{p_end}

{marker general}{...}
{syntab:General}
{synopt:{opt co:py} [{opt f:ull}]}appends copies of counterfactuals that are drawn more than one time by different treated observations. It facilitates
direct associations of treated observations with their counterfactuals by the {help ultimatch##system_vars:_match} identifier enabling interactions between
a treated and non-treated observation, e.g. sample splits that do not separate treated and counterfactuals or the calculation of ratios. 
The option {help ultimatch##restrictions:perc} requires the specification of {cmd:copy} to remove treated observations without counterfatcuals.
With {cmd:copy}, every group of observations sharing the same {help ultimatch##system_vars:_match} identifier contains one treated and at least one
counterfactual. This is not necessarily the case, if the option is omitted. It is {hi:not} directly supported by {hi:coarsened exact} matching because it
matches groups not individuals. The sub-option {cmd:full} forces tuples comprising of exactly one treated and one counterfactual represented by a unique
{help ultimatch##system_vars:_match} ID. If a treated observation has more than one counterfactual, a copy of the treated will be created for every
additional counterfactual. This is the only case where the {help ultimatch##system_vars:_weight} variable may contain a weight different from 1 for
a treated observation to preserve the original distribution of the treated. The {cmd:full} option allows even more control over the interactions at
the expense of an inflated dataset. It is also useful for educational purposes (see: {help ultimatch##example_2:Example 2}).{p_end}
{synopt:{opt rad:ius}}activates radius matching for score-based and distance-based matching based on the {help ultimatch##restrictions:caliper}{p_end}
{synopt:{opth d:raw(#)}}specifies the number of neighbors for every treated observation to be drawn. Neighbors with the same score or
distance are considered one draw unless the option {help ultimatch##general:single} is specified. With this option, it is possible to diminish the burden
of the "nearest neighbor" by including a larger neighborhood at the expense of similarity. It is {hi:not} supported by {hi:coarsened exact} matching
because it always draws all observations in a cell defined by the option {cmd:exact}.{p_end}
{synopt:{opt si:ngle}}dismisses the default behavior of considering all observations with the same score or distance as one observation regarding the
draw limit. Every observation will be counted towards the draw limit. The counterfactuals are randomly drawn within groups of equal scores or
distances. It is {hi:not} supported by {help ultimatch##general:radius} and {hi:coarsened exact} matching as they are not restricted to a specific 
number of counterfactuals.{p_end}
{synopt:{opt g:reedy}}draws without replacement. The treated observation with the lowest distance will claim the non-treated observations. Treated
observations that were deprived of their counterfactuals are reactivated to search for alternative neighbors, potentially repressing other treated
observations. This may initiate a displacement cascade until all treated observations have settled with the best counterfactual they could possibly
claim given the competition. It is strongly advised to apply {help ultimatch##general:greedy} together with a reasonable
{help ultimatch##restrictions:caliper} and the {help ultimatch##general:single} setting. It is {hi:not} supported by {hi:coarsened exact} and
{help ultimatch##general:radius} matching.{p_end}
{synopt:{opt b:etween}}searches for higher and lower ranked neighbors independently. The {help ultimatch##general:draw} option limits both directions
separately. It is only supported by score-based matching.{p_end}
{synopt:{opt su:pport}}guarantees that there is overlap between the treated and non-treated population regarding the score, the so called
{it:common support}. This option enforces the creation of the {help ultimatch##system_vars:_support} variable marking observations with common support
with 1. The score has to be in the confines defined by the minimum of the maximum scores and the maximum of the minimum scores of treated vs. non-treated
observations. In the case of distance-based matching, the first variable is considered to contain the score to allow the inclusion of a propensity score.
It is {hi:not} supported by {hi:coarsened exact} matching.{p_end}
{synopt:{opt ran:k}}activates {hi:Percentile Rank} transformation of all matching variables in {help varlist}. In case of distance-based matching the
Euclidean distance will be used by default. This can be changed to {hi:Mahalanobis} distance with the option {help ultimatch##distance:mahalanobis}.
Rank transformation is incompatible with {help ultimatch##distance:haversine} and {help ultimatch##distance:cosine} distance matching.{p_end}

{marker distance}{...}
{syntab:Distance}
{synopt:{opt m:ahalanobis}}can be applied to switch to Mahalanobis distance calculation. This is the default setting for distance-based matching. This
option is {hi:not} supported by {hi:coarsened exact} and score-based matching.{p_end}
{synopt:{opt eu:clidean}}can be applied to switch to Euclidean distance calculation. This is the default setting in case
of percentile {help ultimatch##distance:rank} transformation. This option is {hi:not} supported by {hi:coarsened exact} and score-based matching.{p_end}
{synopt:{opt hav:ersine}}can be applied if the two variables of {it:varlist} describe geographic coordinates. The first variable has to denote the
{hi:latitude} in degrees between -90 and 90, while the second variable contains the {hi:longitude} between -180 and 180 degrees. Even though any values are
valid, you should heed this convention. The haversine distance is usually applied on geocoded data to identify the spatial neighborhood.{p_end}
{synopt:{opt earth:(#)}}defines the radius for the {hi:haversine} distance. The default radius is 6371 in kilometers. You can change it to
accommodate a different unit of measurement, i.e. miles or meter, or a different planet, i.e 1184 (Pluto) or 69173 (Jupiter). If you use a
{help ultimatch##restrictions:caliper}, adjust it accordingly.{p_end}
{synopt:{opt cos:ine}}distance is typically applied on variables describing a vector usually composed of probabilities or indicators in the same
continuous range, i.e., TF-IDF vectors. The cosine similarity is independent of the magnitude of the vectors as it only measures the cosine of the
angles between the vectors. Null vectors are excluded as they have no direction.{break}
The cosine {hi:distance} is defined in the range of [0,2]: 0 means congruency, 1 stands for orthogonality and 2 for the opposite direction. The cosine
{hi:similarity} (range [-1,1]) equals 1 - {help ultimatch##sytstem_vars:_distance}.{break}
{hi:Warning:} the variables in varlist are considered a row vectors, which will be normalized to a length of 1. Because these vectors are usually
very wide, the original values will be overwritten to save memory space. Their datatype will be changed to double. The length of the original
vector can be found in the variable {hi:_length}. The original values can be restored by multiplication with _length.{p_end}

{marker restrictions}{...}
{syntab:Restrictions}
{synopt:{opth ca:liper(#)}}defines the maximum absolute score difference or distance between a treated and a non-treated observation (default:
no limit). It is {hi:not} supported by {hi:coarsened exact} matching because due to lack of a score or a distance. Caliper describes the radius in
case of {help ultimatch##general:radius} matching.{break}{hi:Hint:} Because it is difficult to assess the range of the Mahalanobis distance, use the
{help ultimatch##general:perc} option to define a percentile caliper, which will be applied after matching.{p_end}
{synopt:{opth p:erc(#)}}defines a retroactive caliper after matching based on the percentile of the distances of the counterfactuals. All
counterfactuals with a higher distance than the percentile caliper will be detached from the treated observation. A treated observation without
counterfactuals will be dismissed. Since this can be considered a transaction between treated and counterfactuals, the option
{help ultimatch##general:copy} has to be specified. High percentiles (for example: 95), can help to reduce outliers.{p_end}
{synopt:{opth exp:(string)}}defines a logical expression that will be evaluated before a potential non-treated observation will be matched. If the
expression evaluates to zero, the observation will be ignored. A variable name with a prefix "t." designates the active treated observation to allow
for operations between treated and non-treated variables. This option is {hi:not} supported by {hi:coarsened exact} matching.{break}
{hi:Example 1:} exp(abs(empl-t.empl) < 20 | min(empl,t.empl)/max(empl,t.empl) >= 0.8){break}
{hi:Example 2:} exp(region != t.region){p_end}
{synopt:{opth l:imit(string)}}defines a list of variable and rank difference pairs. The rank difference can be omitted for a default value of 5. For
each value of one of these variables, a rank percentile will be defined. The absolute rank difference between a treated and a potentially matched
observation has to be lower or equal the specified difference or, if it is omitted, the default value of 5. A rank percentile is defined for the range
[0,100]. A rank difference of 5 means, that the rank of the value of the matched observation is within a 5% interval around the respective rank of the
value of the treated observation. This option can be applied, if polynomials or other non-monotonous transformations were used to estimate the
score. This option should not be confused with {hi:Percentile Rank} matching. This option is {hi:not} supported by {hi:coarsened exact} matching.{break}
{hi:Example:} limit(empl 10 sales patentstock 10){p_end}

{marker reporting}{...}
{syntab:Reporting}
{synopt:{opth re:port(varlist)}}reports the results of the weighted t-tests for the comparisons of the means of these variables between
the treated and the control group. In case of copied counterfactuals (see option {cmd:copy}) or external unit specifications (see option {cmd:unit})
the standard errors are clustered accordingly. The option {cmd:unmatched} additionally reports the t-tests before the matching. A separate star 
character in {it:varlist} is a shortcut to the matching variables ({it:varlist} of {cmd:ultimatch}). Additional variables, not used for matching, can be
included, i.e., {cmd:report(score * fitness)} to additionally report score and fitness.{p_end}
{synopt:{opt unm:atched)}}adds the comparison of the treated with the control group before matching to the report of the weighted t-tests after matching.
It uses the variables specified in {cmd:report}. If option {cmd:report} is omitted, all matching variables ({it:varlist} of {cmd:ultimatch}) are
reported, which is equivalent to {cmd:report(*)}.{p_end}
{synopt:{opt mat:ched)}}reports of the weighted t-tests after matching. If option {cmd:report} is specified, this option is redundant.
If option {cmd:report} is omitted, all matching variables ({it:varlist} of {cmd:ultimatch}) are reported, which is equivalent to {cmd:report(*)}.{p_end}
{synopt:{opth uni:t(varlist)}}defines key variables determining a data unit. These units will be used to estimate clustered standard errors for the
report. If omitted, every observation is considered a unit. {cmd:unit} is useful for panel data, where a unit can be matched in different time
periods.{p_end}
{synoptline}

{marker system_vars}{...}
{title:Generated system variables}

{synoptset 22 tabbed}{...}
{synopt:Variable}Description{p_end}
{synoptline}
{synopt:{opt _match}}contains an identifier designating matched observations. If possible, observations with the same identifier belong together. This
is guaranteed for the {help ultimatch##general:greedy} and the {help ultimatch##general:copy} option. The latter allows {help ultimatch} to append 
observations to avoid conflicts, i.e. two treated observations competing for the same counterfactual. Otherwise, in the case of a conflict, the identifier
of the closest treated observation is used for the counterfactuals. For {hi:coarsened exact} matching, this variable just enumerates matched cells 
containing treated and non-treated observations. Usually there are no gaps in the enumeration of the identifier. Still, they can occur if the option
{help ultimatch##general:greedy} is used, especially in conjunction with {help ultimatch##restrictions:caliper}.{break}
{hi:_match} is missing for non-matched observations.{p_end}
{synopt:{opt _distance}}contains the distance of a counterfactual to the closest treated observation. For treated observations, {hi:_distance} maximizes 
the distance to all associated counterfactuals. This allows to observe the quality of a match even when the _match identifiers of the counterfactuals have
been replaced by other, closer treated observations in case of not using the options {help ultimatch##general:greedy} or {help ultimatch##general:copy}.
{break}{hi:_distance} is missing for non-matched observations. It will not be created for {hi:coarsened exact} matching.{p_end}
{synopt:{opt _weight}}contains the weight of the observation after matching. The weights balance the distribution of the counterfactuals and
the distribution of the treated. In general, the weight of a treated observation is 1, while the sum of the weights of its counterfactuals
also total to 1. If the option {help ultimatch##general:copy} is not specified, overlapping counterfactuals accumulate their weights. These weights
can be used for subsequent estimations. These weights can be considered probability weights ({hi:pweight}). If options require copying treated
observations, the weight balance stays maintained (see {help ultimatch##general:copy full}).{break}
{hi:_weight} is missing for non-matched observations.{p_end}
{synopt:{opt _copy}}contains a dummy designating observations that were copied (appended to the data) to avoid conflicts between treated observations
over a mutual counterfactual. It will be created when the option {help ultimatch##general:copy} is specified, which usually affects only counterfactuals.
If sub-option {help ultimatch##general:full} is specified, treated observations will also be copied.{break}
{hi:_copy} is missing for non-matched observations, 1 for matched and appended and 0 for matched, original observations.{p_end}
{synopt:{opt _support}}marks observations with common support. It will be created when option {help ultimatch##general:support} is specified.{break}
{hi:_support} has the value 1 for observations with common support and 0 for observations without support. Only observations with common support will be
matched.{p_end}
{synopt:{opt _length}}will be created in case of {hi:cosine} distance matching. The {help varlist} will be interpreted as vector and normalized to length 1.
The variable {cmd:_length} contains the length of the original data vector. Multiplying the variables of {help varlist} with {cmd:_length} will restore them.
{p_end}
{synoptline}

{marker description}{...}
{title:Description}

{pstd}{cmd:ultimatch} implements various matching methods. The matching mode depends on the options and parameters specified. If only one variable is
specified, it is considered a score, which will be used for neighborhood or radius matching. In most cases this score is a predicted propensity score, but
it can be any variable providing a distance relation. If more than one variable is specified, the {help ultimatch##distance:mahalanobis} or 
{help ultimatch##distance:euclidean} distance will be used to determine the surroundings for every treated observation. Specialized distance calculations for
{help ultimatch##distance:cosine} or {help ultimatch##distance:haversine} distances can be enforced if the variable list fulfils the required specifications,
i.e. latitude and longitude for the haversine distance. Finally, by omitting any variable, {hi:coarsened exact} matching is assumed requiring the specification
of the grouping variables in the {help ultimatch##matching:exact} option (see option help ultimatch##matching:exact} for an alternative method to
{hi:coarsened exact} matching).{p_end}

{pstd}Besides {hi:score-based} matching, which is initiated when only one variable is specified, {cmd:ultimatch} supports four different kinds of
{hi:distance-based} matching methods:{p_end}

{synoptset 14 tabbed}{...}
{synopt:{help ultimatch##distance:mahalanobis}}The Mahalanobis distance is scale-invariant and accounts for directional dependencies. It effectively
standardizes variables and normalizes the covariance structure, allowing for accurate comparisons in multivariate spaces where features are correlated or
possess different variances. This is the default distance calculation method.{p_end}

{synopt:{help ultimatch##distance:euclidean}}The Euclidean distance treats all dimensions equally and assumes isotropic data. It can be used for already
normalized or homogeneous data.{p_end}

{synopt:{help ultimatch##distance:haversine}}The Haversine formula is used to calculate the shortest surface distance between two points on a sphere, defined
by their latitude and longitude; the great-circle distance. It is particularly relevant in navigation and geography-related sciences{p_end}

{synopt:{help ultimatch##distance:cosine}}Cosine distance is used to measure how dissimilar two vectors are based on the angle between them, irrespective of
their magnitude. It is particularly useful when relative frequencies or patterns matter more than the absolute scale of the data.{p_end}

{pstd}Distance matching allows you to find the closest neighbor or all neighbors within a radius in terms of the applied distance measurement. Usually, the
neighborhood is determined by calculating the distance of a given point (observation) to all other points in the sample. The runtime of this process
increases according to the product of the treated and the non-treated observations. {cmd:ultimatch} employs a heuristic approach named
{help ultimatch##leeway:Hypersphere Leeway} algorithm preventing this inflation of the runtime.{p_end}

{pstd}Percentile {help ultimatch##distance:rank} transformation can be applied on the score or the distance variables. A percentile rank is the percentage of
distinct values that are equal or lower than it. As opposed to percentiles, variables with the same value always have the same percentile rank eliminating the
arbitrariness of percentiles. The percentile ranks of {help varlist} are used as a way to normalize the dimensions. The default distance is
{help ultimatch##distance:euclidean, but it can be switched to {help ultimatch##distance:mahalanobis. In the case of score-based matching the transformation
eliminates the first differences of neighboring scores. You cannot apply this transformation for haversine or cosine distances.{p_end}

{pstd}{cmd:ultimatch} considers non-treated observations with the same score or distance as one draw. It does not arbitrarily pick one of these
observations unless the option {help ultimatch##general:single} is specified. Therefore, it is required to introduce weights
(see {help ultimatch##system_vars:_weight}) to keep the distributions balanced between treated and counterfactuals.{p_end}

{marker leeway}{...}
{title:Hypersphere Leeway}

{pstd}First, a distance score, based on the chosen distance formula, is created for every observation to a vantage point in the outskirts of the finite
sample distribution of {it:varlist}. It is defined by the centroid of the data shifted by the eigenvector with the highest eigenvalue of the covariance
matrix multiplied by 4-times the square root of the eigenvalue (standard deviation). From this view point the projected cross-section of the data has
the lowest profile. By sorting the data by the score, it is guaranteed that observations with the same score are on the surface of a hypersphere
centered on this vantage point. The dimensions of the sphere are determined by {it:varlist}. Starting from a treated observation, moving along the
score axis in both directions increases respectively decreases the radius of the corresponding spherical layer. For every not-treated observation
visited, the actual distance to the treated observation is calculated. All visited observations are confined within the ever-growing leeway between the
deviating inner and outer spheres. The moment where the closest recorded distance to a non-treated observation is shorter than the distance of the
treated observation to the nearest spherical layer, calculated as difference between the scores, all observations further down or up the score axis
will return higher distances. They will always reside on shells that move further away from the selected treated observation.{break} To identify all
neighbors within a given radius, the inner sphere is defined by the specified radius instead of the respectively closest observation. Every observation
encountered within the inner spere belongs to the neighborhood. The neighborhood is complete when the surfaces of the three involved spheres cease to
intersect.{break} The {hi:haversine} and {hi:cosine} distances are derived from the {hi:Euclidean} distance after transforming the data
accordingly.{p_end}

{pstd}Score-based matching exploits the fact that, with only one dimension, the spheres transform to points along the score axis and the
closest point is immediately ascertainable. With only one dimension, a score can be objectively larger or smaller than another score allowing for the option
{help ultimatch##general:between}, which eludes a definition in the multi-dimensional space.{p_end}

{marker output}{...}
{title:Output}

{pstd}{help ultimatch} reports the treated and control statistics in separate columns. If common {cmd:support} is not enforced, all valid observations
without missings in {help varlist} and variables specified in the {help ultimatch##matching:exact} and {help ultimatch##restrictions:limit} option are considered
supported. Potential exclusions defined in the option {help ultimatch##restrictions:exp} are not regarded. If {help ultimatch##general:copy} is specified,
there will be clustering caused by the copied observations. In addition, there can be intrinsic clusters of the specified units (see option
{help ultimatch##reporting:unit}), for example if the same unit is drawn for different time periods.{p_end}

{pstd}The row {it:Clustered} in the output designates the number of observations belonging to a cluster. The row {it:Clusters} accommodates the
number of different clusters (the size of the cluster aggregate). If {cmd:report} variables are specified, the reported standard errors are clustered
accordingly. The {help ultimatch##reporting:unmatched} standard errors are only clustered, if {help ultimatch##reporting:unit} is specified.{p_end}

{pstd}Additionally, the Standardized Differences in Means ({hi:SDM}) according to {it:Hedge's g} (1981) are reported. An SDM below 0.2 constitutes a
"small" difference (Cohen, 1988). In praxis, the effect size should be {hi:well below} that value.{p_end}

{marker return}{...}
{title:Stored Results}

{synoptset 24 tabbed}{...}
{syntab:Scalars}
{synopt:{cmd:r(comp)}}number of search steps to allocate counterfactuals to evaluate {help ultimatch##leeway:hypersphere leeway} performance.{p_end}
{synopt:{cmd:r(perc)}}percentile caliper value, if {help ultimatch##restrictions:perc} option was specified{p_end}
{synopt:{cmd:r(ptile)}}specified percentile of the {help ultimatch##restrictions:perc} option{p_end}

{syntab:Macros}
{synopt:{cmd:r(matching)}}matching header describing major settings of the {help ultimatch} call{p_end}

{syntab:Matrices}
{synopt:{cmd:r(match)}}all reported statistics including t-tests{p_end}
{synopt:{cmd:r(vantage)}}vantage point coordinates for distance-based matching{p_end}
{synopt:{cmd:r(meridian)}}geographical coordinates and height over ground for the haversine vantage point{p_end}

{marker panel}{...}
{title:Panel data and staggered treatment}

{pstd}Matching panel data is challenging especially when treatment inception is distributed over multiple time periods. This is exacerbated for unbalanced
panels. In general, matching has to occur over the first pre-treatment period of the treated and all non-treated observations, with the time reference as
{help ultimatch##matching:exact} matching citeria. The matched observations are cross-sections clustered by the match identifier
{help ultimatch##system_vars:_match}. It is paramount that the counterfactuals are drawn with the {help ultimatch##general:copy} option because the
cross-sections have to be joined with the panel to introduce the system variables to the other panel observations. This guarantees synchroniced chohorts
of treated and control observations. The following tools will help you with these delicate steps and the subsequent event analysis:{p_end}

{pstd}{help syncmatch} {hline 2} matches panel data to synchronize cohorts by internally calling {help ultimatch}{p_end}

{pstd}{help syncevent} {hline 2} conducts cohort-sensitive fixed effects event study for staggered or concurrent treatment using matched panel data{p_end}

{pstd}Visit {browse "https://github.com/ThorstenDoherr/ultimatch":https://github.com/ThorstenDoherr/ultimatch} for more information and updated versions.{p_end}

{marker example_1}{...}
{title:Examples}

{pstd}In this example, data is simulated to demonstrate a selection bias in a difference-in-differences (DiD) setup and how it can be mitigated by
matching. A group of individuals undergoes treatment with the intention to reduce weight. There is a higher propensity for the self-selection of
individuals with a higher fitness into the treatment indicated by the variable {hi:treated}. The data contains {hi:weight}, {hi:age} and {hi:gender}
for every individual in one period before and one after the treatment. The variable {hi:fitness} is considered unobserved. It is correlated with the
dependent variable {hi:weight} and the selection into treatment leading to a selection bias. The example iterates different matching methods based on
the pre-treatment period for instructional reasons. Remember, the variable fitness is actually unobserved and only reported to demonstrate that
matching on observables can mitigate the selection bias. The example concludes with a DiD regression without matching, resulting in an negative and
significant treatment effect represented by the interaction term {hi:treated##period}. Of course, this effect is only driven by the selection bias,
which is shown by a second regression after matching. A final matching exercise demonstrates how to integrate matches with copied counterfactuals
(see {help ultimatch##general:copy) for further analysis.{p_end}

{hline}
{p 8}{cmd:clear}{p_end}
{p 8}{cmd:{cmd:tempfile tmp}}{p_end}
{p 8}{cmd:{cmd:set obs 2000}}{p_end}
{p 8}{cmd:{cmd:gen byte period = 0 }}//pre-treatment{p_end}
{p 8}{cmd:{cmd:gen long id = _n}}{p_end}
{p 8}{cmd:{cmd:gen byte gender = uniform() > 0.5}}{p_end}
{p 8}{cmd:{cmd:gen age = uniform()}}{p_end}
{p 8}{cmd:{cmd:gen fitness = normal(gender*0.25 - age + rnormal(0, 0.1)) }}// unobserved selection{p_end}
{p 8}{cmd:{cmd:gen weight = normal(-gender*0.25 + age*0.25 - fitness*0.25 + rnormal(0, 0.1))}}{p_end}
{p 8}{cmd:gen treated = normal(fitness + rnormal(0, 0.25)) > 0.73}{p_end}
{p 8}{cmd:save `tmp'}{p_end}
{p 8}{cmd:replace period = 1 }// after treatment{p_end}
{p 8}{cmd:replace weight = weight + weight*(uniform()-0.5)*0.2 - weight*(fitness-0.5)*0.25}{p_end}
{p 8}{cmd:append using `tmp'}{p_end}
{p 8}{cmd:sort id period}{p_end}
{p 8}{cmd:replace weight = int(30.5+100*weight)}{p_end}
{p 8}{cmd:replace age = int(18.5+50*age)}{p_end}
{p 8}{cmd:sum age}{p_end}
{p 8}{cmd:gen agegroup = autocode(age,5,r(min),r(max))}{p_end}
{p 8}{cmd:sum weight}{p_end}
{p 8}{cmd:gen weightgroup = autocode(weight,5,r(min),r(max))}{p_end}
{p 8}{cmd:egen long coarsecell = group(agegroup gender weightgroup)}{p_end}
{p 8}{cmd:probit treated age gender weight if period == 0 }// omitting "unobserved" selection{p_end}
{p 8}{cmd:predict score }// propensity score{p_end}

{p 8}// SCORE-BASED MATCHING{p_end}

{p 8}// Copying and Non-Copying Score-based Neighborhood Matching{p_end}
{p 8}// comparing sum of weights{p_end}
{p 8}{cmd:ultimatch score if period == 0, tr(treated) report(score age weight gender fitness) unmatched copy}{p_end}
{p 8}{cmd:sum _weight if treated == 0}{p_end}
{p 8}{cmd:di r(sum) }// sum of weights equals number of matched treated observations{p_end}
{p 8}{cmd:drop if _copy == 1 }// removing appended observations{p_end}
{p 8}{cmd:drop _* }// removing system variables{p_end}
{p 8}{cmd:ultimatch score if period == 0, treated(treated) report(score age weight gender fitness)}{p_end}
{p 8}{cmd:sum _weight if treated == 0 }// compare the weights{p_end}
{p 8}{cmd:di r(sum)}{p_end}

{p 8}// Single Score-based Neighborhood Matching{p_end}
{p 8}// with single draw and common support{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch score if period == 0, treated(treated) report(score age weight gender fitness) single support}{p_end}

{p 8}// Score-based Percentile Rank Neighborhood Matching{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch score if period == 0, treated(treated) report(score age weight gender fitness) rank}{p_end}

{p 8}// Score-based Neighborhood Matching{p_end}
{p 8}// with exact matching of gender{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch score if period == 0, treated(treated) report(score age weight gender fitness) exact(gender)}{p_end}

{p 8}// Score-based Neighborhood Matching{p_end}
{p 8}// controlling for gender with an expression (same as above but less efficient){p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch score if period == 0, treated(treated) report(score age weight gender fitness) exp(gender == t.gender)}{p_end}

{p 8}// Score-based Neighborhood Matching{p_end}
{p 8}// with percentile rank limitation and common support{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch score if period == 0, treated(treated) report(score age weight gender fitness) limit(age weight) support}{p_end}

{p 8}// Score-based Neighborhood Matching (multiple counterfactuals){p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch score if period == 0, treated(treated) report(score age weight gender fitness) draw(3)}{p_end}

{p 8}// Sandwiched Score-based Neighborhood Matching{p_end}
{p 8}// with multiple counterfactuals in both directions{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch score if period == 0, treated(treated) report(score age weight gender fitness) draw(3) between}{p_end}

{p 8}// Greedy Score-based Neighborhood Matching{p_end}
{p 8}// usage of caliper recommended, especially if draw > 1{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch score if period == 0, treated(treated) report(score age weight gender fitness) caliper(0.05) greedy single}{p_end}

{p 8}// Score-based Radius Matching{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch score if period == 0, treated(treated) report(score age weight gender fitness) caliper(0.01) radius}{p_end}

{p 8}// Coarsened Exact Matching{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch if period == 0, treated(treated) report(score age weight gender fitness) exact(agegroup weightgroup gender)}{p_end}

{p 8}// Copying Single Score-based Neighborhood Matching{p_end}
{p 8}// alternative method to Coarsened Exact based on pseudo score {p_end}
{p 8}// allows all score-based options like single, copy, greedy{p_end}
{p 8}// caliper of 0.5 prevents cell transgression{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch coarsecell if period == 0, treated(treated) report(score age weight gender fitness) caliper(0.5) draw(1) single copy}{p_end}
{p 8}{cmd:drop if _copy == 1}{p_end}

{p 8}// DISTANCE-BASED MATCHING{p_end}

{p 8}// Mahalanobis Distance-based Neighborhood Matching{p_end}
{p 8}// Mahalanobis distance is default for distance-based matching (multiple dimensions){p_end}
{p 8}// exact matching on gender{p_end}
{p 8}// placeholder * in report option represents matching variables{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch age weight if period == 0, treated(treated) exact(gender) report(score * gender fitness)}{p_end}

{p 8}// Euclidean Distance-based Percentile Rank Neighborhood Matching{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch age weight if period == 0, treated(treated) exact(gender) report(score age weight gender fitness) rank euclid}{p_end}

{p 8}// Mahalanobis Distance-based Radius Matching{p_end}
{p 8}// copy option allows to interact each treated with its counterfatcual{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch age weight if period == 0, treated(treated) exact(gender) report(score * fitness) caliper(0.12) radius copy}{p_end}
{p 8}{cmd:drop if _copy == 1 }// remove copies{p_end}

{p 8}// Mahalanobis Distance-based Neighborhood Matching (multiple counterfactuals){p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch age weight if period == 0, treated(treated) exact(gender) report(score * fitness) draw(3)}{p_end}

{p 8}// DIFFERENCES IN DIFFERENCES{p_end}

{p 8}// DiD without matching{p_end}
{p 8}{cmd:reg weight treated##period }{p_end}

{p 8}// Mahalanobis Distance-based Neighborhood Matching{p_end}
{p 8}// without copy option, control group is anonymous{p_end}
{p 8}// system variable _weight balances distributions of treated and control group{p_end}
{p 8}// no direct interaction between treated and control observations possible{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:ultimatch age weight if period == 0, treated(treated) exact(gender) report(* fitness) unmatched}{p_end}

{p 8}// DiD with matching{p_end}
{p 8}{cmd:egen long _m = max(_match), by(id) }// extending matching into the treatment period{p_end}
{p 8}{cmd:egen double _w = max(_weight), by(id) }// extending the weight into the treatment period{p_end}
{p 8}{cmd:reg weight treated##period [pweight=_w] if _m != .}{p_end}

{p 8}// Mahalanobis Distance-based Neighborhood Matching{p_end}
{p 8}// with copy option, each _match has 1 treated and n counterfactuals{p_end}
{p 8}// system variable _weight balances counterfactuals per treated (_weight = 1/n){p_end}
{p 8}// direct interaction between treated and control observations possible{p_end}
{p 8}{cmd:cap drop _*}{p_end}
{p 8}{cmd:preserve }// saving original data{p_end}
{p 8}{cmd:ultimatch age weight if period == 0, treated(treated) exact(gender) report(* fitness) copy unmatched}{p_end}
{p 8}{cmd:keep if _match != . }// only matched obs.{p_end}
{p 8}{cmd:keep id _match _distance _weight }// keep association between id and match, weight and distance{p_end}
{p 8}{cmd:sort id}{p_end}
{p 8}{cmd:save `tmp', replace }// save match{p_end}
{p 8}{cmd:restore }// loading original data; all non-matched observations will be removed with joinby{p_end}
{p 8}{cmd:joinby id using `tmp' }// n:m join of match with data over all periods and copies of counterfactuals {p_end}
{p 8}{cmd:egen long newid = group(_match id) }// new id required because original may not be unique anymore{p_end}

{p 8}// DiD with matching and clustering of standard errors by original id{p_end}
{p 8}{cmd:reg weight treated##period [pweight=_weight], vce(cluster id)}{p_end}
{hline}

{marker example_2}{...}
{pstd}The 2. example creates a two-dimensional scatter plot overlayed with with lines connecting treated (red dots) and
counterfactuals (black dots).{p_end}

{hline}
{p 8}{cmd:clear}{p_end}
{p 8}{cmd:set obs 500}{p_end}
{p 8}{cmd:gen x = uniform()}{p_end}
{p 8}{cmd:gen y = invnorm(uniform())}{p_end}
{p 8}{cmd:sum y}{p_end}
{p 8}{cmd:replace y = (y-r(min))/(r(max)-r(min)) - 0.5}{p_end}
{p 8}{cmd:replace y = x+y*2}{p_end}
{p 8}{cmd:sum x}{p_end}
{p 8}{cmd:replace x = (x-r(min)) / (r(max)-r(min)) }// normalizing x-axis{p_end}
{p 8}{cmd:sum y}{p_end}
{p 8}{cmd:replace y = (y-r(min)) / (r(max)-r(min)) }// normalizing y-axis{p_end}
{p 8}{cmd:gen byte treated = _n <= 250}{p_end}
{p 8}{cmd:ultimatch y x, treated(treated) unm copy full euclid}{p_end}
{p 8}{cmd:sum _match}{p_end}
{p 8}{cmd:local max = r(max)}{p_end}
{p 8}{cmd:local graph = ""}{p_end}
{p 8}{cmd:forvalue i = 1/`max' }{{p_end}
{p 12}{cmd:local graph = "`graph' (line y x if _match == `i', lc(gs14))"}{p_end}

{p 8}{cmd:twoway }///{p_end}
{p 12}{cmd:(scatter y x if treated == 0, msize(vsmall) msymbol(circle) mcolor(black)) }///{p_end}
{p 12}{cmd:(scatter y x if treated == 1, msize(vsmall) msymbol(circle) mcolor(red)) }///{p_end}
{p 12}{cmd:`graph', }///{p_end}
{p 12}{cmd:ytitle(Y) ytitle(, size(zero) color(white) orientation(horizontal)) }///{p_end}
{p 12}{cmd:ylabel(none, nogrid) xlabel(none, nogrid) xtitle(X) xtitle(, size(zero)) legend(off) }///{p_end}
{p 12}{cmd:xsize(4) ysize(4) graphregion(margin(0) fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) }///{p_end}
{p 12}{cmd:plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))}{p_end}
{hline}

{marker example_3}{...}
{pstd}The 3. example shows the effect of cosine distance matching. As the original variables will always be normalized, they are restored with
the generated {help ultimatch##system_vars:_length} variable. The graph demonstrates that the distance is determined by the angle between the 
treated and the counterfactual vector and not by the magnitude of the displacement vector.{p_end}

{hline}
{p 8}{cmd:clear}{p_end}
{p 8}{cmd:set obs 500}{p_end}
{p 8}{cmd:gen x = invnorm(uniform())}{p_end}
{p 8}{cmd:gen y = invnorm(uniform())}{p_end}
{p 8}{cmd:gen byte treated = _n <= 250}{p_end}
{p 8}{cmd:ultimatch x y, treated(treated) matched copy full cosine}{p_end}
{p 8}{cmd:replace x = x*_length  }// restoring original value{p_end}
{p 8}{cmd:replace y = y*_length  }// restoring original value{p_end}
{p 8}{cmd:sum _match}{p_end}
{p 8}{cmd:local max = r(max)}{p_end}
{p 8}{cmd:local graph = ""}{p_end}
{p 8}{cmd:forvalue i = 1/`max' }{{p_end}
{p 12}{cmd:local graph = "`graph' (line y x if _match == `i', lc(gs14))"}{p_end}

{p 8}{cmd:twoway }///{p_end}
{p 12}{cmd:(scatter y x if treated == 0, msize(vsmall) msymbol(circle) mcolor(black)) }///{p_end}
{p 12}{cmd:(scatter y x if treated == 1, msize(vsmall) msymbol(circle) mcolor(red)) }///{p_end}
{p 12}{cmd:`graph', }///{p_end}
{p 12}{cmd:xline(0, lcolor(ltblue)) yline(0, lcolor(ltblue)) }///{p_end}
{p 12}{cmd:ytitle(Y) ytitle(, size(zero) color(white) orientation(horizontal)) }///{p_end}
{p 12}{cmd:ylabel(none, nogrid) xlabel(none, nogrid) xtitle(X) xtitle(, size(zero)) legend(off) }///{p_end}
{p 12}{cmd:xsize(4) ysize(4) graphregion(margin(0) fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) }///{p_end}
{p 12}{cmd:plotregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))}{p_end}
{hline}

{marker example_4}{...}
{pstd}The 4. example is about the deterioration of the hypersphere leeway algorithm under high dimensionality with homogeneous distributions. We create a
clustered distribution with {cmd:K} dimensions and {cmd:C} clusters with {cmd:Cobs} members each. The cluster distribution is defined by {cmd:Csd}, while the
spread of each cluster is determined by {cmd:sd}. The program creates a {cmd:cluster} {help frame} containing the cluster centers, which are referenced in the
{cmd:default} {help frame} via {help frlink}. Around each center {cmd:Cobs} normal distributed cluster members will be created with a standard deviation 
of {cmd:sd}.{p_end}

{pstd}We randomly pick 20% as treated observations for Euclidean distance-nearest neighbor matching. Option {help ultimatch##general:copy} is not
required because no further data handling is intended. The stored result {help ultimatch##return:r(comp)} contains the number of scan steps to
allocate the nearest neighbors using the {help ultimatch##leeway:hypersphere leeway} algorithm. Under normal circumstances, this number should be much lower
compared to a full linear scan, which amounts to {cmd:N(treated)*N(control)} steps. The higher the percentage, the more the
{help ultimatch##leeway:hypersphere leeway} deteriorates into a linear scan. A higher dimensionality ({cmd:K}) or tighter clustering with more overlap
({cmd:Csd, sd}) increase the homogeneity of the distribution with detrimental effect on the performance. Both factors reduce the variance of the distances to
the vantage point whereby a high variance is conducive to the performance.{p_end}
{cmd}
{hline}
{p 8}clear frames{p_end}
{p 8}local K = 10{p_end}
{p 8}local C = 100{p_end}
{p 8}local Csd = 20{p_end}
{p 8}local Cobs = 10{p_end}
{p 8}local sd = 1{p_end}

{p 8}frame create cluster{p_end}
{p 8}frame change cluster{p_end}
{p 8}set obs `C'{p_end}
{p 8}gen int cluid = _n{p_end}
{p 8}forvalues k = 1/`K' {{p_end}
{p 12}gen center`k' = rnormal(`Csd'*3,`Csd'){p_end}
{p 8}sort cluid{p_end}

{p 8}frame change default{p_end}
{p 8}set obs `=`C'*`Cobs''{p_end}
{p 8}gen int cluid = ceil(_n/`Cobs'){p_end}
{p 8}frlink m:1 cluid, frame(cluster) gen(cluster_link){p_end}
{p 8}forvalues k = 1/`K' {{p_end}
{p 12}gen x`k' = frval(cluster_link, center`k') + rnormal(0, `sd'){p_end}

{p 8}twoway scatter x1 x2, mcolor(%30) title("Random Cluster Structure in First Two Dimensions"){p_end}
{p 8}gen byte treated = uniform() < 0.2{p_end}
{p 8}ultimatch x*, treated(treated) euclidean {text:// try cosine or mahalanobis}{p_end}
{p 8}di r(comp){p_end}
{p 8}count if _copy != 1 & treated == 0{p_end}
{p 8}local control = r(N){p_end}
{p 8}count if _copy != 1 & treated == 1{p_end}
{p 8}local treated = r(N){p_end}
{p 8}local share = string(`comp'/(`treated'*`control')*100,"%6.2f"){p_end}
{p 8}di as result "`comp'/`=`treated'*`control'' = `share'%"{p_end}
{hline}
{text}
{marker example_5}{...}
{pstd}The 5. example uses the {help ultimatch##distance:haversine} distance option to find spatial neighbors based on geographical coordinates.
We use an approach similar to {help ultimatch##example_4:Example 4} to create agglomerations around the world. First, we are just looking for the
nearest neighbors of 25% randomly picked treated observations reporting the distance statistics. The next matching exercise applies a caliper of
1000km to determine any overlaps between treated observations and counterfactuals from other agglomerations ({cmd:cluid}). The same can be achieved
by excluding any matches within the same agglomeration of the treated (see: {help ultimatch##restrictions:exp}). Finally, all observations of one
cluster become treated to identify the neighbor agglomerations by matching counterfactuals from all other clusters.{p_end}
{cmd}
{hline}
{p 8}clear frames{p_end}
{p 8}local C = 10{p_end}
{p 8}local Cobs = 10{p_end}
{p 8}local sd = 10{p_end}

{p 8}frame create cluster{p_end}
{p 8}frame change cluster{p_end}
{p 8}set obs `C'{p_end}
{p 8}gen int cluid = _n{p_end}
{p 8}gen lat = uniform()*180-90{p_end}
{p 8}gen lon = uniform()*360-180{p_end}
{p 8}sort cluid{p_end}

{p 8}frame change default{p_end}
{p 8}set obs `=`C'*`Cobs''{p_end}
{p 8}gen int cluid = ceil(_n/`Cobs'){p_end}
{p 8}frlink m:1 cluid, frame(cluster) gen(cluster_link){p_end}
{p 8}gen lat = frval(cluster_link, lat) + rnormal(0, `sd'){p_end}
{p 8}gen lon = frval(cluster_link, lon) + rnormal(0, `sd'){p_end}

{p 8}twoway scatter lat lon, mcolor(%30) mlabel(cluid) title("Random Cluster Structure in First Two Dimensions"){p_end}
{p 8}gen byte treated = uniform() < 0.25{p_end}
{p 8}preserve{p_end}

{p 8}{text:// nearest neighbor distance statistics}{p_end}
{p 8}ultimatch lat lon, treated(treated) haversine copy{p_end}
{p 8}sum _distance if _match != . & treated == 0{p_end}
{p 8}restore, preserve{p_end}

{p 8}{text:// 1000km radius cluster overlap}{p_end}
{p 8}ultimatch lat lon, treated(treated) caliper(1000) radius haversine copy{p_end}
{p 8}egen t_cluid = max(cluid/treated), by(_match) {text:// transfers the cluid of the treated (division by zero is missing for counterfactuals)}{p_end}
{p 8}count if _match != . & treated == 0 & t_cluid != cluid{p_end}
{p 8}restore, preserve{p_end}

{p 8}{text:// 1000km radius cluster overlap enforced with expression}{p_end}
{p 8}ultimatch lat lon, treated(treated) caliper(1000) exp(cluid != t.cluid) radius haversine copy{p_end}
{p 8}count if _match != . & treated == 0{p_end}
{p 8}restore, preserve{p_end}

{p 8}{text:// nearest clusters to cluster 1 observations}{p_end}
{p 8}drop treated{p_end}
{p 8}gen byte treated = cluid == 1{p_end}
{p 8}ultimatch lat lon, treated(treated) haversine copy{p_end}
{p 8}table cluid if _match != . & treated == 0, stat(freq) stat(mean _distance){p_end}
{hline}
{text}
{marker history}{...}
{title:Update History}

{p 0 11}{hi:2026.08.31} New {help ultimatch##restrictions:perc} option allows to specify a percentile caliper applied after matching{break}
Reporting of t-tests is more convenient: {help ultimatch##reporting:matched} & {help ultimatch##reporting:unmatched} do not require
specification of report variables{break}
Report option uses a separate {cmd:*} character as shortcut for the matching variables, i.e., {cmd:report(score * fitness)}{break}
System variable {help ultimatch##system_vars:_copy} is now 0 for matched original observations{break}
New help file{p_end}

{p 0 11}{hi:2025.04.09} Fixed a bug where the {help ultimatch##restrictions:exp} and {help ultimatch##restrictions:limit} options do not work
in very large datasets.{p_end}

{p 0 11}{hi:2025.02.20} Fixed a bug where omitting the {help ultimatch##general:draw} parameter for score based matching would lead to ignoring
the draw limit.{p_end}

{p 0 11}{hi:2024.12.02} Better vantage point calculation based on the principal direction of the data.{break}
Added {help ultimatch##distance:cosine} distance based matching.{break}
Added {help ultimatch##distance:haversine} distance based matching using geographical coordinates (latitude and longitude).{p_end}

{p 0 11}{hi:2021.02.01} Distance-based matching now supports the {{help ultimatch##general:draw} option.{break}
Matching results can be reproduced with {help set seed:set seed}.{break}
Fixed a bug in score-based matching regarding the combination of {help ultimatch##general:copy} and {help ultimatch##general:single}.{p_end}

{p 0 11}{hi:2020.05.19} Small adjustments to the help file.{p_end}

{p 0 11}{hi:2020.03.06} The reported means are displayed as rounded numbers (9th digit after the comma).{p_end}

{p 0 11}{hi:2020.01.22} Optimized the calculation of the outside reference point for the hypersphere leeway algorithm.{p_end}

{p 0 11}{hi:2019.09.10} Implemented Standardized Differences in Means ({hi:SDM}) as additional similarity measurement.{break}
Non-Clustered standard errors are now robust.{p_end}

{p 0 11}{hi:2019.08.31} Fixed a bug in Mahalanobis matching that caused partially greedy behavior in non-greedy mode.{break}
Included options {help ultimatch##distance:euclidean} and {help ultimatch##distance:mahalamobis} to improve flexibility for distance matching.{break}
Included option {help ultimatch##general:full} as sub-option to {help ultimatch##general:copy} to enforce treated/counterfactual tuples.{break}
Included option {help ultimatch##general:radius} to explicitly activate radius matching instead of the implicit activation before.{break}
Percentile {help ultimatch##distance:rank} is now considered a general transformation instead of a separate matching method.{break}
{help ultimatch:ultimatch} now supports radius matching for the distance-based matching method.{break}
Prevent the usage of the options {help ultimatch##restrictions:limit} and {help ultimatch##restrictions:exp} for {hi:coarsened exact} matching.{p_end}

{p 0 11}{hi:2019.04.25} Initial version.{p_end}

{title:Author}

{p 4 4}Thorsten Doherr{break}
Leibniz Centre for European Economic Research (ZEW){break}
E-Mail: doherr@zew.de{break}
Source: {browse "https://github.com/ThorstenDoherr/ultimatch":https://github.com/ThorstenDoherr/ultimatch}{p_end}
