{smcl}
{* 28aug2026}{...}
{hline}
help for {hi:syncmatch}
{hline}
{title:Title}

{p2colset 5 18 18 2}{...}
{p2col :{cmd:syncmatch} {hline 2}}Counterfactual matching of panel data to synchronize cohorts{break}
Intended to be used with {help syncevent}{break}
Requires {help ultimatch}{p_end}
{p2colreset}{...}

{marker syntax}{...}
{title:Syntax}

{p 8 15 2}{cmd:syncmatch} [{varlist}] [{it:if}] [{it:in}], {opth ti:me(varname)} {opt s:tart(varname|#)} {opth uni:t(varname)} [{opth t:reated(varname)}]
[{opth e:xact(varname)}] [{opt lag:(# [#])}] [{opth eve:nt(varlist)}]
[{help ultimatch:ultimatch_options}]{p_end}

{marker opt_summary}{...}
{synoptset 22 tabbed}{...}
{synopthdr}
{synoptline}
{marker timing}{...}
{syntab:Timing}
{synopt :{opth ti:me(varname)}}variable containing the absolute time reference as an integer value{p_end}
{synopt :{opt s:tart(varname|#)}}variable defining either the staggered start of the treatment per unit or a time value for concurrent treatment. If 
staggered, only the lowest start time is chosen per unit. The start variable should be missing for potential counterfactuals.{p_end}
{synopt :{opth tr:eated(varname)}}declares a dummy variable, which is 1 for treated entities and 0 for potential counterfactuals. This option is required
for concurrent treatment ({cmd:start} is a number) or when the start variable is non-missing for potential counterfactuals.{p_end}
{synopt :{opt lag(# [#])}}specifies the matching window. By default, the first non-treated observation of a treated entity before the treatment
{help syncmatch##timing:start} is subject to matching. The first number defines the maximum lag between the matching period and the
{help syncmatch##timing:start}. The optional second number specifies the minimum lag to establish a safety distance to treatment. Both numbers have to 
be negative or missing ({cmd:.}). Examples:{break}
{cmd:lag(.)} {hline 1} closest non-treated time unit before treatment (default){break}
{cmd:lag(-5 -2)} {hline 1} first non-treated obs. in the range of 5 to 2 time units before treatment{break}
{cmd:lag(-5)} {hline 1} first non-treated obs. up to 5 time units before treatment{break}
{cmd:lag(-5 .)} {hline 1} same as above{break}
{cmd:lag(-1 -1)} {hline 1} non-treated obs. has to be exactly 1 time unit before treatment{break}
{cmd:lag(-1)} {hline 1} same as above{break}
{cmd:lag(. -3)} {hline 1} first non-treated obs., at least 3 time units before treatment{break}{p_end}
{marker matching}{...}
{syntab:Matching}
{synopt :{opth uni:t(varname)}}defines the entities within the panel. An entity is observed over multiple time periods referenced by the
{help syncmatch##timing:time} option. The panel may be unbalanced with gaps between the periods of an individual entity.{p_end}
{synopt :{opth e:xact(varlist)}}specifies variables outlining exact matching cells. The treated observation and its counterfactuals have 
identical values for these variables. These are usually dummies, categorial variables or coarse ordinal variables, e.g., gender or industry codes.{p_end}
{marker restrictions}{...}
{syntab:Restrictions}
{synopt :{opth eve:nt(varlist)}}allows to already specify the dependent variables for the follow-up event analysis (DiD). Only treated and counterfactual
entities with non-missing values for all variables in {help varlist} that can be observed under treatment and before the individual matching
period are eligible for matching. This prevents matching of entities not contributing to the subsequent analysis. It may be favorable to split
multiple dependent variables over separate matches before risking a selection bias towards high-quality entities.{break}
This option creates an {help ultimatch} expression (see: {help ultimatch##restrictions:exp}). If this option is explicitly used, both expressions will be 
put in parenthesis and combined via a logical {cmd:&} (and) operator.{p_end}
{marker ultimatch_options}{...}
{syntab:ultimatch}
{synopt :{help ultimatch:ultimatch_options}} can be additionally specified. By default, {help syncmatch##syntax:syncmatch} will internally call
{help ultimatch:ultimatch} with the {cmd:copy} option using {cmd:mahalanobis} neighborhood matching with replacement. The most common options would be the 
specification of a {cmd:caliper} or a {cmd:perc}entile caliper. The {cmd:single} option will enforce one-on-one matches by picking a random counterfactual 
from equidistant candidates. The {cmd:draw} option specifies the number of counterfactuals matched per treated, counting equidistant candidates according to 
the {cmd:single} setting. Neighborhood matching can be switched to {cmd:radius} matching. Matching with replacement can be replaced with {cmd:greedy} matching.
For more information, please refer to the {help ultimatch} documentation.{p_end}
{synoptline}

{marker system_vars}{...}
{title:Generated system variables}

{synoptset 22 tabbed}{...}
{synopt :Variable}Description{p_end}
{synoptline}
{synopt :{opt _match}}links the treated entities to matched counterfactuals via the same identifier. A specific {cmd:_match} value marks all panel observations of
a treated entity and all panel observations of the matched counterfactuals (there may be more than one counterfactual per treated).{p_end}
{synopt :{opt _hood}}marks the panel observations used for matching entities:{break}
{cmd:0} {hline 1} panel observations not used for matching{break}
{cmd:1} {hline 1} matched treated observation; usually first non-treated observation of an entity before treatment start{break}
{cmd:2} {hline 1} matched counterfactual observation; synchronized by matching parameters and timing with the treated observation{p_end}
{synopt :{opt _weight}}entity p-weights balancing treated and control group of the matched observations ({cmd:_hood} is 1 or 2):{break}
for {cmd:treated}: always 1{break}
for {cmd:counterfactuals}: frequency shares; total to 1 per entity and {cmd:_hood} == 2{break}
By default, multiple counterfactuals are drawn per treated when having the same distance.{p_end}
{synopt :{opt _distance}}contains the distance to the treated for counterfactuals and the maximum distance among the counterfactuals for the treated.{break}
{synoptline}

{marker description}{...}
{title:Description}

{pstd}{cmd:syncmatch} synchronizes the timing between treated panel entities and control group entities by matching treated observations with similar 
control observations in the same period, which is usually just before treatment. The matched observations are linked to the corresponding entities
via the system variable {cmd:_match} sharing the same identifier for a treated entity and its matched counterfactual entities transferring the cohort
structure of the treated group onto the control group.{p_end}

{pstd}{cmd:syncmatch} prepares the panel data for the command {help ultimatch} by isolating the pre-treatment observation of the treated entities to be 
matched according to the matching variables and the exact criteria. As the timing variable is implicitly added of the exact matching variables, synchronicity
with the counterfactuals is enforced.{p_end}

{pstd}After matching, only the matched panel observations remain in the memory. The data complies with the requirements of the {help syncevent} estimator
for heterogenous treatment effects (staggered treatment). This estimator is also suited for simultaneous treatment, which is only a special case of staggered
treatment.{p_end}

{marker remarks}{...}
{title:Remarks}

{pstd}There are several methods available to conduct the event analysis. {cmd:Two-Way Fixed Effects regression} based on relative time dummies representing
negative pre-treatment periods to check for pre-trends and positive treatment periods. If the treatment is staggered, heterogenous treatment effects over
the different cohorts are only insufficiently captured by controlling for absolute time references as the counterfactuals of the different
cohorts are lumped together. Relative time dummies for the control group can be constructed using the {cmd:_match} variable imposing the individual treatment
start on the counterfactuals. {help csdid} is a module implementing the approach proposed by Callaway and Sant'Anna (2021), which solves the cohort bias 
but does not explicitly benefit from the consistent cohort structure imposed by counterfactual matching. The same is true for {help eventstudyweights} an 
estimator implementing the method suggested by Liyang Suna and Sarah Abraham (2020) and other approaches.{p_end}

{pstd}{help syncevent} is specifically designed to exploit the thorough cohort structure imposed from the treated onto the control group via counterfactual
matching. It performs a cohort-interacted two-way fixed effects regression to isolate the cohort-specific treatment effects. The collective treatment effect
of a relative time period is calculated with a linear combination (using {help lincom}) of the corresponding cohort-specific treatment effects weighted by
the respective share of treated entities.{p_end}

{marker return}{...}
{title:Stored Results}

{pstd}{help syncmatch##syntax:syncmatch} returns the stored results of the called {help ultimatch} command.{p_end}

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

{title:Examples}

{pstd}We use the "National Longitudinal Survey of Young Women, 14-24 years old in 1968" survey (nlswork) data to emulate a staggered treatment event for all examples.{p_end}

{pstd}The treatment variable {cmd:occ_raise} contains the first year when an occupational rank was improved. Although the variable {cmd:occ_code} is rather categorial,
there is a slight tendency to more independent, less routine work for lower numbers. The variable {cmd:occ_raise} is missing for women that never reported a change in their
occupation (the division with the dummy {cmd:switch} is missing for zero). This is the control group. We want to observe the change in the wage for the treated group. We
apply Mahalanobis distance-based neighborhood matching on age, wage and job experience constrained by exact matching of industry, occupation and college education. {p_end}

{pstd}We only consider distances up to the 95% percentile, because the exact matching restrictions may cause a high deviation between counterfactuals and treated.
We choose a percentile instead of an absolute caliper because Mahalanobis distances are difficult to assess. This arbitray restriction is only used
for demonstration purposes in this example.{p_end}

{pstd}Although {help syncmatch:syncmatch} is flexible in regard of the pre-treatment lag of the matching, we chose a strict lag of 1 year before (staggered) treatment
to avoid timing inconsistencies caused by response lags in the unbalanced panel data. We already declare our variables of interest for the event study to ensure that at
least one pre-treatment (not involved with matching) and one treatment period exists per matched observation.{p_end}

{pstd}Our event study with {help syncevent##syntax:syncevent} begins 5 years before treatment to observe pre-treatment trends and exhausts all available treatment 
periods. The system variable {cmd:_match} - generated by {help syncmatch:syncmatch} - constitutes the linkage between treated and counterfactual observations.
You do not have to declare {help syncevent##matching:match(_match)} as this is the default setting for {help syncevent}.{p_end}

{hline}
{phang2}{cmd:webuse nlswork, clear}{p_end}
{phang2}{cmd:sort idcode year}{p_end}
{phang2}{cmd:gen byte switch = occ_code < occ_code[_n-1] & idcode == idcode[_n-1]}{p_end}
{phang2}{cmd:egen byte occ_raise = min(year/switch), by(idcode)}{p_end}
{phang2}{cmd:syncmatch age ln_wage ttl_exp, lag(-1) time(year) start(occ_raise) unit(idcode) exact(ind_code occ_code collgrad) event(ln_wage) perc(95) unmatched}{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(occ_raise) unit(idcode) match(_match) window(-5 .) nocohort balanced}{p_end}
{hline}

{pstd}For our 2. example, we simulate a concurrent treatment in the form of a support program for young women with low wage preferably working in a specific 
industry without college education. Treatment year is 73. We can apply a flexible matching window by omitting the {help syncmatch##timing:lag} restriction for
{help syncmatch:syncmatch} as there will be no distortion of the relative treatment periods because of the specific treatment start. In general, the lag scope can
be more generous when the actual treatment initiation is known, irrespective of being staggered or concurrent.{p_end}

{hline}
{phang2}{cmd:webuse nlswork, clear}{p_end}
{phang2}{cmd:gen p_treated = normal(-collgrad*0.25 + (ind_code == 4)*0.5 - ln_wage*0.25 - (age-16)/29 + invnorm(uniform())*0.5)}{p_end}
{phang2}{cmd:egen byte treated = max(p_treated > 0.5 & year == 72), by(idcode)}{p_end}
{phang2}{cmd:replace ln_wage = ln_wage+0.2 if treated & year > 72}{p_end}
{phang2}{cmd:syncmatch age ln_wage ttl_exp, time(year) start(73) treated(treated) exact(ind_code collgrad) event(ln_wage) unit(idcode) unm}{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(73) treated(treated) unit(idcode) window(-5 .) noc bal}{p_end}
{hline}

{title:Author}

{pstd}Thorsten Doherr{break}
Leibniz Centre for European Economic Research (ZEW){break}
E-Mail: doherr@zew.de{break}
Source: {browse "https://github.com/ThorstenDoherr/ultimatch":https://github.com/ThorstenDoherr/ultimatch}{p_end}
