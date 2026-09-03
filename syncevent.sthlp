{smcl}
{* 28aug2026}{...}
{hline}
help for {hi:syncevent}
{hline}
{title:Title}

{p2colset 5 18 18 2}{...}
{p2col :{cmd:syncevent} {hline 2}}Cohort-sensitive fixed effects event study for staggered or concurrent treatment using matched panel data{break}
Intended to be used with {help syncmatch}{p_end}
{p2colreset}{...}

{marker syntax}{...}
{title:Syntax}

{p 8 15 2}{cmd:syncevent} {depvar} [{it:pweight}] [{it:if}] [{it:in}], {opth ti:me(varname)} {opt s:tart(varname|#)} {opth u:nit(varname)} 
[{opth m:atch(varname)}] [{opth tr:eated(varname)}]{break}[{opt w:indow(# [#] [#])}] [{opt m:odel(string)}] 
[{opt l:evel(#)}] [{opt bal:anced}] [{opt cfac}] [{opt prec:ision}] [{opt noc:ohort}] {p_end}

{marker opt_summary}{...}
{synoptset 22 tabbed}{...}
{synopthdr}
{synoptline}
{marker timing}{...}
{syntab:Timing}
{synopt :{opth ti:me(varname)}}variable containing the absolute time reference as integer value{p_end}
{synopt :{opt s:tart(varname|#)}}variable defining either the staggered start of the treatment per unit or a time value for concurrent treatment. If 
staggered, only the lowest start time is chosen per unit. The start variable should be missing for counterfactuals.{p_end}
{synopt :{opth tr:eated(varname)}}declares a dummy variable, which is 1 for treated entities and 0 for counterfactuals. This option is required for 
concurrent treatment ({cmd:start} is a number) or when the start variable is non-missing for counterfactuals.{p_end}
{marker matching}{...}
{syntab:Matching}
{synopt :{opth u:nit(varname)}}defines the entities within the panel. It may not be unique due to matching with replacement. An entity can be drawn 
multiple times as counterfactual irrespective of the cohort. Therefore, it will be used to cluster the standard errors.{p_end}
{synopt :{opth m:atch(varname)}}links a treated entity to the matched counterfactual entities. This identifier has the same value for all panel 
observations of a treated entity and the panel observations of its matched counterfactuals. If not specified, the system variable {cmd:_match} generated
by {help syncmatch} will be assumed.{p_end}
{marker model}{...}
{syntab:Model}
{synopt :{opt prec:ision}}takes the variation of the treated in an unbalanced panel into account for the weighted linear combination of the treatement 
effects. A time preriod with a low representation of treated has a lower weight than another period with better representation for the same cohort. 
Without this option, weights are defined by the maximum number of treated of a cohort irrespective of panel variation (default). A high deviation of 
the collective coefficients between these settings indicates a cohort specific representation bias.{p_end}
{synopt :{opt m:odel(string)}}specifies the fixed-effects model:
{break}{cmd:areg} {hline 1} dummy regression. This is the default model because it supports weight variation within an entity and therefore unbalanced
panel adjustments.
{break}{cmd:xtreg} {hline 1} within regression. Does not support weight variation within an entity. Supports entity balancing of multiple counterfactuals
per treated and counterfactual balancing.
{break}{cmd:xtpoisson} {hline 1} poisson regression. Does not support weights. Standard errors are not clustered.
{break}{cmd:ppmlhdfe} {hline 1} poisson pseudo-likelihood regession. Does not support weights. Standard errors are clustered.
{p_end}
{synopt :{opt tw:fe}}performs a two-way fixed effects regression instead of using a cohort-sensitive fixed effect model. All cohorts are lumped together
irrespective of cohort-specific selection bias, which is mitigated by absolute time dummies. Can be used for demonstration purposes or to gauge the extend
of cohort bias.{p_end}
{marker balancing}{...}
{syntab:Balancing}
{synopt :{opt bal:anced}}balances the counterfactuals according to the treated. Depending on the matching approach, multiple counterfactuals could have
been drawn for a treated entity. To prevent potential bias, probability weights offset the counterfactuals to maintain a balance between the treated and 
the control group. 
The default model {cmd:areg} additionally balances variation in the representation between the periods, while the {cmd:xtreg} model only balances the 
general populations irrespective of the period. The poisson models do not support balancing. Specified pweights are included into the balancing.{p_end}
{synopt :{opt cfac}}balances counterfactuals and treated. With normal balancing, a treated observation without counterfactuals or vice versa in a given
period still remains in the sample. This option will remove any imbalances between the groups by removing those observations. This option is supported
by all models.{p_end}
{marker reporting}{...}
{syntab:Reporting}
{synopt :{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}{p_end}
{synopt :{opt noc:ohort}}suppresses the output of the cohort estimation{p_end}
{synopt :{opt w:indow(# [#] [#])}}specifies the leads (test for pretrents), the omitted pre-treatmemt period (default t-1) and the lags, for example:
{break}{cmd:window(. -1 .)} {hline 1} all available leads and lags, omitt first non-treated period (-1) before treatment (default)
{break}{cmd:window(-5 -1 5)} {hline 1} report 5 leading and 5 lagging periods, omitt period -1 as baseline
{break}{cmd:window(-5 5)} {hline 1} same as above
{break}{cmd:window(-7 .)} {hline 1} report 7 leading periods and all availabe treatment periods, skipping -1
{break}{cmd:window(-7)} {hline 1} same as above
{break}{cmd:window(-7 -2 .)} {hline 1} same as above but omitts t-2
{p_end}
{synoptline}

{marker description}{...}
{title:Description}

{pstd}{cmd:syncevent} conducts an event study on panel data where the timing is synchronized by matching, i.e. with {help syncmatch:syncmatch}. Treatment 
effects will be estimated over a time window accounting for pre-treatment trends and cohort bias. Every cohort window will be interacted separately 
while the collective treatment effects are calculated by weighted linear combinations of the relative time dummies.
{p_end}

{pstd}Prerequisite is a cluster variable connecting treated panel entities with their associated counterfactuals. Time synchronized matching of panel 
data can be performed with {help syncmatch:syncmatch}, which is a wrapper for the matching tool {help ultimatch:ultimatch}.{p_end}

{marker background}{...}
{title:Background}

{pstd}Matching is an intuitive method to synchronize a treated group with staggered treatement entries with a corresponding control group. A matched  
control observation is not only the nearest neighbor in terms of matching parameters but also in terms of the timing, which is considered immutable.
Thus, treated cohorts are transferred onto the control group. This can be achieved by own means, i.e. propensity score matching by cohorts, or by using
{help syncmatch:syncmatch}, which is based on {help ultimatch:ultimatch}.{p_end}

{pstd}Usually, upholding the association between treated and counterfactuals is not 
needed for simultaneous treatment but staggered treatment imposes additional structural variation between the cohorts, for example, differences between
early and late adopters. Two-way fixed effects models would just lump those groups together creating a potential structural bias which cannot be compensated 
by the absolute time reference and the fixed effects. By interacting the relative time dummies with the corresponding cohort, the cohort specific treatment
dummies capture the isolated effect for every cohort within one regression. The relative treatment time dummies cover a time window ranging from placebo
periods to observe pre-treatment trends to a specified treatment time span, e.g., over 5 pre-treatment periods to 7 periods into treatment. The collective
treatment effects for every period in that window is the weighted average of the corresponding cohort specific coefficients. The weights are proportional to
the frequencies of treated entities in the cohorts. Precision weights can be applied to account for different representations of treated in an unbalanced
panel by adjusting for the representation of treated in every relative time period.{p_end}

{marker balancing}{...}
{title:Balancing}

{pstd}Depending on the matching approach, multiple nearest neighbors may be attached to a counterfactual. {help syncmatch:syncmatch}, for example, draws
all neighbors with the same distance as counterfactuals to avoid arbitrary selection. If this is the case, the distributions of treated and counterfactuals
have to be balanced by assigning a probability weight to offset the counterfactuals. The weights of the counterfactuals of a treated entity add up to 1. If
the fixed effects regression model allows for weight variation per entity, the balanced weights are adjusted for every period. Only the
{help syncevent##model:areg} model supports full balancing support of unbalanced panels. The weight total of the counterfactuals is always 1 irrespective of
the period. The model {help syncevent##model:xtreg} supports only entity specific weights without period adjustments. Both models accept the specification
of {help syncevent##syntax:pweight} for probability weights like the variable {cmd:_weight} - a system variable of {help syncmatch:syncmatch} - with entity
weights after matching.{p_end}

{pstd}You can use {cmd:[pweight=_weight]} without the option {help syncevent##balancing:balanced} to enforce entity weights for {help syncevent##model:areg} and 
{help syncevent##model:xtreg}. This is not equivalent to {help syncevent##model:xtreg} with the option {help syncevent##balancing:balanced} because balancing
is constrained by the observed time {help syncevent##reporting:window}. Counterfactuals completely outside the window are not considered for entity balancing.
{p_end} 

{pstd}The poisson models {help syncevent##model:xtpoisson} and {help syncevent##model:ppmlhdfe} do not support weight-based balancing. It is suggested to draw
only one counterfactual per treated.{p_end}

{pstd}Counterfactual balancing ({help syncevent##balancing:cfac}) considers treated and counterfactuals as twin entities. It removes observations without a 
matching counterpart from the sample. This affects treated observations without counterfactuals as well as counterfactual observations without treated per period.
{help syncevent##balancing:cfac} can be applied to all models as it does not require weights. It can be combined with the {help syncevent##balancing:balanced}
option.{p_end}

{marker return}{...}
{title:Stored Results}

{pstd}{help syncevent##syntax:syncevent} stores results in {cmd:e()} for the cohort-interacted regression according to the used {help syncevent##model:model}.{p_end}

{synoptset 24 tabbed}{...}
{syntab:Scalars}
{synopt:{cmd:r(cfac_t)}}number of dropped treated observation when counterfactual balancing is activated{p_end}
{synopt:{cmd:r(cfac_c)}}number of dropped counterfactual observation when counterfactual balancing is activated{p_end}

{synoptset 24 tabbed}{...}
{syntab:Matrices}
{synopt:{cmd:r(collective)}}collective regression results{p_end}

{marker example_1}{...}
{title:Examples}

{pstd}We use the "National Longitudinal Survey of Young Women, 14-24 years old in 1968" survey (nlswork) data to emulate a staggered treatment event for all examples.{p_end}

{pstd}The treatment variable {cmd:spouse} contains the first year when a spouse is reported present in the household signified by the dummy {cmd:msp}. The variable
{cmd:spouse} is missing for single women that never reported a spouse (the division with the dummy is missing for zero). This is the control group. We observe the
change in weekly work hours for the treated group. The matching is performed with the {help syncmatch:syncmatch} command applying Mahalanobis distance-based neighborhood 
matching on work hours, age, wage and  job experience constrained by exact matching of occupation and college education.{p_end}

{pstd}Although {help syncmatch:syncmatch} is flexible in regard of the pre-treatment lag of the matching, we chose a strict lag of 1 year before (staggered) treatment
to avoid timing inconsistencies caused by response lags in the unbalanced panel data. We already declare our variables of interest for the event study to ensure that at
least one pre-treatment (not involved with matching) and one treatment period exists per matched observation.{p_end}

{pstd}Our event study with {help syncevent##syntax:syncevent} begins 5 years before treatment to observe pre-treatment trends and exhausts all available treatment 
periods. The system variable {cmd:_match} - generated by {help syncmatch:syncmatch} - constitutes the linkage between treated and counterfactual observations. 
You do not have to declare {help syncevent##matching:match(_match)} as this is the default setting for {help syncevent}. We see that there is no difference between the
unbalanced and the balanced model as the {cmd:_weight} variable indicates no multiple assignments of counterfactuals. The system variable {cmd:_hood} marks the panel
observations matched by {help syncmatch:syncmatch}.{p_end}

{hline}
{phang2}{cmd:webuse nlswork, clear}{p_end}
{phang2}{cmd:egen byte spouse = min(year/msp), by(idcode)}{p_end}
{phang2}{cmd:syncmatch hours age ln_wage ttl_exp, lag(-1) time(year) start(spouse) exact(occ_code collgrad) event(hours) unit(idcode) unmatched}{p_end}
{phang2}{cmd:syncevent hours, time(year) start(spouse) unit(idcode) match(_match) window(-5 .)}{p_end}
{phang2}{cmd:syncevent hours, time(year) start(spouse) unit(idcode) window(-5 .) nocohort balanced} // match() is optional after syncmatch{p_end}
{phang2}{cmd:sum _weight if _hood == 2}{p_end}
{hline}

{pstd}Removing {cmd:ln_wage} and {cmd:ttl_exp} from the matching command {help syncmatch:syncmatch} will reduce the variation between the counterfactuals and cause 
multiple counterfactual assignments of similar distances. Although this can be prevented with specific matching options, we suggested to use a balanced event 
study instead. Furthermore, the elaborate matching for this example seems unnecessary considering the random nature of the treatment event.
{help syncevent##example_4:Example 4} provides a simple test for random selection.{p_end}

{marker example_2}{...}
{pstd}For example 2, the treatment variable {cmd:occ_raise} contains the first year when an occupational rank was improved. Although the variable {cmd:occ_code} is rather
categorial, there is a slight tendency to more independent, less routine work for lower numbers. The variable {cmd:occ_raise} is missing for women that never reported a change
in their occupation (the division with the dummy {cmd:switch} is missing for zero). This is the control group. We observe the change in wage for the treated group.
We apply Mahalanobis distance-based neighborhood matching on age, wage and job experience constrained by exact matching of industry, occupation and college education.{p_end}

{pstd}As in the previous example, the start of the treatment is not explicitly specified and has to be constructed. In an unbalanced panel, this means that we have to match
on the first pre-treatment period {cmd:lag(-1)} to avoid timing inconsistencies. We already declare our variables of interest for the event study to ensure that at
least one pre-treatment (not involved with matching) and one treatment period exist per matched observation.{p_end}

{hline}
{phang2}{cmd:webuse nlswork, clear}{p_end}
{phang2}{cmd:sort idcode year}{p_end}
{phang2}{cmd:gen byte switch = occ_code < occ_code[_n-1] & idcode == idcode[_n-1]}{p_end}
{phang2}{cmd:egen byte occ_raise = min(year/switch), by(idcode)}{p_end}
{phang2}{cmd:syncmatch age ln_wage ttl_exp, lag(-1) time(year) start(occ_raise) unit(idcode) exact(ind_code occ_code collgrad) event(ln_wage) unmatched}{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(occ_raise) unit(idcode) window(-5 .) balanced}{p_end}
{hline}

{marker example_3}{...}
{pstd}The 3. example simulates a concurrent treatment in the form of a support program for young women with low wage preferably working in a specific 
industry without college education. Treatment year is 73. We can apply a flexible matching window by omitting the {cmd:lag} restriction for {help syncmatch:syncmatch}
as there will be no distortion of the relative treatment periods because of the specific treatment start. In general, the lag scope can be more generous when the
actual treatment initiation is known, irrespective of being staggered or concurrent.{p_end}

{hline}
{phang2}{cmd:webuse nlswork, clear}{p_end}
{phang2}{cmd:gen p_treated = normal(-collgrad*0.25 + (ind_code == 4)*0.5 - ln_wage*0.25 - (age-16)/29 + rnormal(0,0.5))}{p_end}
{phang2}{cmd:egen byte treated = max(p_treated > 0.5 & year == 72), by(idcode)}{p_end}
{phang2}{cmd:replace ln_wage = ln_wage+0.2 if treated & year > 72}{p_end}
{phang2}{cmd:syncmatch hours age ln_wage ttl_exp, time(year) start(73) treated(treated) exact(ind_code collgrad) event(ln_wage) unit(idcode) unm}{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(73) treated(treated) unit(idcode) window(-5 .) noc bal}{p_end}
{hline}

{marker example_4}{...}
{pstd}The 4. example can be considered a template to test for random selection by randomly assigning counterfactuals using score-based matching on random numbers.
This test confirms that the pre-treatment trends are parallel (insignificant) without elaborate matching for {help syncevent##example_1:Example 1}.{p_end}

{hline}
{phang2}{cmd:webuse nlswork, clear}{p_end}
{phang2}{cmd:egen byte spouse = min(year/msp), by(idcode)}{p_end}
{phang2}{cmd:gen double score = uniform()}{p_end}
{phang2}{cmd:syncmatch score, lag(-1) time(year) start(spouse) unit(idcode) unm}{p_end}
{phang2}{cmd:syncevent hours, time(year) start(spouse) unit(idcode) window(-5 .) noc bal}{p_end}
{hline}

{pstd}Try this test for the setup of {help syncevent##example_3:Example 3}.{p_end}

{title:Author}

{pstd}Thorsten Doherr{break}
Leibniz Centre for European Economic Research (ZEW){break}
E-Mail: doherr@zew.de{break}
Source: {browse "https://github.com/ThorstenDoherr/ultimatch":https://github.com/ThorstenDoherr/ultimatch}{p_end}
