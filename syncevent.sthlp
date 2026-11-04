{smcl}
{* 03nov2026}{...}
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

{p 8 15 2}{cmd:syncevent} {depvar} [{it:if}] [{it:in}], {opth ti:me(varname)} {opt s:tart(varname|#)} {opth u:nit(varname)} 
[{opth m:atch(varname)}] [{opth tr:eated(varname)}]{break}[{opt w:indow(# [#] [#])}] [{opt mod:el(dummy|within|poisson)}] [{opt fe(cohort|match)}]
[{opt a:djustment(event|cohort|none)}] [{opt tw:fe}] [{opt nyt(ignore|exclude|replace)}] [{opt cfac}] [{opth vce(vcetype)}] [{opt noc:ohort}]
[{opt l:evel(#)}]{p_end}

{marker opt_summary}{...}
{synoptset 22 tabbed}{...}
{synopthdr}
{synoptline}
{marker timing}{...}
{syntab:Timing}
{synopt :{opth ti:me(varname)}}variable containing the absolute time reference as discrete values{p_end}

{synopt :{opt s:tart(varname|#)}}variable defining either the staggered start of the treatment per unit or a time value for concurrent treatment. If 
staggered, only the lowest start time is chosen per unit. The start variable should be missing for counterfactuals. This parameter defines the
{cmd:cohort} of the treated unit.{p_end}

{synopt :{opth tr:eated(varname)}}declares a dummy variable, which is 1 for treated entities and 0 for counterfactuals. This option is required for 
concurrent treatment ({cmd:start} is a number) or when the start variable is non-missing for counterfactuals, which is the case when matching uses
not-yet-treated observations (see {help syncmatch} option: {help syncmatch##matching:nyt}). If not specified but required, the system
variable {help syncmatch##system_vars:_treated} will be used by default.{p_end}

{synopt :{opt w:indow(# [#] [#])}}specifies the leads (test for pretrents), the omitted pre-treatmemt period (default t-1) and the lags, for example:{p_end}
{synopt :}{cmd:window(. -1 .)} {hline 1} all available leads and lags, omitt first non-treated period (-1) before treatment (default){p_end}
{synopt :}{cmd:window(-5 -1 5)} {hline 1} report 5 leading and 5 lagging periods, omitt period -1 as baseline{p_end}
{synopt :}{cmd:window(-5 5)} {hline 1} same as above{p_end}
{synopt :}{cmd:window(-7 .)} {hline 1} report 7 leading periods and all availabe treatment periods, skipping -1{p_end}
{synopt :}{cmd:window(-7)} {hline 1} same as above{p_end}
{synopt :}{cmd:window(-7 -2 .)} {hline 1} same as above but omitts t-2{p_end}

{marker matching}{...}
{syntab:Matching}
{synopt :{opth u:nit(varname)}}defines the entities within the panel. It may not be unique due to matching with replacement. An entity can be drawn 
multiple times as counterfactual irrespective of the cohort. Therefore, it will be used to cluster the standard errors.{p_end}

{synopt :{opth m:atch(varname)}}links a treated entity to the matched counterfactual entities. This identifier has the same value for all panel 
observations of a treated entity and the panel observations of its matched counterfactuals. If not specified, the system variable
{help syncmatch##system_vars:_match} (see {help syncmatch}) will be used by default.{p_end}

{marker model}{...}
{syntab:Model}
{synopt :{opt mod:el(...)}}specifies the fixed-effects model (keywords can be abbreviated):{p_end}
{synopt :}{cmd:dummy} {hline 1} dummy regression. It supports weight variation within an entity and therefore unbalanced panel adjustments
(see {help areg}).{p_end}
{synopt :}{cmd:within} {hline 1} within regression. Similar to {help xtreg}, but supports weight variation within an entity and therefore unbalanced panel
adjustments.{p_end}
{synopt :}{cmd:poisson} {hline 1} poisson pseudo-likelihood regession. Does not support weights. Standard errors are clustered. Requires the installation of
module {help ppmlhdfe}.{p_end}

{synopt :{opt fe(...)}}specifies the unit absorbing the fixed-effects (keywords can be abbreviated):{p_end}
{synopt :}{cmd:cohort} {hline 1} fixed-effects are based on {help syncevent##matching:unit} and {cmd:cohort} assigning only one FE to a unit even when
matched multiple times in a cohort. This is the default setting.{p_end}
{synopt :}{cmd:match} {hline 1} absorbs fixed-effects based on every combination of {help syncevent##matching:unit} and {help syncevent##matching:match}.

{synopt :{opt a:djustment(...)}}determines how the linear combinations ({help lincom}) of the cohort-specific treatment coefficients are weighted
(keywords can be abbreviated):{p_end}
{synopt :}{cmd:event} {hline 1} considers the variation of treated by cohort over time, i.e. in unbalanced panels (default){p_end}
{synopt :}{cmd:cohort} {hline 1} uses the maximum number of treated per cohort imposing population representation{p_end}
{synopt :}{cmd:none} {hline 1} assigns every coefficient the same weight{p_end}
{synopt :}In case of {help syncevent##model:twfe}: {cmd:event} weights have no cohort dimension and {cmd:cohort} is not supported.{p_end}

{synopt :{opt tw:fe}}performs a two-way fixed effects regression instead of using a cohort-sensitive fixed effect model. All cohorts are lumped together
irrespective of cohort-specific selection bias, which is mitigated by absolute time dummies. Can be used for demonstration purposes or to gauge the extend
of cohort bias.{p_end}

{marker se}{...}
{syntab:SE}
{synopt :{opth vce(vcetype)}}{it:vcetype} may be {cmd:cluster} (by default clustered on {help syncevent##matching:unit}) or {cmd:robust}. If omitted,
{cmd:cluster} is the default setting.{p_end}

{marker balancing}{...}
{syntab:Balancing}
{synopt :{opt cfac}}balances counterfactuals and treated. With normal balancing, a treated observation without counterfactuals or vice versa in a given
period still remains in the sample. This option will remove any imbalances between the groups by removing those observations. This option is supported
by all models.{p_end}

{synopt :{opt nyt(string)}}declares that the data contains not-yet-treated counterfactuals and specifies how they should be handled when shifting into
treatment (keywords can be abbreviated):{p_end}
{synopt :}{cmd:ignore} {hline 1} ignores the transition into treatment and keep the affected observations but will report them (for educational purposes
to demonstrate treatment contamination of the control group).{p_end}
{synopt :}{cmd:exclude} {hline 1} removes counterfactual observations after becoming treated. When a treated observation becomes isolated because all
counterfactual observations become treated, it will also be excluded. Counterfactual entities will be curtailed by their treated periods, while treated
entities lose their isolated periods.{p_end}
{synopt :}{cmd:replace} {hline 1} replaces counterfactual obs. becoming treated when associated treated obs. would become isolated. As a not-yet-treated
counterfactual entity is also a treated entity of a later cohort, its counterfactuals can be used as replacement. The periods before the transition into
treatment of the corresponding not-yet-treated counterfactual are truncated from the replacement entity copy. The cohort and the
{help syncevent##matching:match} link variable are adjusted according to the new treated entity.{p_end}
{synopt :}Specifying {cmd:nyt} always reports potential not-yet-treated violations if applicable.
For more information see {help syncevent##nyt:here}.{p_end}

{marker reporting}{...}
{syntab:Reporting}
{synopt :{opt noc:ohort}}suppresses the output of the cohort estimation{p_end}

{synopt :{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}{p_end}
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
early and late adopters. Two-way fixed effects models would just lump those groups together creating a potential structural bias, which cannot be compensated 
by the absolute time reference and the fixed effects. By interacting the relative time dummies with the corresponding cohort, the cohort specific treatment
dummies capture the isolated effect for every cohort within one regression. The relative treatment time dummies cover a time window ranging from placebo
periods to observe pre-treatment trends to a specified treatment time span, e.g., over 5 pre-treatment periods to 7 periods into treatment. The collective
treatment effects for every period in that window is the weighted average of the corresponding cohort specific coefficients. The weights are proportional to
the frequencies of treated entities in the cohorts according to the {help syncevent##model:adjustment} setting.{p_end}

{pstd}This approach follows the proposition off {cmd:Sun and Abraham} (2020) in regard of the cohort interaction of relative time dummies but replaces the 
weighting scheme with matched cohorts. The auxilliary regression to determine the weights to convey the cohort structure of the treated onto the population
is replaced by attaching a similarily structured counterfactual to every treated unit synchronized at a cohort specific pre-treatment time period creating
separate tangible control groups for every treated cohort.{p_end}

{marker balancing}{...}
{title:Balancing}

{pstd}Depending on the matching approach, multiple nearest neighbors may be attached to a counterfactual. {help syncmatch:syncmatch}, for example, draws
all neighbors with the same distance as counterfactuals to avoid arbitrary selection. If this is the case, the distributions of treated and counterfactuals
have to be balanced by assigning a probability weight to offset the counterfactuals. The weights of the counterfactuals of a treated entity add up to 1. If
the fixed effects regression model allows for weight variation per entity, the balanced weights are adjusted for every period. The weight total of the 
counterfactuals is always 1 irrespective of the period.{p_end}

{pstd}The {help syncevent##model:poisson} model does {cmd:not} support weighting to offset unbalanced panels or multiple counterfactuals. It is suggested 
to draw only one counterfactual per treated.{p_end}

{pstd}Counterfactual balancing ({help syncevent##balancing:cfac}) considers treated and counterfactuals as twin entities. It removes observations without a 
matching counterpart from the sample. This affects treated observations without counterfactuals as well as counterfactual observations without treated per
period. {help syncevent##balancing:cfac} can be applied to all models as it does not require weights. {p_end}

{marker nyt}{...}
{title:Not-yet-treated counterfactuals}

{pstd}Enforcing to match counterfactuals only from the pool of never-treated entities is a conventional setup to emulate a natural experiment but may be
too restrictive given the specific treatment potentially excluding the nearest matching options. The {help syncmatch} tool allows to match not-yet-treated 
entities as counterfactuals to enable the best possible matching candidates, which still belong to the control group until they become treated in a later
period. This is only possible for staggered treatement.{p_end}

{pstd}The {help syncevent##balancing:nyt} option allows to specify wether to {cmd:ignore}, {cmd:exclude} or {cmd: replace} the counterfactual observations
under treatment. Periods of treated entities are considered {cmd:isolated} when all counterfactual periods have transitioned into treatment. Excluding
transitioned counterfactual periods also removes isolated periods of the treated entity to maintain structural balance between the groups.{p_end}

{pstd}Because not-yet-treated entities are counterfactual copies of treated entities of a later cohort, isolated treated observations can be sustained by
replacing the excluded now-treated counterfactual obs. with the counterfactuals of the corresponding treated entity for the isolated periods. Being nearest
neighbors to the lost counterfactuals, they represent a suitable substitution.{p_end}

{marker return}{...}
{title:Stored Results}

{pstd}{help syncevent##syntax:syncevent} stores results in {cmd:e()} for the cohort-interacted regression according to the used {help syncevent##model:model}.{p_end}

{synoptset 24 tabbed}{...}
{syntab:Scalars}
{synopt:{cmd:r(cfac_t)}}number of dropped treated observation when counterfactual balancing is activated{p_end}
{synopt:{cmd:r(cfac_c)}}number of dropped counterfactual observation when counterfactual balancing is activated{p_end}
{synopt:{cmd:r(nyt_cfac)}}number of treated observations of not-yet-treated counterfactuals{p_end}
{synopt:{cmd:r(nyt_isolated)}}number of excluded treated observations that become isolated due to counterfactuals shifting into treatment{p_end}
{synopt:{cmd:r(nyt_treated)}}number of treated units receiving replacement that would otherwise become isolated{p_end}
{synopt:{cmd:r(nyt_replace)}}number of now-treated counterfactual units receiving replacement for their treated periods{p_end}

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
You do not have to declare {help syncevent##matching:match(_match)} as this is the default setting for {help syncevent}. We suppress the cohort specific output for the
{help syncevent##model:within} regression.{p_end}

{hline}
{phang2}{cmd:webuse nlswork, clear}{p_end}
{phang2}{cmd:egen byte spouse = min(year/msp), by(idcode)}{p_end}
{phang2}{cmd:syncmatch hours age ln_wage ttl_exp, lag(-1) time(year) start(spouse) exact(occ_code collgrad) event(hours) unit(idcode) unmatched}{p_end}
{phang2}{cmd:syncevent hours, time(year) start(spouse) unit(idcode) match(_match) window(-5 .)}{p_end}
{phang2}{cmd:syncevent hours, time(year) start(spouse) unit(idcode) window(-5 .) model(within) nocohort} // match() is optional after syncmatch{p_end}
{hline}

{pstd}The elaborate matching for this example seems unnecessary considering the random nature of the treatment event. {help syncevent##example_4:Example 4} demonstrates a
simple test for random selection.{p_end}

{marker example_2}{...}
{pstd}For example 2, the treatment variable {cmd:occ_raise} contains the first year when an occupational rank was improved. Although the variable {cmd:occ_code} is rather
categorial, there is a slight tendency to more independent, less routine work for lower numbers. The variable {cmd:occ_raise} is missing for women that never reported a change
in their occupation (the division with the dummy {cmd:switch} is missing for zero). This is the control group. We observe the change in wage for the treated group.
We apply Mahalanobis distance-based neighborhood matching on age, wage and job experience constrained by exact matching of industry, occupation and college education.{p_end}

{pstd}As in the previous example, the start of the treatment is not explicitly specified and has to be constructed. In an unbalanced panel, this means that we have to match
on the first pre-treatment period {cmd:lag(-1)} to avoid timing inconsistencies. We already declare our variables of interest for the event study to ensure that at
least one pre-treatment (not involved with matching) and one treatment period exist per matched observation.{p_end}

{pstd}The second part of this example introduces the matching of not-yet-treated time periods of treated units as counterfactuals to treated units of earlier cohorts.
This is achieved with the keyword {help syncmatch##matching:nyt} of the {help syncmatch} command. The system variable {help syncmatch##system_vars:_hood} indicates the
affected cases.{p_end}
 
{pstd}The first event analysis completely neglects the fact that counterfactuals may also have a non-missing treatment indicator {cmd:occ_raise}, which transfers them into 
the treated group. The second regression acknowledges the existence of not-yet-treated cases by implicitly using the system variable {help syncmatch##system_vars:_treated}
to maintain the integrity of the control group. This is equivalent to specifying {help syncevent##timing:treated(_treated)}. Still, the control group is contaminated 
with now-treated time periods of not-yet-treated matches. The keyword {cmd:exclude} for the {help syncevent##balancing:nyt} option removes the now-treated 
time periods of counterfactuals and excludes isolated time periods of treated completely stripped of counterfactuals. Finally, the attrition by counterfactuals transitioning
into treatment can be mitigated by applying the keyword {cmd:replace} for the {help syncevent##balancing:nyt} option. This activates the recursive process of replacing 
treated time periods of counterfactuals with the corresponding time period range of their own counterfactuals as treated.{p_end}

{pstd}For the third section of this example, we remove all never-treated units representing women that never changed their occupation. To match among the staggered treated
cohorts, we have to specify the {help syncmatch##matching:nyt} option of the {help syncmatch} command. We observe the effect of an occupation raise within a population of
women receiving that treatment. This effect is completly timing driven as counterfactuals will catch up eventually.{p_end}

{hline}
{phang2}{cmd:webuse nlswork, clear}{p_end}
{phang2}{cmd:sort idcode year}{p_end}
{phang2}{cmd:gen byte switch = occ_code < occ_code[_n-1] & idcode == idcode[_n-1]}{p_end}
{phang2}{cmd:egen byte occ_raise = min(year/switch), by(idcode)}{p_end}
{phang2}{cmd:preserve}{p_end}
{phang2}{cmd:syncmatch age ln_wage ttl_exp, lag(-1) time(year) start(occ_raise) unit(idcode) exact(ind_code occ_code collgrad) event(ln_wage) unmatched}{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(occ_raise) unit(idcode) window(-5 .) noc}{p_end}
{hline}
{phang2}{cmd:restore, preserve}{p_end}
{phang2}{cmd:syncmatch age ln_wage ttl_exp, lag(-1) time(year) start(occ_raise) unit(idcode) exact(ind_code occ_code collgrad) event(ln_wage) unm nyt}{p_end}
{phang2}{cmd:tab _hood} // shows the extend of not-yet-treated counterfactual{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(occ_raise) unit(idcode) window(-5 .) noc} // completely neglecting nyt: counterfactuals are considered treated{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(occ_raise) unit(idcode) window(-5 .) noc nyt(ignore)} // nyt units stay in the control group but treatment is ignored{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(occ_raise) unit(idcode) window(-5 .) noc nyt(exclude)} // treatment periods of counterfactuals are excluded{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(occ_raise) unit(idcode) window(-5 .) noc nyt(replace)} // substitution of treatment periods of counterfactuals{p_end}
{hline}
{phang2}{cmd:restore}{p_end}
{phang2}{cmd:drop if occ_raise == .}{p_end}
{phang2}{cmd:syncmatch age ln_wage ttl_exp, lag(-1) time(year) start(occ_raise) unit(idcode) exact(ind_code occ_code collgrad) event(ln_wage) unm nyt}{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(occ_raise) unit(idcode) window(-5 .) noc nyt(replace)}{p_end}
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
{phang2}{cmd:syncmatch hours age ln_wage ttl_exp, time(year) start(73) treated(treated) exact(ind_code occ_code collgrad) event(ln_wage) unit(idcode) unm}{p_end}
{phang2}{cmd:syncevent ln_wage, time(year) start(73) treated(treated) unit(idcode) window(-5 .) noc}{p_end}
{hline}

{marker example_4}{...}
{pstd}The 4. example can be considered a template to test for random selection by randomly assigning counterfactuals using score-based matching on random numbers.
This test confirms that the pre-treatment trends are parallel (insignificant) without elaborate matching for {help syncevent##example_1:Example 1}.{p_end}

{hline}
{phang2}{cmd:webuse nlswork, clear}{p_end}
{phang2}{cmd:egen byte spouse = min(year/msp), by(idcode)}{p_end}
{phang2}{cmd:gen score = uniform()}{p_end}
{phang2}{cmd:syncmatch score, lag(-1) time(year) start(spouse) unit(idcode) unm}{p_end}
{phang2}{cmd:syncevent hours, time(year) start(spouse) unit(idcode) window(-5 .) noc}{p_end}
{hline}

{pstd}Try this test for the setup of {help syncevent##example_3:Example 3}.{p_end}

{title:Author}

{pstd}Thorsten Doherr{break}
Leibniz Centre for European Economic Research (ZEW){break}
E-Mail: doherr@zew.de{break}
Source: {browse "https://github.com/ThorstenDoherr/ultimatch":https://github.com/ThorstenDoherr/ultimatch}{p_end}
