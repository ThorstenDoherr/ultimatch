// 2028.09.03 \\
cap program drop syncevent
program define syncevent, rclass
	syntax varlist(min=1 max=1) [pweight/] [if] [in], TIme(varname) Start(string) Unit(varname) [Match(varname)] [TReated(varname)] [Window(string)] [Model(string)] [Level(real 95)] [BALanced] [CFAC] [PRECision] [NOCohort] [TWfe]
	tokenize `"`varlist'"'
	tempvar nouse _treated tmp1 id cohort event cluster wc wt bweight
	tempname hood C E T W R sum
	if "`match'" == "" {
		cap confirm  var _match
		if _rc != 0 {
			di as error "implict match link variable _match not found"
			di as error "use match option to specify match link variable"
			error 999
		}
		cap confirm numeric var _match
		if _rc != 0 {
			di as error "implict match link variable _match is not numeric"
			error 999
		}
		cap confirm long var _match
		local lab : var label _match
		if _rc != 0 | `"`lab'"' != "match id" {
			di as text "using implict match link variable {res:_match}"
		}
		local match = "_match"
	}
	local model = cond(`"`model'"' == "", "areg", `"`model'"')
	if inlist(`"`model'"', "areg", "xtreg", "xtpoisson", "ppmlhdfe") == 0 {
		di as error "unsupported model"
		di as error "supported: areg (default), xtreg, xtpoisson, ppmlhdfe"
		error 999
	}
	if "`weight'" != "" & inlist(`"`model'"', "xtpoisson", "ppmlhdfe") == 1 {
		di as error "no weight support for model `model'"
		error 999
	}
	if "`balanced'" != "" & inlist(`"`model'"', "xtpoisson", "ppmlhdfe") == 1 {
		di as error "model `model' does not support weight based balancing"
		di as error "counterfactual balancing (cfac) is supported"
		error 999
	}
	if inlist("`model'", "ppmlhdfe") == 1 {
		cap which `model'
		if _rc != 0 {
			di as error "model `model' is not installed"
			di as error "try: ssc install `model'"
			error 999
		}
	}
	local window = trim(subinstr(`"`window'"',","," ",.))
	if `"`window'"' == "" {
		local window = ". -1 ."
	}
	local minus = word(`"`window'"', 1)
	local skip = word(`"`window'"', 2)
	local plus = word(`"`window'"', 3)
	if wordcount(`"`window'"') > 3 {
		local minus = 999
	}
	else if wordcount(`"`window'"') == 2 {
		local plus = `"`skip'"'
		local skip = -1
	}
	else if wordcount(`"`window'"') == 1 {
		local skip = -1
		local plus = "."
	}
	local rc = 0
	cap local rc = ((`minus' < 0 | `minus' == .) & `skip' < 0 & (`skip' > `minus' | `minus' == .) & `plus' >= 0) == 0
	local rc = max(`rc', _rc)
	if `rc' > 0 {
		di as error "window syntax: window(lead{text:=.} [[skip{text:=-1}] lag{text:=.}])"
		di as error "specifies the leads (test for pretrents), the omitted pre-treatmemt period and the lags for the DiD, for example:"
		di as error "window(. -1 .): all available leads and lags, omitt first non-treated period (-1) before treatment (default)"
		di as error "window(-5 -1 5): report 5 leading and 5 lagging periods, omitt period -1 as baseline"
		di as error "window(-5 5): same as above"
		di as error "window(-7 .): report 7 leading periods and all availabe treatment periods, skipping -1"
		di as error "window(-7): same as above"
		di as error "window(-7 -2 .): same as above but omitts t-2"
		error 999
	}
	cap confirm number `start'
	if _rc == 0 {
		if "`treated'" == "" {
			di as error "constant event start requires specification of treated option"
			error 999
		}
		local single = `start'
		tempvar start
		qui gen double `start' = `single'
		qui compress `start'
	}
	else {
		cap confirm var `start'
		if _rc != 0 {
			di as error "event start variable does not exist"
			error 999
		}
	}
	if "`weight'" != "" {
		gen double `bweight' = `exp'
		local weight = "[pweight=`bweight']"
	}
	else {
		gen byte `bweight' = 1
	}
	qui gen byte `nouse' = 1
	qui replace `nouse' = 0 `if' `in'
	qui replace `nouse' = 1 if `match' == . | `bweight' == . | `time' == . | `unit' == . | `1' == .
	qui egen long `id' = group(`match' `unit') if `nouse' == 0, autotype
	if "`treated'" == "" {
		qui egen byte `_treated' = max(`time' >= `start') if `nouse' == 0, by(`id')
	}
	else {
		qui egen byte `_treated' = max(`treated' == 1) if `nouse' == 0, by(`id')
	}
	qui egen double `tmp1' = mean(`_treated') if `nouse' == 0, by(`match')
	qui replace `nouse' = 1 if `nouse' == 0 & (`_treated' == . | `tmp1' == . | `tmp1' == int(`tmp1'))
	drop `tmp1'
	local time_type : type `time'
	local time_type = cond("`time_type'" == "byte", "int", "`time_type'")
	qui egen `time_type' `cohort' = min(`start'/(`_treated'==1)) if `nouse' == 0, by(`match')
	qui gen `time_type' `event' = `time'-`cohort' if `nouse' == 0
	if `minus' != . {
		qui replace `nouse' = 1 if `event' < `minus'
	}
	if `plus' != . {
		qui replace `nouse' = 1 if `event' > `plus'
	}
	if "`balanced'`cfac'" != "" {
		if "`cfac'" != "" {
			qui gen byte `tmp1' = `bweight' > 0 & `bweight' != . if `nouse' == 0
		}
		if "`balanced'" != "" {
			qui recast double `bweight'
			qui egen double `wc' = sum(`bweight'/(`_treated'==0)) if `nouse' == 0, by(`match' `event')
			qui egen double `wt' = sum(`bweight'/(`_treated'==1)) if `nouse' == 0, by(`match' `event')
			qui replace `bweight' = cond(`wc' !=  0, `bweight' / `wc', `bweight') if `nouse' == 0 & `bweight' > 0 & `wc' != 1 & `_treated' == 0
			qui replace `bweight' = cond(`wt' !=  0, `bweight' / `wt', `bweight') if `nouse' == 0 & `bweight' > 0 & `wt' != 1 & `_treated' == 1
			local weight = "[pweight=`bweight']"
		}
		else {
			qui egen byte `wc' = max(`_treated'==0) if `nouse' == 0, by(`match' `event')
			qui egen byte `wt' = max(`_treated'==1) if `nouse' == 0, by(`match' `event')
		}
		if "`cfac'" != "" {
			qui replace `bweight' = 0 if `wc' == 0
			qui replace `bweight' = 0 if `wt' == 0
			qui count if `nouse' == 0 & `_treated' == 1 & `bweight' == 0 & `tmp1' == 1
			local val1 = string(r(N), "%12.0f")
			return scalar cfac_t = r(N)
			qui count if `nouse' == 0 & `_treated' == 0 & `bweight' == 0 & `tmp1' == 1
			local val2 = string(r(N), "%12.0f")
			return scalar cfac_c = r(N)
			local l1 = max(length("`val1'"),length("`val2'"))
			di as text "Counterfactual balancing:"
			di "{res}{ralign `l1':`val1'}{text} treated obs. without counterfactuals omitted"
			di "{res}{ralign `l1':`val2'}{text} counterfactual obs. without treated omitted"
			drop `tmp1'
		}
		drop `wt' `wc'
	}
	frame put `id' `unit' `_treated' `cohort' `event' `time' `bweight' `1' if `nouse' == 0 & `bweight' > 0, into(`hood')
	frame change `hood'
	local unit_name = "`unit'"
	local y_name = "`1'"
	local id_name = "ID"
	cap rename `id' `id_name'
	while _rc != 0 {
		local id_name = "_`id_name'"
		cap rename `id' `id_name'
	}
	if regexm("`unit_name'", "^[tc][0-9]+\_[mp][0-9]+$") {
		local unit_name = "_`unit_name'"
		cap rename `unit' `unit_name'
		while _rc != 0 {
			local unit_name = "_`unit_name'"
			cap rename `unit' `unit_name'
		}
	}
	if regexm("`y_name'", "^[tc][0-9]+\_[mp][0-9]+$") {
		local y_name = "_`y_name'"
		cap rename `1' `y_name'
		while _rc != 0 {
			local y_name = "_`y_name'"
			cap rename `1' `y_name'
		}
	}
	qui tab `cohort', matrow(`C')
	local max_C = rowsof(`C')
	qui tab `event', matrow(`E')
	local max_E = rowsof(`E')
	local cvar = ""
	local tvar = ""
	if "`twfe'" != "" {
		forvalue e = 1/`max_E' {
			if `E'[`e',1] == `skip' {
				continue
			}
			local mp = cond(`E'[`e',1] < 0, "m", "p")
			local ev = abs(`E'[`e',1])
			qui gen byte c_`mp'`ev' = `event' == `E'[`e',1]
			local cvar = "`cvar'" + " c_`mp'`ev'"
			qui gen byte t_`mp'`ev' = c_`mp'`ev' * `_treated'
			local tvar = "`tvar'" + " t_`mp'`ev'"
		}
		local nocohort = ""
		local absolute = ""
		if `max_C' > 1 {
			qui tab `time', matrow(`T')
			local max_T = rowsof(`T')-1
			forvalue t = 2/`max_T' {
				local abs = `T'[`t',1]
				qui gen byte a_`abs' = `time' == `T'[`t',1]
				local absolute = "`absolute'" + " a_`abs'"
			}
		}
	}
	else {
		forvalue c = 1/`max_C' {
			local co = `C'[`c',1]
			forvalue e = 1/`max_E' {
				if `E'[`e',1] == `skip' {
					continue
				}
				local mp = cond(`E'[`e',1] < 0, "m", "p")
				local ev = abs(`E'[`e',1])
				qui gen byte c`co'_`mp'`ev' = (`event' == `E'[`e',1]) * (`cohort' ==  `C'[`c',1])
				local cvar = "`cvar'" + " c`co'_`mp'`ev'"
				qui gen byte t`co'_`mp'`ev' = c`co'_`mp'`ev' * `_treated'
				local tvar = "`tvar'" + " t`co'_`mp'`ev'"
			}
		}
	}
	local t = "t"
	if "`model'" == "areg" {
		qui areg `y_name' `tvar' `cvar' `absolute' `weight', absorb(`id_name') vce(cluster `unit_name') level(`level')
	}
	if "`model'" == "xtreg" {
		if "`balanced'" != "" {
			qui egen double `tmp1' = min(`bweight'), by(`id_name')
			qui replace `bweight' = `tmp1'
			qui drop `tmp1'
		}
		qui xtset `id_name'
		qui xtreg `y_name' `tvar' `cvar' `absolute' `weight', fe vce(cluster `unit_name') level(`level')
	}
	if "`model'" == "xtpoisson" {
		qui xtset `id_name'
		qui xtpoisson `y_name' `tvar' `cvar' `absolute', fe vce(robust) level(`level')
		local t = "z"
	}
	if "`model'" == "ppmlhdfe" {
		qui ppmlhdfe `y_name' `tvar' `cvar' `absolute', absorb(`id_name') vce(cluster `unit_name') level(`level')
		local t = "z"
	}
	if "`nocohort'" == "" {
		`model', level(`level')
	}
	if "`twfe'" != "" {
		exit
	}
	local vars = e(datasignaturevars)
	local len = 0
	foreach v in `vars' {
		if `len' == 0 {
			local len = max(11, length(abbrev("`v'", 12)))
		}
		else {
			local len = max(`len', length("`v'"))
		}
	}
	local len = `len'+2
	local cols = "estimate se `t' p lb ub level"
	local rows = ""
	matrix `R' = J(`max_E'-1, wordcount("`cols'"), 0)
	matrix colnames `R' = `cols'
	matrix  `W' = J(`max_C',`max_E',0)
	forvalue c = 1/`max_C' {
		forvalue e = 1/`max_E' {
			if `E'[`e',1] == `skip' {
				continue
			}
			qui sum `bweight' if `cohort' == `C'[`c',1] & `event' == `E'[`e',1] & `_treated'
			matrix `W'[`c',`e'] = r(sum)
		}
	}
	if "`precision'" == "" {
		mata: syncevent_cohort("`W'")
	}
	local i = 0
	forvalue e = 1/`max_E' {
		if `E'[`e',1] == `skip' {
			continue
		}
		local mp = cond(`E'[`e',1] < 0, "m", "p")
		local ev = abs(`E'[`e',1])
		local lc = ""
		scalar `sum' = 0
		forvalue c = 1/`max_C' {
			local co = `C'[`c',1]
			local coef = "t`co'_`mp'`ev'"
			if colnumb(e(b), "`coef'") != . & colnumb(e(b), "o.`coef'") == . {
				local lc = `"`lc' + `W'[`c',`e'] * t`co'_`mp'`ev'"'
				scalar `sum' = `sum' + `W'[`c',`e']
			}
		}
		local lc = substr(`"`lc'"',4,.)
		local rows = "`rows' t_`mp'`ev'"
		if "`lc'" != "" & `sum' > 0 {
			qui lincom (`lc')/`sum', level(`level')
		}
		else {
			mata: st_rclear()
		}
		local i = `i'+1
		local j = 0
		foreach r in `cols' {
			local j = `j'+1
			matrix `R'[`i',`j'] = r(`r')
		}
	}
	local rows = trim("`rows'")
	matrix rownames `R' = `rows'
	if "`nocohort'" != "" {
		di ""
		if "`model'" == "areg" {
			local val1 = "F("+string(e(df_m),"%18.0f")+", "+string(e(df_r),"%18.0f")+")"
			local l1 = max(length("No. of categories"),length("`val1'"))
			local l2 =  max(length(string(e(N),"%12.0f")),length(string(e(chi2),"%18.2f")),6)
			local l3 = `l1'-length("`val1'")
			local l4 = `len'+62-`l1'-`l2'
			local l5 = `l4'-length("Absorbed variable: ")
			local l6 = `len'+65
			local val1= string(e(N),"%18.0f")
			di as text "{lalign `l4':Linear regression, absorbing indicators}{lalign `l1':Number of obs} = {res}{ralign `l2':`val1'}"
			local val1 = string(e(k_absorb),"%18.0f")
			local val2 = abbrev("`id_name'",`l5'-1)
			di as text "Absorbed variable: {res}{lalign `l5':`val2'}{text}{lalign `l1':No. of categories} = {res}{ralign `l2':`val1'}"
			local val1 = string(e(df_m),"%18.0f")
			local val2 = string(e(df_r),"%18.0f")
			local val3 = string(e(F),"%18.2f")
			di as text "{lalign `l4':}F({res:`val1'}, {res:`val2'}){dup `l3': } = {res}{ralign `l2':`val3'}"
			local val1 = string(Ftail(e(df_m), e(df_r), e(F)),"%18.4f")
			di as text "{lalign `l4':}{lalign `l1':Prob > F} = {res}{ralign `l2':`val1'}"
			local val1 = string(e(r2),"%18.4f")
			di as text "{lalign `l4':}{lalign `l1':R-squared} = {res}{ralign `l2':`val1'}"
			local val1 = string(e(r2_a),"%18.4f")
			di as text "{lalign `l4':}{lalign `l1':Adj. R-squared} = {res}{ralign `l2':`val1'}"
			local val1 = string(e(rmse),"%18.4f")
			di as text "{lalign `l4':}{lalign `l1':Root MSE} = {res}{ralign `l2':`val1'}"
			local val1 = string(e(N_clust),"%18.0f")
			local val2 = e(clustvar)
			if `val2' != . {
				di _newline as text "{ralign 78:(Std. err. adjusted for {res:`val1'} clusters in {res:`val2'})}"
			}
			else {
				di
			}
		}
		if "`model'" == "xtreg" {
			local val1 = "F("+string(e(df_m),"%18.0f")+", "+string(e(df_r),"%18.0f")+")"
			local l1 = max(length(" Number of groups"),length("`val1'"))
			local l2 =  max(length(string(e(N),"%12.0f")),length(string(e(chi2),"%18.2f")),6)
			local l3 = `l1'-length("`val1'")
			local l4 = `len'+62-`l1'-`l2'
			local l5 = `l4'-length("Group variable: ")
			local l6 = `l4'-length("     Within  = ")-6
			local val1 = string(e(corr),"%18.4f")
			local l7 = `l4'-length("corr(u_i, Xb) = ")-length("`val1'")
			local val1= string(e(N),"%18.0f")
			di as text "{lalign `l4':Fixed-effects (within) regression}{lalign `l1':Number of obs} = {res}{ralign `l2':`val1'}"
			local val1 = string(e(N_g),"%18.0f")
			local val2 = abbrev("`id_name'",`l5'-1)
			di as text "Group variable: {res}{lalign `l5':`val2'}{text}{lalign `l1':Number of groups} = {res}{ralign `l2':`val1'}"
			di as text "{lalign `l4':R-squared:}{lalign `l1':Obs per group:}"
			local val1 = string(e(r2_w),"%18.4f")
			local val2 = string(e(g_min),"%18.0f")
			di as text "     Within  = {res}{ralign 6:`val1'}{text}{dup `l6': }              min = {res}{ralign `l2':`val2'}
			local val1 = string(e(r2_b),"%18.4f")
			local val2 = string(e(g_avg),"%18.2f")
			di as text "     Between = {res}{ralign 6:`val1'}{text}{dup `l6': }              avg = {res}{ralign `l2':`val2'}
			local val1 = string(e(r2_o),"%18.4f")
			local val2 = string(e(g_max),"%18.0f")
			di as text "     Overall = {res}{ralign 6:`val1'}{text}{dup `l6': }              max = {res}{ralign `l2':`val2'}
			local val1 = string(e(df_m),"%18.0f")
			local val2 = string(e(df_r),"%18.0f")
			local val3 = string(e(F),"%18.2f")
			di as text "{lalign `l4':}F({res:`val1'}, {res:`val2'}){dup `l3': } = {res}{ralign `l2':`val3'}"
			local val1 = string(e(corr),"%18.4f") 
			local val2 = string(Ftail(e(df_m), e(df_r), e(F)),"%18.4f")
			di as text "corr(u_i, Xb) = {res}`val1'{text}{dup `l7': }{lalign `l1':Prob > F} = {res}{ralign `l2':`val2'}
			local val1 = string(e(N_clust),"%18.0f")
			local val2 = e(clustvar)
			if `val2' != . {
				di _newline as text "{ralign 78:(Std. err. adjusted for {res:`val1'} clusters in {res:`val2'})}"
			}
			else {
				di
			}
		}
		if "`model'" == "xtpoisson" {
			local val1 = "Wald chi2("+string(e(df_m),"%18.0f")+")"
			local l1 = max(length(" Number of groups"),length("`val1'"))
			local l2 =  max(length(string(e(N),"%12.0f")),length(string(e(chi2),"%18.2f")),6)
			local l3 = `l1'-length("`val1'")
			local l4 = `len'+62-`l1'-`l2'
			local l5 = `l4'-length("Group variable: ")
			local val1 = string(e(ll),"%18.4f")
			local l6 = `l4'-length("Log pseudolikelihood = ")-length("`val1'")
			local val1= string(e(N),"%18.0f")
			di as text "{lalign `l4':Conditional fixed-effects Poisson regression}{lalign `l1':Number of obs} = {res}{ralign `l2':`val1'}"
			local val1 = string(e(N_g),"%18.0f")
			local val2 = abbrev("`id_name'",`l5'-1)
			di as text "Group variable: {res}{lalign `l5':`val2'}{text}{lalign `l1':Number of groups} = {res}{ralign `l2':`val1'}"
			di as text "{lalign `l4':}{lalign `l1':Obs per group:}"
			local val1 = string(e(g_min),"%18.0f")
			di as text "{lalign `l4':}              min = {res}{ralign `l2':`val1'}
			local val1 = string(e(g_avg),"%18.2f")
			di as text "{lalign `l4':}              avg = {res}{ralign `l2':`val1'}
			local val1 = string(e(g_max),"%18.0f")
			di as text "{lalign `l4':}              max = {res}{ralign `l2':`val1'}
			local val1 = string(e(df_m),"%18.0f")
			local val2 = string(e(chi2),"%18.2f")
			di as text "{lalign `l4':}Wald chi2({res:`val1'}){dup `l3': } = {res}{ralign `l2':`val2'}"
			local val1 = string(e(ll),"%18.4f")
			local val2 = string(chi2tail(e(df_m), e(chi2)),"%6.4f")
			di as text "Log pseudolikelihood = {res:`val1'}{dup `l6': }{lalign `l1':Prob > chi2} = {res}{ralign `l2':`val2'}"
			di _newline as text "{ralign 78:(Std. err. adjusted for clustering on {res:`id_name'})}"
		}
		if "`model'" == "ppmlhdfe" {
			local val1 = "Wald chi2("+string(e(df_m),"%18.0f")+")"
			local l1 = max(length("Residual df"),length("`val1'"))
			local l2 =  max(length(string(e(N),"%12.0f")),length(string(e(chi2),"%18.2f")),6)
			local l3 = `l1'-length("`val1'")
			local l4 = `len'+62-`l1'-`l2'
			local l5 = max(length(string(e(deviance),"%18.6f")),length(string(e(ll),"%18.6f")))
			local l6 = `l4'-length("Deviance             = ")-`l5'
			local val1= string(e(N),"%18.0f")
			di as text "{lalign `l4':HDFE PPML regression}{lalign `l1':No. of obs} = {res}{ralign `l2':`val1'}"
			local val1 = string(e(df),"%18.0f")
			di as text "{lalign `l4':Absorbing 1 HDFE group}{lalign `l1':Residual df} = {res}{ralign `l2':`val1'}"
			local val1 = string(e(df_m),"%18.0f")
			local val2 = string(e(chi2),"%18.2f")
			di as text "{lalign `l4':Statistics robust to heteroskedasticity}Wald chi2({res:`val1'}){dup `l3': } = {res}{ralign `l2':`val2'}"
			local val1 = string(e(deviance),"%18.6f")
			local val2 = string(chi2tail(e(df_m), e(chi2)),"%6.4f")
			di as text "Deviance             = {res}{ralign `l5':`val1'}{text}{dup `l6': }{lalign `l1':Prob > chi2} = {res}{ralign `l2':`val2'}"
			local val1 = string(e(ll),"%18.6f")
			local val2 = string(e(r2_p),"%6.4f")
			di as text "Log pseudolikelihood = {res}{ralign `l5':`val1'}{text}{dup `l6': }{lalign `l1':Pseudo R2} = {res}{ralign `l2':`val2'}"
			local val1 = string(e(N_clust),"%18.0f")
			local val2 = e(clustvar)
			di _newline as text "{ralign 78:(Std. err. adjusted for {res:`val1'} clusters in {res:`val2'})}"
		}
	}
	local y_name = abbrev("`y_name'",12)
	local val1 = string(`R'[1,7],"%6.0g")
	di as text "{hline `len'}{c TT}{hline 64}"
	di as text "{ralign `len': }{c |} Collective    Robust"
	di as text "{ralign `len': `y_name' }{c |} Coefficient  std. err.      `t'    P>|`t'|{ralign 25:[`val1'% conf. interval]}"
	di as text "{hline `len'}{c +}{hline 64}"
	local max_R = rowsof(`R')
	forvalue i = 1/`max_R' {
		local row = word("`rows'", `i')
		di as text "{ralign `len': `row' }{c |} " as result %10.6g `R'[`i',1] " " %10.6g `R'[`i',2] "  " %7.2f `R'[`i',3] "   " %5.3f `R'[`i',4] "    " %9.7g `R'[`i',5]  "   " %9.7g `R'[`i',6] 
	}
	if "`nocohort'" != "" {
		if "`model'" == "areg" {
			di as text "{hline `len'}{c BT}{hline 64}"
		}
		if "`model'" == "xtreg" {
			di as text "{hline `len'}{c +}{hline 64}"
			di as text "{ralign `len': sigma_u }{c |} " as result %10.6g e(sigma_u)
			di as text "{ralign `len': sigma_e }{c |} " as result %10.6g e(sigma_e)
			di as text "{ralign `len': rho }{c |} " as result %10.6g e(rho) as text "   (fraction of variance due to u_i)"
			di as text "{hline `len'}{c BT}{hline 64}"
		}
		if "`model'" == "xtpoisson" {
			di as text "{hline `len'}{c BT}{hline 64}"
		}
		if "`model'" == "ppmlhdfe" {
			local y_name = abbrev("`id_name'",12)
			di as text "{hline `len'}{c +}{hline 64}"
			di as text "{ralign `len': Absorbed FE }{c |}   Categories     Redundant = Num. Coefs"
			di as text "{hline `len'}{c +}{hline 64}"
			di as text "{ralign `len': `y_name' }{c |} " as result %12.0f e(df_a_nested) "  " %12.0f e(df_a_redundant) " " %12.0f e(df_a)
			di as text "{hline `len'}{c BT}{hline 64}"
		}
	}
	else {
		di as text "{hline `len'}{c BT}{hline 64}"
	}
	return matrix collective = `R'
end

mata:
void syncevent_cohort(string scalar W)
{	real matrix w, cohorts
	w = st_matrix(W)
	cohorts = rowmax(w)
//	cohorts =  cohorts :/ sum(cohorts)
	st_matrix(W, w :* 0 :+ cohorts)
}

void syncevent_precision(string scalar W)
{	real matrix w, events
	w = st_matrix(W)
	events = colsum(w)
	st_matrix(W, w :/ events)
}
end
