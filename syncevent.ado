// 2026.10.30 \\
cap program drop syncevent
program define syncevent, rclass
	syntax varlist(min=1 max=1) [if] [in], TIme(varname) Start(string) Unit(varname) [Match(varname)] [TReated(varname)] [Window(string)] [MODel(string)] [Level(real 95)] [CFAC] [PRECision] [NOCohort] [TWfe] [NYT(string)] [VCE(string)] [FE(string)]
	tokenize `"`varlist'"'
	tempvar nouse _treated tmp1 first cohort event wc wt bweight from reserve with swap id
	tempname hood replace job C E T W R G sum msum psum
	tempfile main tmp
	local balanced = "balanced"
	
	// checking parameters
	if "`match'" == "" {
		cap confirm  var _match
		if _rc != 0 {
			di as error "implicit match link variable _match not found"
			di as error "use match option to specify match link variable"
			error 999
		}
		cap confirm long var _match
		if _rc != 0 {
			di as error "implicit match link variable _match is not long"
			error 999
		}
		local lab : var label _match
		if `"`lab'"' != "match id" {
			di as error "implicit match link variable _match has invalid label"
			di as error `"it has to be "{res:matched id}" (see {res:syncmatch})"'
			error 999
		}
		di as text "using implicit match link variable {res:_match}"
		local match = "_match"
	}
	local model = trim(`"`model'"')
	local model = cond(`"`model'"' == "", "dummy", `"`model'"')
	foreach v in dummy within poisson {
		if substr("`v'",1,length(`"`model'"')) == `"`model'"' {
			local model = "`v'"
			continue, break
		}
	}
	if inlist(`"`model'"', "dummy", "within", "poisson") == 0 {
		di as error "unsupported model"
		di as error "supported: dummy, within, poisson"
		di as error "dummy uses dummy regression areg (default)"
		di as error "within performs a within-transformation followed by reg"
		di as error "poisson uses ppmlhdfe (Poisson pseudo-likelihood regression)"
		error 999
	}
	if "`model'" == "poisson" {
		local balanced = "" // weights not supported
		cap which ppmlhdfe
		if _rc != 0 {
			di as error "model poisson requires module ppmlhdfe"
			di as error "try: ssc install ppmlhdfe"
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
		if `"`nyt'"' != "" {
			di as error "contant event start and nyt (not-yet-treated) are mutually exclusive"
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
	if "`treated'" == "" & ("`single'" != "" | `"`nyt'"' != ""){
		cap confirm  var _treated
		if _rc != 0 {
			di as error "implicit treatment variable _treated not found"
			di as error "use treated option to specify treatment dummy"
			error 999
		}
		cap confirm byte var _treated
		if _rc != 0 {
			di as error "implicit treatment variable _treated is not byte"
			error 999
		}
		local lab : var label _treated
		if `"`lab'"' != "matched treated unit" {
			di as error "implicit treatment variable _treated has invalid label"
			di as error `"it has to be "{res:matched treated unit}" (see {res:syncmatch})"'
			error 999
		}
		di as text "using implicit treatment variable {res:_treated}"
		local treated = "_treated"
	}
	local nyt = trim(`"`nyt'"')
	if `"`nyt'"' != "" {
		foreach v in ignore exclude replace {
			if substr("`v'",1,length(`"`nyt'"')) == `"`nyt'"' {
				local nyt = "`v'"
				continue, break
			}
		}
		if inlist(`"`nyt'"', "ignore", "exclude", "replace") == 0 {
			di as error "nyt defines how not-yet-treated counterfactuals are handled after becoming treated"
			di as error "nyt supports following keywords: ignore, exlude, replace"
			di as error "ignore the fact that counterfactual obs. become treated (default)"
			di as error "exclude counterfactual obs. after becoming treated and isolated treated obs."
			di as error "replace counterfactual obs. after becoming treated for isolated treated obs."
			error 999
		}
	}
	local vce = trim(`"`vce'"')
	local vce = cond(`"`vce'"' == "", "cluster", `"`vce'"')
	if `"`vce'"' != "" {
		foreach v in cluster robust {
			if substr("`v'",1,length(`"`vce'"')) == `"`vce'"' {
				local vce = "`v'"
				continue, break
			}
		}
		if inlist(`"`vce'"', "cluster", "robust") == 0 {
			di as error "vce specifies the reported standard errors"
			di as error "vce supports following keywords: cluster, robust"
			di as error "cluster reports clustered standard errors always based on unit (default)"
			di as error "robust reports robust standard errors"
			error 999
		}
	}
	local fe = trim(`"`fe'"')
	local fe = cond(`"`fe'"' == "", "cohort", `"`fe'"')
	if `"`fe'"' != "" {
		foreach v in cohort match {
			if substr("`v'",1,length(`"`fe'"')) == `"`fe'"' {
				local fe = "`v'"
				continue, break
			}
		}
		if inlist(`"`fe'"', "cohort", "match") == 0 {
			di as error "fe defines the unit fixed-effects"
			di as error "fe supports following keywords: cohort, match"
			di as error "cohort declares fewer unit fixed-effects per cohort (default)"
			di as error "match declares more unit fixed-effects per match"
			error 999
		}
	}
	local y = "`1'"
	
	// prepare selection
	// define treated observation according to parameters: start, treated
	// excluding matches without variation in treated and control
	// relative time reference (event) and window constrains 
	qui gen byte `nouse' = 1
	qui replace `nouse' = 0 `if' `in'
	qui replace `nouse' = 1 if `match' == . | `time' == . | `unit' == . | `y' == .
	if "`treated'" == "" {
		qui egen byte `_treated' = max(`time' >= `start') if `nouse' == 0, by(`match' `unit')
	}
	else {
		qui egen byte `_treated' = max(`treated' == 1) if `nouse' == 0, by(`match' `unit')
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
	
	// keep only relevant variables
	cap preserve // new stata versions should support nested preserve calls
	if _rc != 0 {
		di as error "warning: preserve is canceled"
		qui restore, not
		preserve
	}
	qui drop if `nouse' == 1
	keep `unit' `match' `_treated' `cohort' `event' `time' `y'
	order `unit' `match' `_treated' `cohort' `event' `time' `y'
	qui frame
	local mainframe = r(currentframe)

	// not-yet-treated
	// reporting counterfactuals switching to treated status but nothing else
	if "`nyt'" == "ignore" {
		di _newline as text "Not yet treated: " as result "ignore"
		qui egen `from' = max(`cohort'/`_treated'), by(`unit') // cfac unit has treated cohort
		qui egen `reserve' = max(`from' == . & `_treated' == 0), by(`match') // treated has reserve of real cfacs
		local N = _N
		qui count if `time' >= `from' & `_treated' == 0
		if r(N) > 0 {
			return scalar nyt_cfac = r(N)
			di as result r(N) as text " treated counterfactual obs."
			qui egen `tmp1' = max(`from'/(`_treated' == 0)), by(`match') // last cfac exit period 
			qui count if `time' >= `tmp1' & `_treated' == 1 & `reserve' == 0
			di as result r(N) as text " affected treated obs."
			return scalar nyt_isolated = r(N)
			drop `tmp1'
		}
		drop `reserve' `from'
	}
	
	// not-yet-treated
	// excluding counterfactuals switching to treated status
	if "`nyt'" == "exclude" {
		di _newline as text "Not yet treated: " as result "exclude"
		qui egen `from' = max(`cohort'/`_treated'), by(`unit') // cfac unit has treated cohort
		qui egen `reserve' = max(`from' == . & `_treated' == 0), by(`match') // treated has reserve of real cfacs
		local N = _N
		qui drop if `time' >= `from' & `_treated' == 0
		if `N' > _N {
			local N = `N'-_N
			return scalar nyt_cfac = `N'
			di as result `N' as text " treated counterfactual obs. excluded"
			qui egen `tmp1' = max(`from'/(`_treated' == 0)), by(`match') // last cfac exit period 
			local N = _N
			qui drop if `time' >= `tmp1' & `_treated' == 1 & `reserve' == 0
			local N = `N'-_N
			di as result `N' as text " isolated treated obs. excluded"
			return scalar nyt_isolated = `N'
			drop `tmp1'
		}
		drop `reserve' `from'
	}
	
	// not-yet-treated
	// replacing counterfactuals switching to treated status with their own counterfactuals 
	while "`nyt'" == "replace" {
		di _newline as text "Not yet treated: " as result "replace"
		qui egen `time_type' `from' = max(`cohort'/`_treated'), by(`unit') // cfac unit has treated cohort
		qui egen byte `reserve' = max(`from' == . & `_treated' == 0), by(`match') // treated has reserve of real cfacs
		qui egen long `with' = max(`match'/`_treated'), by(`unit')
		qui replace `from' = . if `_treated' == 1
		qui replace `with' = . if `_treated' == 1 | `reserve' == 1
		drop `reserve'
		frame put `cohort' `match' `with' `from' if `_treated' == 0 & `with' != ., into(`replace')
		local N = _N
		qui drop if `time' >= `from' & `_treated' == 0
		drop `from' `with'
		if `N' > _N {
			local N = `N'-_N
			return scalar nyt_cfac = `N'
			di as result `N' as text " treated counterfactual obs. excluded"
		}
		else {
			frame drop `replace'
			continue, break
		}
		frame change `replace'
		sort `cohort' `match' `with'
		qui drop if `match' == `match'[_n-1] & `with' == `with'[_n-1]
		qui count if `match' != `match'[_n-1]
		return scalar nyt_replace = _N
		return scalar nyt_treated = r(N)
		local val1 = string(_N, "%12.0f")
		local val2 = string(r(N), "%12.0f")
		di as result "`val2'" as text cond(r(N) == 1, " treated unit receives replacement",  " treated units receive replacement")
		di as result "`val1'" as text cond(_N == 1, " counterfactual unit will be replaced", " counterfactual units will be replaced")
		local l1 = max(length("`val1'"),length("`val2'"))
		qui rename `match' `swap'
		qui rename `with' `match'
		qui rename `swap' `with'
		sort `cohort' `match'
		frame change `mainframe'
		sort `match'
		qui save `main'
		frame change `replace'
		qui tab `cohort', matrow(`C')
		local max_C = rowsof(`C')
		forvalue c = `max_C'(-1)1 {
			cap frame drop `job'
			frame change `replace'
			frame put if `cohort' == `C'[`c',1], into(`job')
			frame change `job'
			local val1 = string(_N, "%12.0f")
			di as result "{ralign `l1':`val1'}" as text " replaced in cohort " as result `C'[`c',1]
			joinby `match' using `main'
			qui drop if `_treated' == 1 | `time' < `from'
			qui replace `match' = `with'
			drop `from' `with'
			qui save `tmp', replace
			frame change `mainframe'
			append using `tmp'
			qui save `main', replace
		}
		frame change `mainframe'
		continue, break
	}
	
	// balancing treated with counterfactuals using weights
	qui gen double `bweight' = 1
	if "`balanced'`cfac'" != "" {
		if "`balanced'" != "" {
			qui egen double `wc' = sum(`bweight'/(`_treated'==0)), by(`match' `event')
			qui egen double `wt' = sum(`bweight'/(`_treated'==1)), by(`match' `event')
			qui replace `bweight' = cond(`wc' !=  0, `bweight' / `wc', `bweight') if `wc' != 1 & `_treated' == 0
			qui replace `bweight' = cond(`wt' !=  0, `bweight' / `wt', `bweight') if `wt' != 1 & `_treated' == 1
			local weight = "[pweight=`bweight']"
		}
		else {
			qui egen byte `wc' = max(`_treated'==0), by(`match' `event')
			qui egen byte `wt' = max(`_treated'==1), by(`match' `event')
		}
		if "`cfac'" != "" {
			qui replace `bweight' = 0 if `wc' == 0
			qui replace `bweight' = 0 if `wt' == 0
			qui count if `_treated' == 1 & `bweight' == 0
			local val1 = string(r(N), "%12.0f")
			return scalar cfac_t = r(N)
			qui count if `_treated' == 0 & `bweight' == 0
			local val2 = string(r(N), "%12.0f")
			return scalar cfac_c = r(N)
			local l1 = max(length("`val1'"),length("`val2'"))
			di _newline as text "Counterfactual balancing:"
			di "{res}{ralign `l1':`val1'}{text} treated obs. without counterfactuals omitted"
			di "{res}{ralign `l1':`val2'}{text} counterfactual obs. without treated omitted"
			qui drop if `bweight' == 0
		}
		drop `wt' `wc'
	}
	
	// renaming variables to prefent conflicts with generated dummies
	local unit_name = "`unit'"
	local y_name = "`y'"
	if regexm("`unit'", "^[tc][0-9]*\_[mp][0-9]+$") | "`twfe'" != "" & regexm("`unit'", "^a\_[0-9]+$") {
		local unit = "_`unit'"
		cap rename `unit_name' `unit'
		while _rc != 0 {
			local unit = "_`unit'"
			cap rename `unit_name' `unit'
		}
	}
	if regexm("`y'", "^[tc][0-9]*\_[mp][0-9]+$") | "`twfe'" != "" & regexm("`y'", "^a\_[0-9]+$") {
		local y_name = "_`y'"
		cap rename `y_name' `y'
		while _rc != 0 {
			local y = "_`y'"
			cap rename `y_name' `y'
		}
	}
	
	// generating cohort units absorbing fixed-effects
	// sorting data (before generating a bunch of dummies)
	if "`fe'" == "match" {
		local cunit = "M_`unit_name'"
		qui egen `cunit' = group(`match' `unit')
	}
	else {
		local cunit = "C_`unit_name'"
		qui egen `cunit' = group(`cohort' `unit')
	}
	sort `cunit'
	
	// generate dummy sets
	qui tab `cohort', matrow(`C')
	local max_C = rowsof(`C')
	qui tab `event', matrow(`E')
	local max_E = rowsof(`E')
	local cvar = ""
	local tvar = ""
	
	// two way fixed effects dummies + absolute time reference
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
	
	// relative time dummies with cohort interaction 
	if "`twfe'" == "" {
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
	
	// regressions
	local t = "t"
	if "`vce'" == "cluster" {
		local vce = "vce(cluster `unit')"
	}
	else {
		local vce = "vce(`vce')"
	}
	if "`model'" == "dummy" {
		qui areg `y' `tvar' `cvar' `absolute' `weight', absorb(`cunit') `vce' level(`level')
		local reg = "areg"
	}
	if "`model'" == "within" {
		qui egen `first' = seq(), by(`cunit' `event') // use only first occurence of cohort unit & event
		qui replace `first' = `first' == 1
		qui recast byte `first'
		foreach v of varlist `y' `tvar' `cvar' `absolute' {
			qui egen double `tmp1' = mean(`v'/`first'), by(`cunit') // mean over events (first occurence)
			qui replace `v' = `v' - `tmp1' if `tmp1' != 0
			drop `tmp1'
		}
		qui reg `y' `tvar' `cvar' `absolute' `weight', `vce' level(`level')
 		local reg = "reg"
	}
	if "`model'" == "poisson" {
		qui ppmlhdfe `y' `tvar' `cvar' `absolute', absorb(`cunit') `vce' level(`level')
		local t = "z"
		local reg = "ppmlhdfe" 
	}
	
	// width of coefficient output to align collective output
	local len = 0
	local vars = subinstr(e(cmdline), ",", "[,", 1) + "["
	local vars = substr("`vars'", index("`vars'", " "), .)
	local vars = substr("`vars'", 2, index("`vars'", "[") - 4)
	foreach v in `vars' {
		if `len' == 0 {
			local len = max(11, length(abbrev("`v'", 12))) // y
		}
		else {
			local len = max(`len', length("`v'"))
		}
	}
	local len = `len'+2
	
	// regression output
	if "`nocohort'" == "" {
		if "`model'" == "within" {
			local val1 = "F("+string(e(df_m),"%18.0f")+", "+string(e(df_r),"%18.0f")+")"
			local l1 = max(length("Number of units"),length("`val1'"))
			local l2 =  max(length(string(e(N),"%12.0f")),length(string(e(chi2),"%18.2f")),6)
			local l3 = `l1'-length("`val1'")
			local l4 = `len'+62-`l1'-`l2'
			local l5 = `l4'-length("Absorbed unit: ")
			qui count if `cunit' != `cunit'[_n-1]
			local val1 = string(r(N),"%18.0f")
			di _newline as text "{lalign `l4':Within transformation}{lalign `l1':Number of units} = {res}{ralign `l2':`val1'}"
			local val2 = abbrev("`cunit'",`l5'-1)
			di as text "Absorbed unit: {res}{lalign `l5':`val2'}{text}{lalign `l1':Obs per unit:}"
			qui egen `tmp1' = sum(`first'), by(`cunit')
			qui sum `tmp1' if `cunit' != `cunit'[_n-1]
			local val1 = string(r(min),"%18.0f")
			local val2 = string(r(mean),"%18.2f")
			local val3 = string(r(max),"%18.0f")
			drop `tmp1'
			di as text "{lalign `l4':}            min = {res}{ralign `l2':`val1'}
			di as text "{lalign `l4':}            avg = {res}{ralign `l2':`val2'}
			di as text "{lalign `l4':}            max = {res}{ralign `l2':`val3'}
		}
		`reg', level(`level')
	}
	
	// stop if twfe
	if "`twfe'" != "" {
		exit
	}

	// determining cohort weights based on treated observations
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

	// linear combinations
	local cols = "estimate se `t' p lb ub level"
	local rows = ""
	matrix `R' = J(`max_E'+1, wordcount("`cols'"), 0)
	matrix colnames `R' = `cols'
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
		local rows = "`rows'  t_`mp'`ev'"
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

	// linear combinations global
	local mlc = ""
	local plc = ""
	scalar `msum' = 0
	scalar `psum' = 0
	forvalue e = 1/`max_E' {
		if `E'[`e',1] == `skip' {
			continue
		}
		local mp = cond(`E'[`e',1] < 0, "m", "p")
		local ev = abs(`E'[`e',1])
		forvalue c = 1/`max_C' {
			local co = `C'[`c',1]
			local coef = "t`co'_`mp'`ev'"
			if colnumb(e(b), "`coef'") != . & colnumb(e(b), "o.`coef'") == . {
				local `mp'lc = `"``mp'lc' + `W'[`c',`e'] * t`co'_`mp'`ev'"'
				scalar ``mp'sum' = ``mp'sum' + `W'[`c',`e']
			}
		}
	}
	foreach mp in m p {
		local `mp'lc = substr(`"``mp'lc'"',4,.)
		if "``mp'lc'" != "" & ``mp'sum' > 0 {
			qui lincom (``mp'lc')/``mp'sum', level(`level')
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
	local rows = "`rows' minus plus"
	matrix rownames `R' = `rows'

	// output of collective coefficients with model specific statistics
	if "`nocohort'" != "" {
		di ""
		if "`model'" == "dummy" {
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
			local val2 = abbrev(e(absvar),`l5'-1)
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
		if "`model'" == "within" {
			local val1 = "F("+string(e(df_m),"%18.0f")+", "+string(e(df_r),"%18.0f")+")"
			local l1 = max(length("Number of units"),length("`val1'"))
			local l2 =  max(length(string(e(N),"%12.0f")),length(string(e(chi2),"%18.2f")),6)
			local l3 = `l1'-length("`val1'")
			local l4 = `len'+62-`l1'-`l2'
			local l5 = `l4'-length("Absorbed unit: ")
			local val1= string(e(N),"%18.0f")
			di as text "{lalign `l4':Within fixed-effects regression}{lalign `l1':Number of obs} = {res}{ralign `l2':`val1'}"
			qui count if `cunit' != `cunit'[_n-1]
			local val1 = string(r(N),"%18.0f")
			local val2 = abbrev("`cunit'",`l5'-1)
			di as text "Absorbed unit: {res}{lalign `l5':`val2'}{text}{lalign `l1':Number of units} = {res}{ralign `l2':`val1'}"
			di as text "{lalign `l4': }{lalign `l1':Obs per unit:}"
			qui egen `tmp1' = sum(`first'), by(`cunit')
			qui sum `tmp1' if `cunit' != `cunit'[_n-1]
			local val1 = string(r(min),"%18.0f")
			local val2 = string(r(mean),"%18.2f")
			local val3 = string(r(max),"%18.0f")
			drop `tmp1'
			di as text "{lalign `l4':}            min = {res}{ralign `l2':`val1'}
			di as text "{lalign `l4':}            avg = {res}{ralign `l2':`val2'}
			di as text "{lalign `l4':}            max = {res}{ralign `l2':`val3'}
			local val1 = string(e(df_m),"%18.0f")
			local val2 = string(e(df_r),"%18.0f")
			local val3 = string(e(F),"%18.2f")
			di as text "{lalign `l4':}F({res:`val1'}, {res:`val2'}){dup `l3': } = {res}{ralign `l2':`val3'}"
			local val1 = string(Ftail(e(df_m), e(df_r), e(F)),"%18.4f")
			di as text "{lalign `l4':}{lalign `l1':Prob > F} = {res}{ralign `l2':`val1'}
			local val1 = string(e(N_clust),"%18.0f")
			local val2 = e(clustvar)
			if `val2' != . {
				di _newline as text "{ralign 78:(Std. err. adjusted for {res:`val1'} clusters in {res:`val2'})}"
			}
			else {
				di
			}
		}
		if "`model'" == "poisson" {
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
			if `val2' != . {
				di _newline as text "{ralign 78:(Std. err. adjusted for {res:`val1'} clusters in {res:`val2'})}"
			}
			else {
				di
			}
		}
	}
	local y_name = abbrev("`y_name'",12)
	local val1 = string(`R'[1,7],"%6.0g")
	di as text "{hline `len'}{c TT}{hline 64}"
	di as text "{ralign `len': }{c |} Collective    Robust"
	di as text "{ralign `len': `y_name' }{c |} Coefficient  std. err.      `t'    P>|`t'|{ralign 25:[`val1'% conf. interval]}"
	di as text "{hline `len'}{c +}{hline 64}"
	local max_R = rowsof(`R')
	local mp = 0
	forvalue i = 1/`max_R' {
		local row = word("`rows'", `i')
		if `mp' == 0 & inlist("`row'", "minus", "plus") == 1 {
			di as text "{hline `len'}{c +}{hline 64}"
			local mp = 1
		}
		di as text "{ralign `len': `row' }{c |} " as result %10.6g `R'[`i',1] " " %10.6g `R'[`i',2] "  " %7.2f `R'[`i',3] "   " %5.3f `R'[`i',4] "    " %9.7g `R'[`i',5]  "   " %9.7g `R'[`i',6] 
	}
	if "`nocohort'" != "" & "`model'" == "poisson" {
		di as text "{hline `len'}{c +}{hline 64}"
		di as text "{ralign `len': Absorbed FE }{c |}   Categories     Redundant = Num. Coefs"
		di as text "{hline `len'}{c +}{hline 64}"
		di as text "{ralign `len': `=abbrev(e(absvars),12)' }{c |} " as result %12.0f e(df_a_nested) "  " %12.0f e(df_a_redundant) " " %12.0f e(df_a)
	}
	di as text "{hline `len'}{c BT}{hline 64}"
	di as text "Variable {res:`cunit'} represents `fe' units of {res:`unit'}"
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
