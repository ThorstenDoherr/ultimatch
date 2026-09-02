// 2028.09.01 \\
cap program drop syncmatch
program define syncmatch, rclass
	syntax [varlist(default=none)] [in] [if], Time(varname) Start(string) UNIt(varname) [Treated(varname)] [Exact(varlist)] [LAG(string)] [EVEnt(varlist)] [EXP(string)] *
	tempfile matched hood
	tempvar use match valid _start _treated minmax maxtime mintime pretrend pretreat 
	tempname comp perc ptile match vantage meridian
	cap which ultimatch
	if _rc != 0 {
		di as error "ultimatch required"
		di as error "try: ssc install ultimatch"
		error 999
	}
	local lag = trim(subinstr(`"`lag'"',","," ",.))
	if `"`lag'"' == "" {
		local lag = ". ."
	}
	local far = word(`"`lag'"', 1)
	local near = word(`"`lag'"', 2)
	if wordcount(`"`lag'"') > 2 {
		local near = 999
	}
	else if wordcount(`"`lag'"') == 1 {
		local near = "."
	}
	local rc = 0
	cap local rc = ((`near' < 0 | `near' == .) & (`far' < 0 | `far' == .)) == 0
	local rc = max(`rc', _rc)
	if `rc' > 0 {
		di as error "lag syntax: lag({res:far}{text:=.} [{res:near}{text:=.}])"
		di as error "only negative numbers or missing allowed for {res:far} and {res:near}"
		di as error "specifies the valid time range for counterfactual matching in unbalanced panels, for example:"
		di as error "lag(.): closest non-treated time unit before treatment (default)"
		di as error "lag(-5 -2): first non-treated obs. in the range of 5 (far) to 2 (near) time units before treatment"
		di as error "lag(-5): first non-treated obs. up to 5 time units before treatment"
		di as error "lag(-5 .): same as above"
		di as error "lag(-1 -1): non-treated obs. has to be exactly 1 time unit before treatment"
		di as error "lag(-1): same as above for discrete time units"
		di as error "lag(. -3): first non-treated at least 3 time units before treatment"
		error 999
	}
	if `far' > `near' {
		local swap = `far'
		local `far' = `near'
		local `near' = `swap'
	}
	if `"`event'"' != "" & regex(`" `options'"', " exp\(") {
		di as error "options event and exp are mutually exclusive"
		error 999
	}
	foreach v in _match _weight _distance _hood {
		cap confirm var `v'
		if _rc == 0 {
			di as error "variable `v' is already defined"
			error 999
		}
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
	local type_start : type `start'
	local type_time : type `time'
	sort `unit' `time'
	qui gen byte `use' = 0
	qui replace `use' = 1 `in' `if'
	qui replace `use' = 0 if missing(`unit', `time')
	qui egen `type_start' `_start' = min(`start') if `use' == 1, by(`unit')
	if "`treated'" == "" {
		qui egen byte `_treated' = max(`time' >= `_start') if `use' == 1, by(`unit')
	}
	else {
		qui egen byte `_treated' = max(`treated' == 1) if `use' == 1, by(`unit')
	}
	qui replace `_start' = . if `_treated' == 0
	qui gen byte `match' = `use'
	local vars = subinstr(trim(itrim(`"`varlist' `exact'"')), " ", ",", .)
	qui replace `match' = 0 if missing(`vars')
	if "`event'" != "" {
		local var = "`maxtime'"
		foreach v of varlist `event' {
			qui egen `time_type' `var' = max(`time'/(`v' != .)) if `use' == 1, by(`unit')
			if "`var'" == "`maxtime'" {
				local var = "`tmp'"
			}
			else {
				qui replace `maxtime' = `tmp' if `maxtime' != . & (`tmp' == . | `tmp' < `maxtime')
				drop `tmp'
			}
		}
		local var = "`mintime'"
		foreach v of varlist `event' {
			qui egen `time_type' `var' = min(`time'/(`v' != .)) if `use' == 1, by(`unit')
			if "`var'" == "`mintime'" {
				local var = "`tmp'"
			}
			else {
				qui replace `mintime' = `tmp' if `mintime' != . & `tmp' > `mintime'
				drop `tmp'
			}
		}
		local var = "`pretrend'"
		foreach v of varlist `event' {
			if "`single'" != "" {
				if `near' == . {
					qui egen long `var' = sum(`v' != . & `time' < `single') if `use' == 1 , by(`unit')
				}
				else {
					qui egen long `var' = sum(`v' != . & `time' <= `single'+`near') if `use' == 1 , by(`unit')
				}
			}
			else {
				if `near' == . {
					qui egen long `var' = sum(`v' != . & `time' < `_start') if `use' == 1 & `_start' != ., by(`unit')
				}
				else {
					qui egen long `var' = sum(`v' != . & `time' <= `_start'+`near') if `use' == 1 & `_start' != ., by(`unit')
				}
			}
			if "`var'" == "`pretrend'" {
				local var = "`tmp'"
			}
			else {
				qui replace `pretrend' = `tmp' if `pretrend' != . & (`tmp' == . | `tmp' < `pretrend')
				drop `tmp'
			}
		}
		qui replace `match' = 0 if `match' == 1 & (`maxtime' == . | `mintime' == .)
		if "`single'" != "" {
			qui replace `match' = 0 if `match' == 1 & (`maxtime' < `single' | `mintime' >= `single' | `pretrend' < 2 | `pretrend' == .)
		}
		else {
			qui replace `match' = 0 if `match' == 1 & `_treated' == 1 & (`maxtime' < `_start' | `mintime' >= `_start' | `pretrend' < 2 | `pretrend' == .)
			if `"`exp'"' != "" {
				local exp = `"exp((`maxtime' >= t.`_start' & `mintime' < t.`time') & (`exp'))"'
			}
			else {
				local exp = `"exp(`maxtime' >= t.`_start' & `mintime' < t.`time')"'
			}
		}
	}
	preserve
	qui drop if `match' != 1 
	if `near' == . {
		qui gen byte `pretreat' = (`time'-`_start') < 0 & (`time'[_n+1]-`_start') >= 0 if `_treated' == 1 & `unit' == `unit'[_n+1]
	}
	else {
		qui gen byte `pretreat' = (`time'-`_start') <= `near' & (`time'[_n+1]-`_start') > `near' if `_treated' == 1 & `unit' == `unit'[_n+1]
	}
	if `far' != . {
		qui replace `pretreat' = 0 if `pretreat' == 1 & (`time'-`_start') < `far'
	}
	qui keep if `pretreat' == 1 | `_treated' == 0
	ultimatch `varlist', treated(`_treated') exact(`time' `exact') `exp' copy `options' 
	scalar `comp' = r(comp)
	scalar `perc' = r(perc)
	scalar `ptile' = r(ptile)
	local matching = r(matching)
	matrix `match' = r(match)
	matrix `vantage' = r(vantage)
	matrix `meridian' = r(meridian)
	qui keep if _match != .
	keep `unit' _match _weight _distance `time'
	sort `unit'
	qui save `matched'
	keep _match `time'
	sort _match
	qui drop if _match == _match[_n-1]
	qui save `hood'
	restore
	joinby `unit' using `matched'
	cap drop _merge
	qui merge n:1 _match `time' using `hood', keep(master match)
	label values _merge
	qui recode _merge 3=2 1=0
	rename _merge _hood
	label var _hood "matched observations"
	qui replace _hood = 1 if _hood == 2 & `_treated' == 1
	qui label define _hood 1 "treated" 2 "counterfactual" 0 "panel", replace
	label val _hood _hood
	if `comp' != . {
		return scalar comp = `comp'
	}
	if `perc' != . {
		return scalar perc = `perc'
	}
	if `ptile' != . {
		return scalar ptile = `ptile'
	}
	return local matching = `"`matching'"'
	if `match'[1,1] != . {
		return matrix match = `match'
	}
	if `vantage'[1,1] != . {
		return matrix vantage = `vantage'
	}
	if `meridian'[1,1] != . {
		return matrix meridian = `meridian'
	}
end
