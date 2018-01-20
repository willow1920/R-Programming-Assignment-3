# This function reads the file and 
# returns a character vector with the name of the hospital that has the lowest 
# 30-day motality for the specified outcome is that state
# 
# state: 2-character abbreviated name of a state, stored in column 7
# outcome: 1) heart attack 2) heart failure 3)pneumonia
#		corresponding to column 11, 17, 23 respectively
# hospital name : column 2

best <- function(state, outcome) {
	## read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	## check if the state is valid
	check_state <- data[,7] == state	
	if ( sum(check_state) == 0 ) {
		stop("invalid state")
	} 
	data_state <- data[check_state,]	
	## check if outcome is valid
	if ( outcome == "heart attack" ) {		
		data_state1 <- as.numeric(data_state[,11])	
	} else if ( outcome == "heart failure" ) {
		data_state1 <- as.numeric(data_state[,17])
	} else if ( outcome == "pneumonia" ) {
		data_state1 <- as.numeric(data_state[,23])
	} else {
		stop("invalid outcome")
	}
	## Return hospital name in that state with lowest 30-day death	
	min_index <- which.min(data_state1)
	data_state[min_index,2]	
}


# find a website with the answer and get some idea from the post :
# http://www.dataanalytictips.com/r-programming-code-to-compare-hospitals-in-a-state-and-determine-the-best-in-a-given-condition/
# so I will use the good pieces 

# the following function has similar variable as best(),
# num : the ranking of a hospital in that state for that outcome, 
#		can be "best", "worst" or an integer (smaller numbers are better)
# it also returns the name of the hospital

rankhospital <- function(state, outcome, num ="best") {
	outcomes <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
	if ( outcome %in% names(outcomes) ) {
		data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
		if ( state %in% data[["State"]] ) {
			check_state <- data["State"] == state
			if ( sum(check_state) < num & num != "best" & num != "worst") {
				return(NA)
			}
		} else {
			stop("invalid state")
		}
	} else {
		stop("invalid outcome")
	}
	outcome <- outcomes[[outcome]]
	data_state <- data[check_state,]
	data_state1 <- as.numeric(data_state[,outcome])
	data_order <- data_state[order(data_state1, data_state["Hospital.Name"], na.last = NA),]
	#print(tail(data_order))
	if ( num == "best" ) {
		data_order[1, "Hopistal.Name"]
	} else if ( num == "worst" ) {
		#print(nrow(data_order))
		data_order[nrow(data_order), "Hospital.Name"]
	} else {
		data_order[num, "Hospital.Name"]
	}
}


# The following function is to rank hospital in all states
# returns a 2-column data frame : 
# 	1) hospital in each state that has thr ranking specified in num
#	2) state

rankall <- function(outcome, num = "best") {
	outcomes <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
	if ( outcome %in% names(outcomes) ) {
		data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	} else {
		stop("invalid outcome")
	}
	outcome <- outcomes[[outcome]]
	
	s <- split(data, data$State)
	out <- sapply(s, function(x) {
		x_numeric <- as.numeric(x[,outcome])
		x_order <- x[order(x_numeric, x["Hospital.Name"], na.last = NA),]
		if ( num == "best" ) {
			num <- 1
		} else if ( num == "worst" ) {
			num <- nrow(x_order)
		}
		x_order[num, c(hospital = "Hospital.Name", state = "State")]
	})
	out <- as.data.frame(t(out))
	names(out) <- c("hospital", "state")
	out
}
	