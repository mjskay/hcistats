Enter file contents here# Functions to generate datasets
generateUIDs <- function(n){
	UID <- sapply(1:n, digest, algo="md5")
	return(UID)
}


assignTeam <- function(teams, data, p=rep(1/length(teams), length(teams))){
	data$team <- rbinom(dim(data)[1], length(teams)-1, p)
	return(data)
}

monthlySUSScale <- function(t, data, m0 = 2, within=.2, between=.5){  # team hard-coded
	data <- ddply(data, .(), function(x, t){
		return(data.frame(data[rep(seq_len(nrow(data)), each=t),], time=c(1:t)))
	}, t=t)
	data <- data[-1]
	data <- ddply(data, .(team, time), function(x, m0, within, between){
		x <- generatePsychometricData(x$uid, mean=(m0+x$time*within+x$team*between), nvar=10, nfact=2, g=.3, r=.3, store=FALSE)
	}, m0=2, within=within, between=between)
	data <- data[order(data$team,data$time),]
	return(data)
}

responseTime <- function(t, data, m0=10, within=0, between=0, wb=.1, sigma=1){  # team hard-coded
	data <- ddply(data, .(), function(x, t){
		return(data.frame(data[rep(seq_len(nrow(data)), each=t),], time=c(1:t)))
	}, t=t)
	data <- data[-1]
	data <- ddply(data, .(team, time), function(x, m0, within, between, wb){
		x  <- data.frame(x, responseTime=rnorm(length(x$uid), (m0+x$time*within+x$team*between+x$time*x$team*wb), 1))
		return(x)
	}, m0=2, within=within, between=between, wb=wb)
	data$responseTime <- ifelse(data$responseTime < 0, 0, data$responseTime)
	data <- data[order(data$team,data$time),]
	return(data)
}

generatePsychometricData <- function(uid, mean=3, nvar=9, nfact=3, g=.3, r=.3, store=FALSE){
	n <- length(uid)
	raw.dat <- sim.general(nvar,nfact, g=g,r=r,n=n)
	scale.dat <- round(raw.dat+mean)
	scale.dat <- ifelse(scale.dat<1,1,scale.dat)
	scale.dat <- ifelse(scale.dat>5,5,scale.dat)
	ret.dat <- data.frame(uid=uid, scale.dat)
	if(store){save(ret.dat, file="Psychometric.RData")}
	return(ret.dat)
}

heartRatePerMinute <- function(uid, months=4, startMonth=6){
	minutes <- months * 31 *24 *60
	data <- rep(NA, minutes*length(uid))
