# Functions to generate datasets
generateUIDs <- function(n){
	UID <- sapply(1:n, digest, algo="md5")
	return(UID)
}


assignTeam <- function(teams, data, p=rep(1/length(teams), length(teams))){
	data$team <- rbinom(dim(data)[1], length(teams)-1, p)
    data$teamName <- factor(data$team, labels=teams)
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
    #make minimum response time non-zero (since zero response time is nonsensical)
	data$responseTime <- pmax(data$responseTime, 0.1)
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
	dc <- 1
	x0 <- rnorm(length(uid), 80, 10)
	for(i in 1:minutes){
		str <- ""
		for(j in 1:length(uid)){
			str <- paste("uid=", uid[j], sep="")
			str <- paste(str, ";time=", 
					as.POSIXct(as.numeric(ISOdatetime(2014,startMonth,1,0,0,0)) + i*60 + runif(1,-30,30), origin="1970-01-01"),
					sep="")
			str <- paste(str, ";value=",rnorm(1, x0[j] + 10*sin(.5*i) + 20*cos(rnorm(1,0,.001)*i)), sep="")
			
			# store:
			if(rbinom(1,1,(1-.0005))>0){ # a few missing obs
				data[dc] <- str
				dc <- dc+1
			}
		}
	}
	return(data[!is.na(data)])
}

generateGroups <- function(uid, no.groupings = 2, levels = c(2,10)){
	dat <- ddply(data.frame(uid), .(uid), function(x, no, lev){
		dat <- rep(NA, no)
		for(i in 1:no){
			dat[i] <- sample(1:lev[i],1)
		}
		return(data.frame(uid=x$uid, group=t(dat)))
	}, no=no.groupings, lev=levels)
	return(dat)
}

generateNetwork <- function(uid, p, directed=TRUE){
	n <- length(uid)
	m <- matrix(rbinom(n*n, 1, p), n, n)
	if(!directed){
		m[lower.tri(m)] <- t(m)[lower.tri(m)]
	}
	diag(m) <- 1
	return(data.frame(uid, m))
}
	
