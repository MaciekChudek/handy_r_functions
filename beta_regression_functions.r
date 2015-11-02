
require('betareg')
require('MASS')
require("sandwich")
require("lmtest")



convert_to_open_unit_interval_arbitrary = function(x, minVal=NA, maxVal=NA){
	if(is.na(minVal)) minVal = min(x)
	if(is.na(maxVal)) maxVal = max(x)
	convert_to_open_unit_interval((x- minVal)/(maxVal - minVal))
}

convert_to_open_unit_interval_one_hundred_point_scale = function(x){
	n = length(x)
	((x/100)*(n-1)+.05)/n
}

convert_to_open_unit_interval = function(x){
	n = length(x)
	(x*(n-1)+.05)/n
}

convert_from_open_unit_interval = function(x,n, minVal=0, maxVal=100){
	(x*n-.05)/(n-1)*maxVal + minVal
}





reg2beta = function(x, intercept, phi, phi_log=T)
{
	if(phi_log) {
		get_beta_density(x, l2p(intercept),exp(phi))
	}else{
		get_beta_density(x, l2p(intercept),phi)
	}
}

get_beta_density = function(x, mu, phi)
{
	Q = phi * (1-mu)
	P = phi * mu
	dbeta(x, P, Q)
	#print(paste(P,Q))
}



crse = function(model, cluster){ #adapted from http://www.drewdimmery.com/robust-ses-in-r/ 
 require(sandwich)
 require(lmtest)
 M <- length(unique(cluster))
 N <- length(cluster)
 uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
 K = ncol(uj)
 dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
 crse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
 crse.se <- coeftest(model, crse.cov)
 return(list(crse.cov, crse.se))
}




crse.boot = function(model, cluster, reps=10, regressionFunction = betareg){ #adapted from http://www.r-bloggers.com/the-cluster-bootstrap/
	  frmla = model$formula
	  clusts <- names(table(cluster))
	  sterrs <- matrix(NA, nrow=reps, ncol=length(coef(model)))
	  model_data = model.frame(model)
	  
	  for(i in 1:reps){
		index <- sample(1:length(clusts), length(clusts), replace=TRUE)
		aa <- clusts[index]
		bb <- table(aa)
		bootdat <- NULL
		for(j in 1:max(bb)){
		  cc <- model_data[cluster %in% names(bb[bb %in% j]),]
		  for(k in 1:j){
			bootdat <- rbind(bootdat, cc)
		  }
		}
		sterrs[i,] <- coef(regressionFunction(frmla, bootdat))
	  }

	 crse.cov <- cov(sterrs)
	 crse.se <- coeftest(model, crse.cov)
	 return(list(crse.cov, crse.se))
	}


crbr = function(frmla, dat, clust = dat$id){
	crse(betareg(frmla, dat, link.phi='log'), clust)[[2]]
}

