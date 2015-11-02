
#centering

sa = function(y) I(scale(y, scale=F)) # Scale age, within model
cent = function(x) scale(x, scale=F)


#logistic


lrm2 = function(...) glm(..., family=binomial(link="logit"))
lrm = function(...)summary(lrm2(...))


#convert between logits, odds and probability

p2o = function(pr)(pr/(1-pr))
o2p = function(odds)(odds/(1+odds))  
p2l = function(pr)(log(pr/(1-pr))) 
l2p = function(logits)(exp(logits)/(1+exp(logits)))  
l2o = function(logits)exp(logits)
o2l = function(odds)log(odds)


criticalZ_95 = qnorm(.975)
criticalZ_90 = qnorm(.95)
criticalZ_99 = qnorm(.995)


capFirst <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

cap <- function(x) sapply(x, capFirst)



#get a transparent version of a color
colorAlpha = function(color, alpha=.4){
	rgb(t(col2rgb(color))/255, alpha=alpha)
}



technicolorTitle <- function(words, colours, line_number=1,cex=1, recale=1) { #borrowed from https://stat.ethz.ch/pipermail/r-help/2009-January/185696.html
    widths <- strwidth(words,cex=(cex/recale))
    spaces <- rep(strwidth(" ",cex=cex), length(widths)-1)
    middle <- mean(par("usr")[1:2])
    total <- sum(widths) + sum(spaces)
    start <- c(0,cumsum(widths[-length(widths)] + spaces))
    start <- start + middle - total/2
    mtext(words, 3, line_number, at=start, adj=0, col=colours,cex=cex)
}

