# Graphing Scripts

library(doBy)
	library(plyr)
	library(car)

	# Graphing funciton
	one.way.plot = function(DV,IV1,SubjNo,x.label ="Add X Label", main.label = "Add main header", y.label = "Add y label", log.test = FALSE){
		print("Logit Transform DV")
		ylim.grph <- c(0,1)
	    if(log.test == TRUE){
	    	logit(DV) -> DV
	    	ylim.grph <- c(-4,1)}
	    tapply(DV, INDEX = list(IV1), FUN = mean,na.rm = T) -> graph.data
	    tapply(DV, INDEX = list(IV1), FUN = sd, na.rm = T) -> graph.se
	    graph.se/sqrt(length(unique(SubjNo))) -> graph.se
	    barplot(graph.data, beside = T, col = c("white"), ylim = ylim.grph, ylab = y.label, xlab = x.label, main = main.label, border = NA) -> dat.g
	    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightgray")
	    abline(h=(seq(-3,1)), col="black", lty="dotted")
	    legend(4.5,1.3,rownames(graph.data), fill = c("red","blue"))
	    points(dat.g, graph.data, col = c("red","blue"), bg = c("red","blue"), pch = 22, cex = 6)
	   arrows(dat.g, (graph.data+graph.se), dat.g, (graph.data-graph.se), angle = 90, lwd = 2, lty = 1, code = 0)
	}