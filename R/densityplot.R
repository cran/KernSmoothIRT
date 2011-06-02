densityplot <-
function(x,xlim,ylim,xlab,ylab,main,...){



		if(missing(main)){main="Observed Score Distribution\n"}
		if(missing(xlab)){xlab="Score"}
		if(missing(xlim)){xlim=c(0,max(x$scoresbysubject))}
		
		if(missing(ylab)){ylab="Density of Score"}
		

		ymax<-max(density(x$scoresbysubject,from=0,to=max(x$scoresbysubject))$y)


		if(missing(ylim)){ylim=c(0,ymax)}
	
	plot(density(x$scoresbysubject,from=0,to=max(x$scoresbysubject)),xlim=xlim,ylim=ylim,ylab=ylab,xlab=xlab,main = main,...)

		axis(3,at=x$quantiles, lab=labels(x$quantiles),tck=0)
		abline(v=x$quantiles,col="blue",lty=2)
		box()

}

