expectedplot <-
function(OBJ, axis, quants, main, xlim, ylim, xlab, ylab, ...){


		if(missing(main)){main="Expected Total Score\n"}
		if(missing(ylab)){ylab="Expected Score"}
		if(missing(ylim)){ylim=c(min(OBJ$scoresbysubject),max(OBJ$scoresbysubject))}
		if(missing(xlim)){xlim=c(min(axis),max(axis))}	


	evals<-apply(OBJ$probs[,-c(1:3)],2,function(x)sum(x*OBJ$probs[,3]))
	
	plot(axis,evals,type="l",ylab=ylab,xlab=xlab,xlim=xlim,ylim=ylim,main=main,...)

	
		axis(3,at=quants, lab=labels(quants),tck=0)
		abline(v=quants,col="blue",lty=2)
	
	return(round(evals,3))
}

