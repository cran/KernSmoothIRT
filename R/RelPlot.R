RelPlot<-function(OBJ, axis, quants, main, xlab, ylab, ...){


	inf<-IICplot(OBJ,axis=axis, quants=quants, test=TRUE,noout=TRUE)


	if(missing(main)){main="Reliability Plot\n"}
	if(missing(ylab)){ylab="Reliability"}
	

	plot(axis,1/(1+1/(inf*OBJ$nitems)),main=main,xlab=xlab,ylab=ylab,type="l",...)
	
		axis(3,at=quants, lab=labels(quants),tck=0)
		abline(v=quants,col="blue",lty=2)


}
