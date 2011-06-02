plot.ksIRT <-
function(x, plottype = "OCC", items="all", subjects, axistype = "distribution", alpha, main, xlab, ylab, xlim, ylim, cex,...){


	if(items[1]=="all"){items<-1:x$nitems}


	if(axistype=='distribution'){
		axis<-x$theta
		quants<-x$quantilestheta
		if(missing(xlab)){
			xlab<-paste(simpleCap(x$enumerate[[1]]), "Quantiles",sep=" ")
		}
	}
	else{
		axis<-x$scoresattheta
		quants<-x$quantiles
		if(missing(xlab)){	
			xlab<-"Expected Score"
		}
	}


	if(plottype=="density"){densityplot(x,xlim,ylim,xlab,ylab,main,...)}
	else if(plottype=="ICC"){ICCplot(x,items,alpha,axis=axis,quants=quants,main,xlab,ylab,xlim,ylim,cex,...)}
	else if(plottype=="OCC"){OCCplot(x,items,alpha,axis=axis,quants=quants,main,xlab,ylab,xlim,ylim,...)}
	else if(plottype=="expected"){expectedplot(x,axis=axis,quants=quants, main, xlim,ylim, xlab, ylab, ...)}
	else if(plottype=="sd"){sdplot(x,axis=axis,quants=quants,main, xlab, ylab,...)}
	else if(plottype=="info"){IICplot(x,items,axis=axis,quants=quants,test=FALSE,noout=FALSE, main, xlab, ylab, ...)}
	else if(plottype=="testinfo"){IICplot(x,items,axis=axis,quants=quants,test=TRUE,noout=FALSE, main, xlab, ylab, ...)}
	else if(plottype=="se"){SEplot(x,axis=axis,quants=quants,main, xlab, ylab, ...)}
	else if(plottype=="reliability"){RelPlot(x,axis=axis,quants=quants,main,xlab,ylab,...)}
	else if(plottype=="triangle"){Simplexplot(x,items,main, ...)}
	else if(plottype=="tetrahedron"){Tetplot(x,items,main, ...)}
	else if(plottype=="credibility"){Credplot(x,axis=axis,quants=quants,xlab=xlab,subjects=subjects,...)}
	else if(plottype=="ICCDIF"){ICCDIFplot(x,items,alpha,axis=axis,quants=quants,main,xlab,ylab,xlim,ylim,cex,...)}
	else if(plottype=="OCCDIF"){
	OCCDIFplot(x,items,alpha,axis=axis,quants=quants,main,xlab,ylab,xlim,ylim,...)
	}
	else if(plottype=="PCA"){PCAplot(x, ...)}
	else if(plottype=="expectedDIF"){expectedDIFplot(x,axis=x$scoresattheta,quants=x$quantiles, main, xlim,ylim, xlab, ylab, ...)}
	else if(plottype=="densityDIF"){densityDIFplot(x,xlim,ylim,xlab,ylab,main,...)}
	else{print("plottype not recognized")}
	
}

