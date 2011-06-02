densityDIFplot <-
function(x,xlim,ylim,xlab,ylab,main,...){

		


		if(missing(main)){main="Observed Score Distribution\n"}
		if(missing(xlab)){xlab="Score"}
		if(missing(xlim)){xlim=c(0,max(x$scoresbysubject))}
		
		if(missing(ylab)){ylab="Density of Score"}
		

		ymax<-max(density(x$scoresbysubject,from=0,to=max(x$scoresbysubject))$y)

		if(missing(ylim)){ylim=c(0,ymax)}


		plot(c(0,max(x$scoresbysubject)),c(0,ymax),type="n",xlim=xlim, ylim=ylim ,xlab=xlab,ylab=ylab,main = main,...)
		ngrps<-length(x$groups)

		plot_colors <- c("blue","red","forestgreen","black","yellow","orange")

		if(ngrps>6){plot_colors<-c(plot_colors,sample(colors(),ngrps-6))}


		line_type<-rep(1:6,ngrps)


		for(i in 1:ngrps){

			cgrp<-x$subsets[[i]]
			lines(density(cgrp$scoresbysubject,from=0,to=max(cgrp$scoresbysubject)),col=plot_colors[i],lty=line_type[i],...)
		
		}

		legend(0, ymax,x$groups, cex=0.8, col=plot_colors[1:ngrps],lty=line_type[1:ngrps], lwd=2, bty="n");

		axis(3,at=x$quantiles, lab=labels(x$quantiles),tck=0)
		abline(v=x$quantiles,col="blue",lty=2)
		box()

}

