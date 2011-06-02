expectedDIFplot <-
function(OBJ, axis, quants, main, xlim, ylim, xlab, ylab, ...){
		par(ask=TRUE)
		par(oma=c(1,1,1,2))

		if(missing(main)){main="Pairwise Expected Scores\n"}
	
		grps<-OBJ$groups
		ngrps<-length(grps)

		if(missing(xlim)){xlim<-c(min(OBJ$scoresbysubject),max(OBJ$scoresbysubject))}
		if(missing(ylim)){ylim<-c(min(OBJ$scoresbysubject),max(OBJ$scoresbysubject))}


		for(i in 1:(ngrps-1)){
		
			for(j in (i+1):ngrps){

				grp1<-OBJ$subsets[[i]]
				grp2<-OBJ$subsets[[j]]				

				eval1<-apply(grp1$probs[,-c(1:3)],2,function(x)sum(x*grp1$probs[,3]))
				eval2<-apply(grp2$probs[,-c(1:3)],2,function(x)sum(x*grp2$probs[,3]))

				plot(eval1,eval2,type="l",ylab=OBJ$groups[j], xlab=OBJ$groups[i], xlim=xlim, ylim=ylim, main=main, ...)

		
				axis(3,at=quants, lab=labels(quants),tck=0)
				abline(v=quants,col="blue",lty=2)
				axis(4,at=quants, lab=labels(quants),las=2,tck=0)
				abline(h=quants,col="blue",lty=2)
				abline(0,1,lty=2)



			}



		}


	
}

