IICplot<-function(OBJ, items, quants, axis, test=FALSE, noout=FALSE, main, xlab, ylab, ...){


			if(missing(main)){main=-1}
			if(missing(ylab)){ylab="Information"}
	
		getinfo2<-function(probopt,OBJ){
				inf <- (diff(probopt)^2)/probopt[-1]	
			}
			

		plotit<-function(x,OBJ,axis,quants,main,xlab,ylab,...){
			OCC<-OBJ$probs[which(OBJ$probs[,1]==x),]
			OIF<-apply(OCC[,-c(1:3)],1,getinfo2,OBJ=OBJ)
			IIF<-apply(OIF,1,sum)
				
			if(!test){
				par(ask=TRUE)
				if(main==-1){main=paste("Item: ",OBJ$itemlabels[x],"\n")}
	
				plot(axis[-1],xlab=xlab,ylab=ylab,IIF,main=main,type="l",...)
				
					axis(3,at=quants, lab=labels(quants),tck=0)
					abline(v=quants,col="blue",lty=2)
							
			}
			

			return(round(IIF,3))
		}

		if (test){items=1:OBJ$nitems}

		eachitem<-sapply(items,plotit,OBJ=OBJ,axis=axis,quants=quants,main, xlab, ylab, ...)

		if (test){
			TIF<-apply(eachitem,1,sum)/length(eachitem[1,])
			if(noout==TRUE){return(round(TIF,3))}

			if(main==-1){main="Test Information\n"}



			plot(axis[-1],TIF,xlab=xlab,ylab=ylab,main=main,type="l",...)
				
				axis(3,at=quants, labels=labels(quants),tck=0)
				abline(v=quants,col="blue",lty=2)
				
			return(round(TIF,3))
		}
			


}
