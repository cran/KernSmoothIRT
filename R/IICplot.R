IICplot<-function(OBJ, items, quants, axis, test=FALSE, noout=FALSE, main, xlab, ylab, ...){


			if(missing(main)){main=-1}
			if(missing(ylab)){ylab="Information"}
	
		getinfo2<-function(probopt,OBJ){
			P<-probopt
			Q<-1-P

			thetas<-OBJ$theta
			thetassq<-thetas**2
		
			#cat(P,Q,thetas,thetassq,"\n")
			
			
			if(sum(P[which(P>=1 | P<=0)])){
			
				coefs<-c(0,0,0)
			
			}
			else{
				quad<-try(lm(log(P/(1-P))~thetas+thetassq))
				if(class(quad)=="try-error"){
					coefs<-c(0,0,0)
				}
			
				else{
					coefs<-quad$coefficients
					coefs[which(coefs > 10)]<-10
				}
			}
			
			partial<-exp(coefs[1] + coefs[2]*thetas + coefs[3]*thetassq)

			der <- coefs[2] + thetas * 2 * coefs[3]			
			rbind(der,partial)
		}

		plotit<-function(x,OBJ,axis,quants,main,xlab,ylab,...){
			OCC<-OBJ$probs[which(OBJ$probs[,1]==x),]
			OIF<-apply(OCC[,-c(1:3)],1,getinfo2,OBJ=OBJ)
			IIF0<-apply(OIF,1,sum)
			
			piece<-OIF/IIF0

			dzs<-OIF[seq(from=1,to=length(IIF0),by=2),]
			pieces<-piece[seq(from=2,to=length(IIF0),by=2),]									

			moddz<-dzs*pieces
			moddz2<-(dzs**2)*pieces

			itemdz<-apply(moddz,1,sum)
			itemdz2<-apply(moddz2,1,sum)

			IIF<-itemdz2-itemdz**2
				
			if(!test){
				par(ask=TRUE)

			if(main==-1){main=paste("Item: ",OBJ$itemlabels[x],"\n")}



	
				plot(axis,xlab=xlab,ylab=ylab,IIF,main=main,type="l",...)
				
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



			plot(axis,TIF,xlab=xlab,ylab=ylab,main=main,type="l",...)
				
				axis(3,at=quants, lab=labels(quants),tck=0)
				abline(v=quants,col="blue",lty=2)
				
			return(round(TIF,3))
		}
			


}
