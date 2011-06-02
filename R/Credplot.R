Credplot <-
function(OBJ,axis,subjects,quants,main,xlab,ylab,...){


	getLik<-function(x,answers){
		

		check<-numeric()

		for(i in 1:length(x)){
			if(answers[i]==0){check[i]<-(1-x[i])}
			else{check[i]<-x[i]}
		}			

		return(prod(check))
	

	}


	if(missing(ylab)){ylab="Relative Credibility"}
	if(missing(subjects)){subjects<-1:OBJ$nex}
	if(missing(main)){main=-1}

	par(ask=TRUE)		

	
	for(i in subjects){
	

		resp1<-matrix(0,ncol=OBJ$nval,nrow=OBJ$nitems)



		for(j in 1:OBJ$nitems){


		responses<-OBJ$binres[which(OBJ$binres[,1]==j),i+3]
		probs<-OBJ$probs[which(OBJ$probs[,1]==j),]
	

		resp1[j,]<-apply(probs[,-c(1:3)],2,getLik,answers=responses)
		
		}

	

	cred<-apply(resp1,2,prod)
	relcred<-cred/max(cred)	
	

	if(main==-1){main0<-paste("Subject: ",i,"\n")}

	plot(axis,relcred,type="l",main=main0,ylab=ylab,xlab=xlab,...)



	
		axis(3,at=quants, lab=labels(quants),tck=0)
		abline(v=quants,col="blue",lty=2)
	
	if(axis[1]==OBJ$theta[1]){


		abline(v=OBJ$probrank[i],col="red")

	}
	else{
		abline(v=OBJ$scoresbysubject[i],col="red")
	}




	}



		
			



}

