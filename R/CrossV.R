CrossV <-
function(answered0,probrank0,kernel0){

	##cat("sTART oPTIMIZE","\n",file="output.txt",append=TRUE)


	Calc<-function(x,band){
	

		blah<-CV(A=band,B=probrank0,C=kerneltog,D=x,E=answered0[,-x])
		blah[which(is.nan(blah) | blah > 1)]<-0
		error<-(1-sum(blah*answered0[,x]))**2
		##cat(blah,"\n",file="output.txt",append=TRUE)
		##cat(error,"\n",file="output.txt",append=TRUE)
		return(error)

	}



	CVstat<-function(x){
		#cat("NEXT","\n",file="output.txt",append=TRUE)	
		band<-x
		CVsamp <- sample(1:length(probrank0),round(length(probrank0)/10),replace=FALSE)
		return(sum(sapply(CVsamp,Calc,band=band)))
	}



		if(kernel0=="gaussian"){kerneltog<-1;}
		if(kernel0=="quadratic"){kerneltog<-2;}
		if(kernel0=="uniform"){kerneltog<-3;}

		here<-optimize(f=CVstat,interval=c(0,1),tol=.05)
		return(here$minimum)


}

