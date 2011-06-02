CrossV3 <-
function(answered,theta,probrank,kernel){
	

		

	if(kernel=="gaussian"){
			numfxn<-function(arg){
				exp((-(arg**2))/2)			
			}
	}
	else if(kernel=="quadratic"){
			numfxn<-function(arg){
				ifelse(abs(arg)<=1,1-arg**2,0)
			}
	}			
	else if (kernel=="uniform"){
			numfxn<-function(arg){
				ifelse(abs(arg)<=1,1,0)
			}
	}



	Calc<-function(x,band){


	
		arg<-(probrank[x] - probrank[-x])/band	

		num<-numfxn(arg)

		denom<-sum(num)
		smoother<-num/denom
	
		blah<-apply(answered[,-x],1,function(x)sum(smoother*x,na.rm=TRUE))
		
		error<-(1-sum(blah*answered[,x]))**2
		

		return(error)
	}
	
	CVstat<-function(x){
		band<-x
		return(sum(sapply(1:length(probrank),Calc,band=band)))
		
	}
		
	here<-optimize(f=CVstat,interval=c(0,1),tol=.05)
	return(here$minimum)


}

