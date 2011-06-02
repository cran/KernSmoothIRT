handlemiss <-
function(x,miss){
	answers<-x

	


	if(miss=="random.multinom"){
		freqbycol<-table(answers)
		
		answers[is.na(answers)]<-as.numeric(names(freqbycol))[which(rmultinom(1,1,freqbycol)==1)]


	}
	
	else if(miss=="random.unif"){
		answers[is.na(answers)]<-sample(na.omit(unique(answers)),1)

	}
	

	else if(miss=="category"){

	
		answers[is.na(answers)]=-1

	
	}
	


	return(answers)



}

