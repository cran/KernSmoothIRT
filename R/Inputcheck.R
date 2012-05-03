
Inputcheck<-function(responses,key,scale,labs,weights,miss,theta,bandwidth,nitems,nex,kernel,NAweight,nval,enumerate,groups){




	if(scale[1]!="nominal" & scale[1]!="ordinal" & length(scale)!=nitems){
		print('scale is either "nominal" for all nominal, "ordinal" for all ordinal or a vector of length nitems with 1s for nominal and 0s for ordinal')
		return(0)		
	}
	
	if(!is.null(weights) & !is.null(key)){
		print('Both weights and a key were input, use 1 or the other')
		return(0)
	
	}
	if(!is.null(weights)){
		if(class(weights)!="list" | length(weights) != nitems | nrow(weights[[2]]!=2)){
			print('weights must be a list of length n items. Each list element must have two rows. The first row is the response, the second row is the weight')
			return(0)
			}
		
		}

	if(!is.null(key) & length(key)!=nitems){
		print('key must be of length n items with the correct response for each nominal item or the highest level for each ordinal item')	
		return(0)
	}
	if(!is.null(labs) & length(labs)!=nitems){
		print('labs must be of length items or left blank')
		return(0)
	}
	if(bandwidth[1] != "default" & bandwidth[1] !="CV" & length(bandwidth) != nitems){
		print('bandwidth much either be: "CV", "default", a vector of length equal to then number of items or left blank')
		return(0)
	}
	if(kernel != "gaussian" & kernel != "quadratic" & kernel != "uniform"){
		print('kernel must be either: "gaussian", "quadratic", or "uniform"')
		return(0)	
	}
	if(NAweight <0 | !is.numeric(NAweight)){
		print('NAweight must be numeric greater than 0');
		return(0)
	}
	if(nval <0 | !is.numeric(nval)){
		print('nval must be numeric greater than 0');
		return(0)
	}
	if(!is.list(enumerate)){
		print('enumerate must be a list with the first element containing the type of distribution ("norm", "beta", "unif", "cauchy", etc.). The other arguments are the parameters of the distribution ');
		return(0)
	}




	return(1)


}

