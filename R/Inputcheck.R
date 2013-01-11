
Inputcheck<-function(responses,key,format,itemlabels,weights,miss,evalpoints,bandwidth,nitem,nsubj,kernel,NAweight,nevalpoints,thetadist,groups, SubRank){

	
	if(is.na(min(apply(responses,2,sd, na.rm=TRUE)))){
		print("One or more item(s) has no variation in responses (ie. All subjects chose the same option or all responses are missing). Please exclude this item from the responses matrix")
		return(0)
	}
	if(min(apply(responses,2,sd, na.rm=TRUE))==0){
		print("One or more item(s) has no variation in responses (ie. All subjects chose the same option). Please exclude this item from the responses matrix")
		return(0)
	}
	if(!is.null(weights) & !is.null(format)){
		print('Weights and a format/key were input, use one or the other')
		return(0)
	
	}
	if(is.null(weights)){
		if(length(format) != 1){
			if(length(format) != nitem){
			print('format must be a single number or a vector equal to the number of items specifying the format for each.')
			return(0)
			}
			if(length(key) != nitem){
			print('format must either be 1 for all Multiple-choice items (with a key specified for each item), 2 for all Rating scale/Partial credit items (with a key specified); 3 for all nominal items (key omitted); a numeric vector of length equal to the number of items specifing the format for each item (key specified); or omitted with the weights argument specifying the weight for each option and item.')
			return(0)	
		
			}
		}
		else if((format[1] == 1 | format[1] == 2) & length(key) != nitem){
			print('format must either be 1 for all Multiple-choice items (with a key specified for each item), 2 for all Rating scale/Partial credit items (with a key specified); 3 for all nominal items (key omitted); a numeric vector of length equal to the number of items specifing the format for each item (key specified); or omitted with the weights argument specifying the weight for each option and item.')
			return(0)		
		}
		else if(format[1] == 3 & is.null(SubRank)){
			print('If format = 3 then SubRank must be specified')
			return(0)
		}
	}
	if(!is.null(weights)){
		if(class(weights)!="list" | length(weights) != nitem | nrow(weights[[2]])!=2){
		
			print('weights must be a list of length n items. Each list element must have two rows. The first row is the response, the second row is the weight')
			return(0)
			}
		
		}

	if(!is.null(key) & length(key)!=nitem){
		print('key must be of length n items with the correct response for each nominal item or the highest level for each ordinal item')	
		return(0)
	}
	if(!is.null(itemlabels) & length(itemlabels)!=nitem){
		print('itemlabels must be of length items or left blank')
		return(0)
	}
	if(bandwidth[1] != "Silverman" & bandwidth[1] !="CV" & length(bandwidth) != nitem){
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
	if(nevalpoints <0 | !is.numeric(nevalpoints)){
		print('nevalpoints must be numeric greater than 0');
		return(0)
	}
	if(!is.list(thetadist)){
		print('thetadist must be a list with the first element containing the type of distribution ("norm", "beta", "unif", "cauchy", etc.). The other arguments are the parameters of the distribution ');
		return(0)
	}
	if(!is.null(SubRank) & length(SubRank) != nsubj){
		print("SubRank must either be a vector of length equal to the number of subjects with each element specifying the relative rank of each subject or unspecified in which case subjects will be ranked according to the function specified with the RankFun argument")
		return(0)
	}



	return(1)


}

