biserialcorr <-
function(OBJ){

	scores<-matrix(0,nrow=OBJ$nitems,ncol=OBJ$nex)

	for(i in 1:OBJ$nitems){
		sub<-OBJ$responses[which(OBJ$responses[,1]==i),]
		subp<-try(apply(sub,1,function(x)return(x[3]*x[-c(1:3)])))
		scores[i,]<-try(apply(subp,1,sum))

	}



	corrs<-try(apply(scores,1,function(x)cor(x,OBJ$scoresbysubject,use="complete.obs")))
	print("Item, total score correlations")
	return(try(cbind(OBJ$itemlabels,round(corrs,digits = 3))))


}

