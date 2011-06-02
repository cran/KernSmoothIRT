print.ksIRT <-
function(x,...){
	toout<-as.data.frame(biserialcorr(x))
	colnames(toout)<-c("Item","Correlation")
	print(toout)
}

