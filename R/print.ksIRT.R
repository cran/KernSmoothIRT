print.ksIRT <-
function(x,...){
	toout<-data.frame(Item=1:x$nitems,Correlation=x$pserial)
	print(toout)
}

