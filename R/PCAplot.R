PCAplot<-function(x,...){

	nval   <- x$nval        # number of ticks on the x-axis
	nitems <- x$nitems	# numberof items
	theta  <- x$theta	# ticks on the x-axis

	X <- matrix(0,nitems,nval)       # (nitems x nval)-matrix referred to the nitems ICCs

	for(i in 1:nitems){
		

		lines0<-x$probs[which(x$probs[,1]==i),]


		lines1<-apply(lines0[,-c(1:3)],2,function(x)x*lines0[,3])

		if(x$scale[i]==0){
			maxitem<-max(lines0[,3])
			X[i,]<-apply(lines1,2,sum)/maxitem
		}
		else{

			X[i,]<-apply(lines1,2,sum)
		
		}
		
	}


	Z <- X-matrix(rep(colMeans(X),nitems),nitems,nval,byrow=T) # Centered items (TestGraf95, p. 64)
prcomp(Z,scale = F) -> pcX                # Principal Component Analysis of X

################################
## PLOT TO ADD IN THE PACKAGE ##
################################

evalpoint <- min(theta)+(max(theta)-min(theta))/10
par(fig=c(0.22,0.82,0.22,0.82))   
par(mai=c(0.05,0.05,0.05,0.05)) 
par(omi=c(0,0,0,0))
par(oma=c(1,1,1,1))
par(cex.axis=0.5)
plot(pcX$x[,1],pcX$x[,2],type="n",...)
box()
text(pcX$x[,1],pcX$x[,2],labels = 1:nitems,cex=0.7)
abline(v=0,lty=2)
abline(h=0,lty=2)
par(fig=c(0,0.16,0.42,0.58),new=TRUE)
plot(theta,X[which(pcX$x[,1]==min(pcX$x[,1])),],type="l",xlab="",ylab="",ylim=c(0,1),axes=F)
text(evalpoint,y=0.9,labels=paste(which(pcX$x[,1]==min(pcX$x[,1])),sep=""),cex=0.6)
box()
par(fig=c(0.84,1,0.42,0.58),new=TRUE)
plot(theta,X[which(pcX$x[,1]==max(pcX$x[,1])),],type="l",xlab="",ylab="",ylim=c(0,1),axes=F)
text(evalpoint,y=0.9,labels=paste(which(pcX$x[,1]==max(pcX$x[,1])),sep=""),cex=0.6)
box()
par(fig=c(0.42,0.58,0,0.16),new=TRUE)
plot(theta,X[which(pcX$x[,2]==min(pcX$x[,2])),],type="l",xlab="",ylab="",ylim=c(0,1),axes=F)
text(evalpoint,y=0.9,labels=paste(which(pcX$x[,2]==min(pcX$x[,2])),sep=""),cex=0.6)
box()
par(fig=c(0.42,0.58,0.84,1),new=TRUE)
plot(theta,X[which(pcX$x[,2]==max(pcX$x[,2])),],type="l",xlab="",ylab="",ylim=c(0,1),axes=F)
text(evalpoint,y=0.9,labels=paste(which(pcX$x[,2]==max(pcX$x[,2])),sep=""),cex=0.6)
box()

par(mfrow=c(1,1))
}
