ksIRT <-
function(responses,key=NULL,scale=NULL,kernel="gaussian",labs=NULL,weights=NULL,miss="category",NAweight=0,theta=NULL,nval=51,bandwidth="default",enumerate=list("norm",0,1),groups=FALSE){

	if(miss=="omit"){
		nasubs<-apply(responses,2,function(xxx)which(is.na(xxx)))
	}
	else{nasubs<-numeric(0)}
	
	nex <- nrow(responses);
	nitems<-ncol(responses);

	Check<-Inputcheck(responses,key,scale,labs,weights,miss,theta,bandwidth,nitems,nex,kernel,NAweight,nval,enumerate,groups)
	if(Check==0){return(Check)}

	if(!is.null(scale[1])){
		if(scale[1]=="nominal"){
			
			scale<-rep(1,nitems)
		}

		else if(scale[1]=="ordinal"){scale<-rep(0,nitems)}

	}

	if(is.null(labs)){labs<-as.character(seq(from=1, to=nitems))}

	responses<-apply(responses,2,handlemiss,miss=miss)

	optsbyitem<-list()
	for(i in 1:ncol(responses)){
		optsbyitem[[i]] <- unique(responses[,i])
	}

	fullresponses<-matrix(0,length(unlist(optsbyitem)),ncol=3+nex)

		crow<-0
		for(i in 1:nitems){
			for(j in 1:length(optsbyitem[[i]])){
				crow<-crow+1

					fullresponses[crow,1:2]<-c(i,optsbyitem[[c(i,j)]])

					if(!is.null(weights)){	
						fullresponses[crow,3]<-getweight(item=i,option=fullresponses[crow,2],weights=weights[[i]],NAweight=NAweight)
					}
					if(!is.null(key)){
						fullresponses[crow,3]<-getweight(item=i,option=fullresponses[crow,2],scale=scale[i],key=key[i],NAweight=NAweight)}						

				
				}


		}


	fullresponses0<-fullresponses

	if(miss=="omit" & sum(is.na(fullresponses[,2]))){
		fullresponses<-fullresponses[-which(is.na(fullresponses[,2])),]	
	}




	

	optitwgtresp<-make_mat(A=fullresponses,B=responses)
	#print("optitwgt")
	#print(optitwgtresp)


	#return(list(responses,fullresponses,fullresponses0,optitwgtresp))

	ncorrectex<-numeric()

	for(i in 1:nex){
		ncorrectex[i]<-sum(optitwgtresp[,3]*optitwgtresp[,i+3])
	}


	qdist <- get(paste("q",enumerate[[1]],sep=""),mode="function")

	rankscores<-rank(ncorrectex,ties.method="random")

	probrank<-eval(parse(text=(paste("qdist((rankscores)/(nex+1),",paste(unlist(enumerate[-1]),collapse=","),")",sep=""))))

	
	quantstheta<-quantile(probrank,probs=c(.05,.25,.50,.75,.95))

	

	if(is.null(theta)){

			lim1<-qdist(1/(nex+1),enumerate[[2]],enumerate[[3]])
			lim2<-qdist(nex/(nex+1),enumerate[[2]],enumerate[[3]])
			theta<-seq(from=lim1, to=lim2, length.out=nval);

		}
	else{

		nval<-length(theta)

	}

	

	scoresattheta<-quantile(ncorrectex,(1:nval)/nval)
	


	if(bandwidth[1]=="CV"){		
		
		h<-numeric()
		h0<-numeric()
		torep<-table(optitwgtresp[,1])

		for(i in 1:nitems){
			
			mat<-optitwgtresp[which(optitwgtresp[,1]==i),-c(1:3)]	
			#h0[i]<-CrossV3(answered=mat,theta=theta,probrank=probrank,kernel=kernel)
			h0[i]<-CrossV(answered0=mat,probrank0=probrank,kernel0=kernel)
			h<-c(h,rep(h0[i],torep[i]))

		}
	}

	else if(bandwidth[1]=="default"){
		if(enumerate[[1]]=="norm"){
			sighat <- enumerate[[3]]			
			
		}
		else{
			sighat <- sd(probrank)
		}		
			h<-rep(1.06*sighat*nex^(-.2),nrow(optitwgtresp))
			h0<-rep(h[1],nitems)
	}

	else{
		torep<-table(optitwgtresp[,1])
		h0<-bandwidth;
		h<-numeric()

		#print(optitwgtresp)
		#print(h0)
		#print(torep)

		for(i in 1:nitems){
			h<-c(h,rep(h0[i],torep[i]))
		}

	}




	ICC<-matrix(0,nrow=nrow(optitwgtresp),ncol=nval)
	SmthWgts<-matrix(0,nrow=nrow(optitwgtresp),ncol=nval)
	stderr<-matrix(0,nrow=nrow(optitwgtresp),ncol=nval)

	if(kernel=="gaussian"){ktog<-1;}
	if(kernel=="quadratic"){ktog<-2;}
	if(kernel=="uniform"){ktog<-3;}


	for (i in 1:nrow(optitwgtresp)){

		retval<-smoother(A = h[i], B = probrank, C = theta, D = optitwgtresp[i,-c(1:3)], E = ktog) 
		ICC[i,]<-retval[["ICC"]]
		stderr[i,]<-retval[["stderr"]]
		SmthWgts[i,]<-retval[["weights"]]
	}



	Probs<-cbind(optitwgtresp[,c(1:3)],ICC)
	Stderrs<-cbind(optitwgtresp[,c(1:3)],stderr)
	Stderrs[which(is.na(Stderrs))]<-0

	if(groups[1]==FALSE){subsets<-FALSE; grps<-NULL;}
	else{
		grps<-unique(groups)
		subsets<-lapply(grps,function(x)ksIRT(responses=responses[which(groups==x),],key=key,scale=scale,kernel=kernel,labs=labs,weights=weights,miss=miss,NAweight=NAweight,theta=theta,nval=nval,bandwidth=bandwidth,enumerate=enumerate,groups=FALSE))

	}

	evals<-apply(Probs[,-c(1:3)],2,function(x)sum(x*Probs[,3]))
	quantsex<-quantile(ncorrectex,probs=c(.05,.25,.50,.75,.95))


	this<-cbind(optitwgtresp[,1],t(apply(optitwgtresp,1,function(xxx)xxx[-c(1:3)]*xxx[3])))
	corr<-numeric(nitems)


	for(i in 1:nitems){
		thisit<-which(this[,1]==i)
		summed<-apply(this[thisit,-1],2,sum)	
		if(length(nasubs)>0){
			nahere<-nasubs[[i]]
			summed[nahere]<-NA
		}

		corr[i]<-cor(ncorrectex,summed,use="complete.obs")		
	}


	toret<-list(binres=optitwgtresp,probs=Probs,Stderrs=Stderrs,scoresbysubject=ncorrectex,itemlabels=labs,theta=theta,
quantiles=quantsex,quantilestheta=quantstheta,SmthWgts=SmthWgts,scale=scale,enumerate=enumerate,
probrank=probrank,band=h0,nitems=nitems,nex=nex,nval=nval,subsets=subsets,groups=grps,expectedscores=evals,pserial=corr)


	class(toret)<-"ksIRT"

	return(toret)

}