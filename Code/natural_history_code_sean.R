#################################
#libraries
###################################
library(msm)
library(ggplot2)
library(reshape2)
library(nloptr)
##################################
#read in the data
read_data<-function(filepath,mulitplier=1){
#browser()
  the_data=read.csv(filepath)
the_data$PY=the_data$Pop*the_data$years
the_data$rate=mulitplier*the_data$Count/the_data$PY*1e5
the_data$Count=the_data$Count*mulitplier
the_data=the_data[the_data$midage<80,]
return(the_data)
}

get_rate_mat<-function(params,k,mst,dmst){
 # browser()
  rate_mat=matrix(0,nrow=k, ncol=k)
  for(i in 1:(k-3)){
    rate_mat[i,i+1]=exp(params[i])
  }
  c=1/dmst
  b=rate_mat[k-3,k-2]
  rate_mat[k-3,k-1]=(c+b)/(c*mst)-b
  a=rate_mat[k-3,k-1]
  rate_mat[k-2,k]=c
  print(b/(c*(a+b))+1/(a+b))
  
  diag(rate_mat)=-apply(rate_mat,1,"sum")
  return(rate_mat)
}

#Compute the cause specific hazard##
cause_spec_hazards<-function(age,rate_mat,k){
 # browser()
  prob_mat=MatrixExp(t=age,mat=rate_mat)
  prob_l=prob_mat[1,(k-1)]
  prob_d=prob_mat[1,(k)]
  denom=1-(prob_mat[1,(k-1)]+prob_mat[1,k])
  num_L=prob_mat[1,(k-3)]*rate_mat[(k-3),k-1]
  h_L=num_L/denom
  num_D=prob_mat[1,(k-2)]*rate_mat[(k-2),(k)]
  h_D=num_D/denom
  return(list(h_L=h_L,h_D=h_D,dens_L=num_L, dens_D=num_D,prob_L=prob_l,prob_D=prob_d))
}

#Compute probability of cancer incidence by age?
sum_stats_age<-function(age,rate_mat,clin_cancer=F){
  k=dim(rate_mat)[1]
  prob_mat=MatrixExp(t=age,mat=rate_mat)
  
  out=cause_spec_hazards(age,rate_mat,k)
  prob_clin_cancer=out$prob_D+out$prob_L
  prob_cancer=prob_mat[1,(k-3)]+prob_mat[1,(k-2)]+prob_mat[1,(k-1)]+prob_mat[1,(k)]
 # browser()
  if(clin_cancer==F){
    return(c(prob_cancer))
  }else{
    return(c(prob_clin_cancer))
  }
    #c(prob_clin_cancer)
  
}


likelihood_fun<-function(params, mst, dmst,k,
               allage,allobserved,allPY,allstage){

  rate_mat=get_rate_mat(params,k, mst,dmst)
  loglike=mapply(allage,allobserved,allPY,allstage,FUN="indiv_likelihood",MoreArgs = list(rate_mat=rate_mat,k=k))
 
  # browser()
  out_LL=sum(unlist(loglike))
  return(out_LL)
}

indiv_likelihood<-function(age,observed,PY,stage,
                 rate_mat,k){

  haz_out=cause_spec_hazards(age=age,rate_mat=rate_mat,k=k)

  if(stage=="local"||stage=="Early"){  
  thehaz=haz_out$h_L
  }else{
  thehaz=haz_out$h_D
}
  themean=PY*thehaz

  loglike=observed*log(themean)-themean
  return(loglike)
}


get_max_LL<-function(the_data,mst, num_seeds,k,dmst){
  c=1/dmst
  M=mst
  if(c<=1/M){print("Error: dmst must be less than mst")
    return()}

  out_list=list()
  for(i in 1:num_seeds){

  out_list[[i]]=optim(par=-1*abs(rnorm(n=(k-3),mean=3)), fn=likelihood_fun,method="L-BFGS",lower=rep(-Inf,(k-3)),upper = c(rep(Inf, (k-4)),
                        log(c/(c*M-1))),mst=mst,dmst=dmst,k=k, allage=the_data$midage,allobserved=the_data$Count,
                       allPY=the_data$PY,allstage=the_data$stage,control=list(fnscale=-1, trace=2,maxit=9000))
  }
  maxLL=which.max(unlist(lapply(out_list,"[[",c("value"))))
  maxout=out_list[[maxLL]]
  return(maxout)
}

plot_observed_expected<-function(the_data,fit,mst,dmst,k){
  b=exp(fit$par[k-3])
  c=1/dmst
  a=(c+b)/(c*mst)-b
  
  sojourn_time=b/(c*(a+b))+1/(a+b)
  earlypreclin=1/(a+b)
  the_data$stage=factor(the_data$stage)
  rate_mat=get_rate_mat(fit$par,k=k, mst=mst,dmst=dmst)
  pred_cause_spec_hazards<-mapply(the_data$midage[1:17],FUN="cause_spec_hazards",MoreArgs=list(rate_mat=rate_mat,k=k))
  the_data$pred=c(unlist(pred_cause_spec_hazards[2,])*1e5,unlist(pred_cause_spec_hazards[1,])*1e5)
  the_data=melt(the_data,measure.vars = c("pred","rate"))
  #browser()
  the_data$Legend=the_data$variable
  the_data$Legend=factor(the_data$Legend,labels=c("Predicted rate","Observed rate"))
  p1=ggplot(data=the_data,aes(x=midage,y=value))+geom_point(aes(color=Legend))+geom_line(aes(color=Legend,group=Legend))+
  facet_grid(~stage)+xlab("Age")+ylab("Rate/100,000")+ggtitle(paste("OMST=",mst,", ",
                                                                    "LMST =",dmst,sep=""))
  

  
  p2=ggplot(data=subset(the_data,variable=="rate"),aes(x=midage,y=value))+geom_point(aes(color=variable))+geom_line(aes(color=variable,group=variable))+
    facet_grid(~stage)+xlab("Age")+ylab("Rate/100,000")+ggtitle("Observed Cancer Incidence Data (SEER)")
  
  return(list(p1=p1,the_data=the_data))
  
  
}

summarize_fit<-function(the_data,fit,mst,dmst,k){

  b=exp(fit$par[k-3])
  c=1/dmst
  a=(c+b)/(c*mst)-b

  
  sojourn_time=b/(c*(a+b))+1/(a+b)
  preclinearly=1/(a+b)
  out=list(a=a,b=b,sojourn_time=sojourn_time,mst=mst,latemst=dmst,preclinearly=preclinearly)
  return(out)
}

get_fit<-function(mst=mst,dmst=dmst,the_data=the_data,num_seeds=num_seeds,k=k){
  print(k)
  the_fit=tryCatch(get_max_LL(the_data=the_data,num_seeds=num_seeds,mst=mst,dmst=dmst,k=k),error = function(e) e)
  while(any(class(the_fit)=="error")){
    the_fit=tryCatch(get_max_LL(the_data=the_data,num_seeds=num_seeds,mst=mst,dmst=dmst,k=k),error = function(e) e)
  }
  #browser()
  outplot=plot_observed_expected(the_data=the_data,fit=the_fit,mst=mst,dmst=dmst,k=k)
  rate.matrix=get_rate_mat(params=the_fit$par,k=k,mst=mst,dmst=dmst)

  summary_out=summarize_fit(the_data=the_data,fit=the_fit,mst=mst,dmst=dmst,k=k)
  return(list(the_fit=the_fit,outplot=outplot$p1,fitted_data=outplot$the_data,rate.matrix=rate.matrix,summary_out=summary_out))
}