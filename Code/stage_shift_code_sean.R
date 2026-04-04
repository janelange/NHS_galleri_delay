######################################################
# Functions to obtain analytic stage shifts
####################################################
library(msm)
library(dplyr)

get_control_late<-function(rate.matrix,a1,a2){
#####################################################################
#author JL 
#
#This function gets the probability of being diagnosed with distant stage disease
# between ages a1 and a2 in the control arm
#
#INPUTS: a1: start age
#        a2: end age
#        rate.matrix=rate matrix
#OUTPUTS: the probability of begin diagnosed with distant stage disease
########################################################################
  
  #browser()
  prob_mat_a1=MatrixExp(t=a1,mat=rate.matrix)
  prob_mat_a2=MatrixExp(t=a2,mat=rate.matrix)
  
  k=dim(rate.matrix)[1]
  distant_prob=(prob_mat_a2[1,(k)]-prob_mat_a1[1,(k)])/(1-prob_mat_a1[1,k]-prob_mat_a1[1,(k-1)])
  return(distant_prob)
}
  
get_init<-function(rate.matrix,a1){
#####################################################################
#author JL 
#
#This function gets the initial distribution given no clinical disease diagnosed by
# age  a1
#INPUTS: a1: age at beginning of trial
#        rate.matrix=rate matrix governing natural history
#OUTPUTS: the probability of occupying each latent stage by age a1
########################################################################
  

  k=dim(rate.matrix)[1]
  init=vector()
  init[k]=0
  init[k-1]=0


  prob_mat_a1=MatrixExp(t=a1,mat=rate.matrix)
  denom=1-prob_mat_a1[1,(k-1)]-prob_mat_a1[1,(k)]
  for(j in 1:(k-2)){

    init[j]=prob_mat_a1[1,j]/denom
  }

  return(init)

}

get_emission<-function(k,sens_e,sens_l){
#####################################################################
#author JL 
#
#This function gets the emission distribution given sensitivity for early and late disease
#
#INPUTS: k: number of states in natural history model
#        sens_e: sensitivity for early stage disease
#        sens_l: sensitivity for late stage disease
#OUTPUTS: a matrix with the emission probablities.
#        the ith row corresponds to the hidden value X(t)=i, and the kth column to O(t)=k|X(t)=i
#        thus the rows sum to 1, and k columns correspond to the k possible observed states
########################################################################
  emit=matrix(0,nrow=k,ncol=5)
  emit[1:(k-4),1]=1
  emit[(k-3),2]=sens_e
  emit[(k-3),1]=1-sens_e
  emit[(k-2),3]=sens_l
  emit[(k-2),1]=1-sens_l
  
  
  emit[(k-1),4]=1
  emit[k,5]=1
  return(emit)
}

get_the_data_post<-function(screen_times, extended_followup){
#####################################################################
#author JL 
#
#This function gets the sequence of observations corresponding to late stage clinical diagnosis 
#      after the last screen
#
#INPUTS: screen_times: times of the screens
#        extended_followup: is there extended followup after the last screen?
#OUTPUTS: a list with the observed data corresponding to a late stage clinical diagnosis at the end of the trial
########################################################################
  
  the_data=list()
  the_data[[1]]=c(rep(1,times=length(screen_times)),5)
  return(the_data)
}

get_the_data<-function(screen_times, preclin_distant=FALSE,extended_followup=NULL){
#####################################################################
#author JL 
#
#This function gets a list of sequences of observed data corresponding to screen or clinical distant stage diagnoses
#      at the screens or after the last screen. 
#   
#
#INPUTS: screen_times: times of the screens
#        preclin_distant: are we capturing screen-detected pre-clinical late stage cancers or clinically detected late stage cancers?
#        extended_followup: is there extended followup after the last screen?
#OUTPUTS: a list with the observed data corresponding to a late stage clinical diagnosis at the end of the trial
########################################################################
  
  n=length(screen_times)
  the_data=list()
  
  if(length(screen_times)==1){
    if(preclin_distant==T){
      the_data[[1]]=c(3)
    }
    if(preclin_distant==F&is.null(extended_followup)){
      print("Warning: neeed extended followup after single exam")
    }
    if(preclin_distant==F&!is.null(extended_followup)){
      the_data[[1]]=c(1,5)
    }
    
  }else{
  if(!(preclin_distant)){
  for(i in 1:(n-1)){
    the_data[[i]]=c(rep(1,times=i),5)
  }
    if(!is.null(extended_followup)){
      k=length(the_data)
      the_data[[k+1]]=c(rep(1,times=n),5)
    }
   }else{
    the_data[[1]]=c(3)
    for(i in 1:(n-1)){
      the_data[[i+1]]=c(rep(1,times=i),3)
    }
   }

  }
  return(the_data)
}

get_times_post_trial<-function(screen.times,extended_followup){
#####################################################################
#author JL 
#
#This function gets the times of a sequence of data corresponding to 
# screens + extended followup
#   
#
#INPUTS: screen_times: times of the screens
#        extended_followup: length of extended follow-up after the last screen?
#OUTPUTS: a list with the times corresponding to the screens and the extended followup at the end of the trial. 
########################################################################
  
  the_times=list()
  the_times[[1]]=c(screen.times,max(screen.times)+extended_followup)
  return(the_times)
}


get_times_list<-function(screen_times,preclin_distant=FALSE,extended_followup=NULL){
#####################################################################
#author JL 
#
#This function gets list of times for corresponding to
# a diagnosis at each screen/interval (preceded by negative screens in prior intervals), with the possibility of 
# including diagnoses after the last screen during extended followup. 
#
#INPUTS: screen_times: times of the screens
#        extended_followup: is there extended followup after the last screen?
#OUTPUTS: a list with the times corresponding to the screens and the extended followup at the end of the trial. 
########################################################################
  
  n=length(screen_times)
  the_times=list()
  
  
  if(length(screen_times)==1){
    if(preclin_distant==T){
      the_times[[1]]=c(0)
    }
    if(preclin_distant==F&is.null(extended_followup)){
      print("Warning: neeed extended followup after single exam")
    }
    if(preclin_distant==F&!is.null(extended_followup)){
      the_times[[1]]=c(0,extended_followup)
    }
    
  }else{
  if(!(preclin_distant)){
    for(i in 1:(n-1)){
        the_times[[i]]=screen_times[1:(i+1)]
    }
    if(!is.null(extended_followup)){
      k=length(the_times)
      the_times[[k+1]]=c(screen_times,tail(screen_times,1)+extended_followup)
    }
   }else{
     for(i in 1:(n)){
       the_times[[i]]=screen_times[1:i]
     }
   }
  }

  return(the_times)
  
}

get_screen_late<-function(sens_e,sens_l, screen.times,a1,rate.matrix,preclin_distant=FALSE,extended_followup=NULL){
#####################################################################
#author JL 
#
#This function gets the probability of a late stage cancer in the screen arm during the screening period+1 interval of extended followup.
#   
#
#INPUTS: sens_e: early stage sensitivity
#        sens_l: late stage sensitivity
#        screen.times=times of screens
#        a1: age first screen
#        rate.matrix: rate matrix governing natural history
#        preclin_distant: are we capturing screen detected cancers or interval cancers?
#        extended_followup: years of extended followup after the last screen
#OUTPUTS: probability of late stage cancer in the screen arm
########################################################################
  
  k=dim(rate.matrix)[1]

  init.dist=get_init(rate.matrix,a1)

  emission.dist=get_emission(k,sens_l=sens_l,sens_e=sens_e)
  obs.data.list=get_the_data(screen.times,preclin_distant,extended_followup)
  obs.times.list=get_times_list(screen.times,preclin_distant,extended_followup)
  

  init.list=list()
  emission.list=list()
  rates.list=list()


  for(i in 1:length(obs.data.list)){
  init.list[[i]]=init.dist
  emission.list[[i]]=emission.dist
  rates.list[[i]]=rate.matrix
  }
  
 # browser()
  likelihood(rates.list=rates.list,
             init.list=init.list,
             emission.list=emission.list,
             obs.data.list=obs.data.list,
             obs.times.list=obs.times.list)

}


get_screen_post<-function(sens_e,sens_l, screen.times,a1,rate.matrix,extended_followup){
#####################################################################
#author JL 
#
#This function gets the probability of a late stage cancer in the screen arm after the screening period. 
#   
#
#INPUTS: sens_e: early stage sensitivity
#        sens_l: late stage sensitivity
#        screen.times=times of screens
#        a1: age first screen
#        rate.matrix: rate matrix governing natural history
#        extended_followup: years of extended followup after the last screen
#OUTPUTS: probability of late stage cancer in the screen arm
########################################################################
  
  k=dim(rate.matrix)[1]
  
  init.dist=get_init(rate.matrix,a1)
  
  emission.dist=get_emission(k,sens_l=sens_l,sens_e=sens_e)
 
  
  obs.data.list=get_the_data_post(screen.times,extended_followup)
  
  
  obs.times.list=get_times_post_trial(screen.times,extended_followup)
  
 # browser()
  init.list=list()
  emission.list=list()
  rates.list=list()
  
  
  for(i in 1:length(obs.data.list)){
    init.list[[i]]=init.dist
    emission.list[[i]]=emission.dist
    rates.list[[i]]=rate.matrix
  }
  
  
  likelihood(rates.list=rates.list,
             init.list=init.list,
             emission.list=emission.list,
             obs.data.list=obs.data.list,
             obs.times.list=obs.times.list)
  
}


transition.prob.list<-function(time.intervals, rate.matrix){
#####################################################################
#author JL 
#
#This function gets a list of transition probability matrices for several
#time intervals, assuming a time-homogeneous CTMC
#i.e., P(t2-t1), P(t3-t2), ...P(tn-t_{n-1})
#If exact observation times is indicated at time l>1, then set
# equal toP[[l-1]] times rate matrix(diag=0)
#
#INPUTS: time.intervals t2-t1, t3-t2, ...,t_n-t_n-1
#        rate.matrix=rate matrix
#OUTPUTS: trnsition.prob.list = list of transition probability matrices
###########################################################
  

  probs.list<-lapply(time.intervals, FUN="MatrixExp", mat=rate.matrix)
  return(probs.list)
}
#####################################################################################

transition.prob.all<-function(time.intervals.list, rate.matrix.list){
#####################################################################
#author JL 
#
#This function gets a list of transition probability matrices for several
#time intervals, assuming a time-homogeneous CTMC
#i.e., P(t2-t1), P(t3-t2), ...P(tn-t_{n-1})
#If exact observation times is indicated at time l>1, then set
# equal toP[[l-1]] times rate matrix(diag=0)
#
#INPUTS: time.intervals t2-t1, t3-t2, ...,t_n-t_n-1
#        rate.matrix=rate matrix
#OUTPUTS: transition.prob.list = list of transition probability matrices
###########################################################
  
  
  probs.all<-mapply(time.intervals.list, rate.matrix.list, FUN="transition.prob.list", SIMPLIFY=FALSE)
  return(probs.all)
}
#####################################################################################
forwardback<-function(x, Pi, delta, emission.matrix){
#################################################################################
#Author: JL
#This function computes forward and backward probabilties and observed data log likelihood
# for a hidden markov model
#INPUTS: x=observed data
#        Pi=list of transition probabilities from t1-t2, t2-t3,...,t_n-1-t_n
#        delta = vector corresponding to initial distribution of hidden states
#
#        emmision.matrix=a matrix with the emission probablities.
#        the ith row corresponds to the hidden value X(t)=i, and the kth column to O(t)=k|X(t)=i
#        thus the rows sum to 1, and k columns correspond to the k possible observed states
#OUTPUTS: a list with logalpha (log of forward probabilities), logbeta (log of backward probs),
#         and LL (log likelihoodd of the observed data)
####################################################################################
 if(length(x)==1){
   LL=log(sum(emission.matrix[,x]*delta))
   return(list(LL=LL))
 }else{
  m <- nrow(Pi[[1]])
  #m <- nrow(Pi)
  n <- length(x)
  phi <- matrix(delta, nrow = 1)
  logalpha <- matrix(rep(NA, m * n), nrow = n)
  lscale <- 0
  for (i in 1:n) {
    if (i > 1){
      phi <- phi %*% Pi[[i-1]]}
    phi <- phi %*% diag(emission.matrix[,x[i]])
   # print(c(phi,"\n"))
    sumphi <- sum(phi)
    phi <- phi/sumphi
    lscale <- lscale + log(sumphi)
    logalpha[i, ] <- log(phi) + lscale
  }
  LL1=lscale
  
  logbeta <- matrix(rep(NA, m * n), nrow = n)
  logbeta[n, ] <- 0
  phi <- matrix(rep(1/m, m), ncol = 1)
  lscale <- log(m)
  for (i in seq(n - 1, 1, -1)) {
    phi <- Pi[[i]] %*% diag(emission.matrix[,x[i+1]]) %*% phi
    logbeta[i, ] <- log(phi) + lscale
    sumphi <- sum(phi)
    phi <- phi/sumphi
    lscale <- lscale + log(sumphi)
  }
 
  return(list(logalpha = logalpha, logbeta = logbeta, LL=LL1))
 }
}
#############################################################################
#Author: JL
#This function computes the likelihood for list of sequences of data
#INPUTS: rates.list=list of rate matrices
#        init.list=list of initial distributions
#        emision.list=list of matrices with the emission probablities.
#        the ith row corresponds to the hidden value X(t)=i, and the kth column to O(t)=k|X(t)=i
#        thus the rows sum to 1, and k columns correspond to the k possible observed states
#        obs.data.list=list of sequences of data
#        obs.times.list=list of observation times
#OUTPUTS: probability of observed sequences of data
#############################################################################

likelihood<-function(rates.list,init.list,emission.list,obs.data.list,obs.times.list){
 
  time.diffs.list <- lapply(obs.times.list,FUN = "diff")
  
  transition.probabilities.list = transition.prob.all(time.diffs.list, rates.list)

  likelihood.forward.backward.list2 <- mapply(obs.data.list, 
                                              transition.probabilities.list, init.list, emission.list, 
                                              FUN = "forwardback", 
                                              SIMPLIFY = F)
    
  

  LL2=unlist(lapply(likelihood.forward.backward.list2,"[[",c("LL")))
  prob2=sum(exp(LL2))
  return(prob2)
}

stage_shift_post_screens<-function(sens_e,sens_l,screen.int,a1,a2,rate.matrix,extended_followup){
#####################################################################
#author JL 
#
#This function gets the stage shift observed across the entire trial. 
#   
#
#INPUTS: sens_e: early stage sensitivity
#        sens_l: late stage sensitivity
#        screen.times=times of screens
#        a1: age first screen
#        rate.matrix: rate matrix governing natural history
#        preclin_distant: are we capturing screen detected cancers or interval cancers?
#        extended_followup: years of extended followup after the last screen
#OUTPUTS: stage shift at the end of the trial
  ########################################################################
  
  screen.times=seq(0,a2-a1,by=screen.int)

  control=get_control_late(rate.matrix,a1=a1,a2=(a2+extended_followup))
 
  screen_int=get_screen_late(rate.matrix=rate.matrix,a1=a1,screen.times=screen.times,
                             sens_e=sens_e,sens_l=sens_l,preclin_distant=FALSE,extended_followup = extended_followup)
  
  
  screen_dist=get_screen_late(rate.matrix=rate.matrix,a1=a1,screen.times=screen.times,
                              sens_e=sens_e,sens_l=sens_l,preclin_distant=TRUE,extended_followup = extended_followup)
  screen_total=screen_int+screen_dist
  # browser()
  
  per_reduction=100*(control-screen_total)/control
  
  screen_post=get_screen_post(rate.matrix=rate.matrix,a1=a1,screen.times=screen.times,
                              sens_e=sens_e,sens_l=sens_l,extended_followup = extended_followup)
  ratio=screen_total/control
  
  return(list(per_reduction=per_reduction,ratio=ratio,screen_int=screen_int, screen_dist=screen_dist,screen_late=screen_total,
              control_late=control,screen_post=screen_post,screen_times=screen.times))
}





stage_shift<-function(sens_e,sens_l,screen.int,a1,a2,rate.matrix,extended_followup=NULL){
#####################################################################
#author JL 
#
#This function gets the stage shift observed across the entire trial. 
#   
#
#INPUTS: sens_e: early stage sensitivity
#        sens_l: late stage sensitivity
#        screen.int=screening interval
#        a1: age at first screen
#        a2: age last screen
#        rate.matrix: rate matrix governing natural history
#        extended_followup: years of extended followup after the last screen
#OUTPUTS: stage shift at the end of the trial
########################################################################
  
  screen.times=seq(0,a2-a1,by=screen.int)
  if(is.null(extended_followup)){

  control=get_control_late(rate.matrix,a1=a1,a2=a2)
  }else{
    
  control=get_control_late(rate.matrix,a1=a1,a2=(a2+extended_followup))
  }


  screen_int=get_screen_late(rate.matrix=rate.matrix,a1=a1,screen.times=screen.times,
                                  sens_e=sens_e,sens_l=sens_l,preclin_distant=FALSE,extended_followup = extended_followup)


  screen_dist=get_screen_late(rate.matrix=rate.matrix,a1=a1,screen.times=screen.times,
                              sens_e=sens_e,sens_l=sens_l,preclin_distant=TRUE,extended_followup = extended_followup)
  screen=screen_int+screen_dist
 # browser()
  
  per_reduction=100*(control-screen)/control
  ratio=screen/control
  
  return(list(per_reduction=per_reduction,ratio=ratio,screen_int=screen_int, screen_dist=screen_dist,screen_late=screen,control_late=control,
              screen_times=screen.times))

}

##################################################################################################
#Get the stage shifts across a range of sensitivities 
#####################################################################
#author JL 
#
#This function gets the stage shift observed across the entire trial across a range of sensitiviites 
#   
#
#INPUTS: sensitivities_e: vector of early stage sensitivities
#        screen.int: screening interval
#        a1: age first screen
#        a2: age at last screen
#        fit: list object with item "rate": rate matrix governing natural history
#        extended_followup: years of extended followup after the last screen
#        postscreen: are we capturing stage shifts in intervals after the last screen
#OUTPUTS: stage shift at the end of the trial
########################################################################

stage_shift_sensitivity=function(fit,sensitivities_e,sens_l,screen.int,
                                 a1,a2,
                                 rate.matrix,
                                 extended_followup=NULL,postscreen=F){
  
  if(!(postscreen)){
   # browser()
    out=lapply(sensitivities_e,FUN="stage_shift", screen.int=screen.int,sens_l=sens_l,
             a1=a1,a2=a2,rate.matrix=fit,
             extended_followup=extended_followup)
    screen_post=NA
    time="screen"
  }else{
    out=lapply(sensitivities_e,FUN="stage_shift_post_screens", screen.int=screen.int,sens_l=sens_l,
               a1=a1,a2=a2,rate.matrix=fit,
               extended_followup=extended_followup)
    screen_post=unlist(lapply(out,"[[","screen_post"))
    time="screenpost"
  }
  stage_shift=unlist(lapply(out,"[[","per_reduction"))
  control_late=unlist(lapply(out,"[[","control_late"))
  screen_late=unlist(lapply(out,"[[","screen_late"))
  screen_int=unlist(lapply(out,"[[","screen_int"))
  screen_dist=unlist(lapply(out,"[[","screen_dist"))
  
  sensitivity_e=sensitivities_e
#  mst=fit$mst
#  latemst=fit$latemst
#  preclinearly=fit$preclinearly
#  browser()
   outdata=data.frame(sens_e=sensitivity_e,sens_l=sens_l,stage_shift=stage_shift,control_late=control_late, screen_late=screen_dist, interval_late=screen_int, 
                     screen_post=screen_post,
                     #mst=mst,latemst=latemst,preclinearly=preclinearly,
                     a1=a1,a2=a2,screen.int=screen.int,
                     extended_followup=extended_followup,time=time)
 
  return(outdata)
}

##################################################################################################
#Get the stage shifts by interval
#####################################################################
#author JL 
#
#This function gets the stage shift observed across the entire trial by interval 
#   
#
#INPUTS: sensitivities_e: vector of early stage sensitivities
#        numscreens: number of screens
#        screen.int: screening interval
#        age: age first screen
#        rateout: list with item "rate" containing rate matrix governing natural history
#        num.followup.intervals: number of followup intervals
#OUTPUTS: stage shift at the end of the trial
########################################################################

stage_shift_by_screen<-function(numscreens,age,screen.int,rateout,sensitivities_e,sens_l,num.followup.intervals){
  

  shift_list=list()
  shift_followup_list=list()
  for(i in 1:numscreens){
    
    a1=age
    a2=age+screen.int*i-screen.int
    
    shift_list[[i]]=stage_shift_sensitivity(fit=rateout,sensitivities_e=sensitivities_e, sens_l=sens_l, screen.int=screen.int,
                                            a1=a1,a2=a2,extended_followup = screen.int)
    shift_list[[i]]$interval=i
    
  }
  
  for(i in 1:num.followup.intervals){
    
    shift_followup_list[[i]]=stage_shift_sensitivity(fit=rateout,sensitivities_e=sensitivities_e, sens_l=sens_l, screen.int=screen.int,
                                            a1=a1,a2=a2,extended_followup = i*screen.int,postscreen = T)
    shift_followup_list[[i]]$interval=numscreens+i-1
  }
  
  
  screen.times=seq(0,a2-a1,by=screen.int)
  postscreen.times=seq(max(screen.times)+screen.int,max(screen.times)+screen.int*num.followup.intervals,screen.int)
  ages=c(screen.times,postscreen.times)+a1
  ages=ages[-length(ages)]

  allout=rbind(data.frame(do.call(rbind, shift_list)),data.frame(do.call(rbind, shift_followup_list)))

  
  allout=allout %>%
    group_by(sens_e,time)  %>%
    arrange(control_late, .by_group = TRUE) %>%
    mutate(diff_control_late=diff(c(0,control_late)),
           diff_screen_late=diff(c(0,screen_late)),
           diff_interval_late=diff(c(0,interval_late)),
           diff_screen_post=diff(c(screen_post,0)))
 
   allout=allout%>% mutate(stage_shift_by_interval=100*(diff_control_late-(diff_interval_late+diff_screen_late))/(diff_control_late))

  allout<-allout[!(allout$time=="screenpost" & allout$interval==numscreens),] 

  allout=allout%>% mutate(cumulative_stage_shift=stage_shift)
  
  allout=allout[,c("sens_e","sens_l","interval","cumulative_stage_shift","stage_shift_by_interval","time","control_late","screen_late","interval_late","diff_control_late","diff_interval_late","diff_screen_late")]
  names(allout)[7:12]=c("cum_control_late","cum_screen_late","cum_interval_late","control_late","interval_late","screen_late")

  allout=cbind(allout,age=rep(ages,times=length(sensitivities_e)))

  return(allout)
}
################################################



