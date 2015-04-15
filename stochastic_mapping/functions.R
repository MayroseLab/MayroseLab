library(phytools)
library(diversitree)
library(lawstat)

chi_two_traits <- function(st.s1, st.s2, phy){
  phy$edge.length =  phy$edge.length/sum(phy$edge.length)	# scale tree so that total tree length is 1
  my_lenght_chi = matrix (c(0, 0, 0,0), nrow=2)	# create 2X2 matrix of 0s
  for (i in 1:length(st.s2$history)) #for each branch
  {
    branch_len=phy$edge.length[i]
    my_flag=0
    b1=rbind(st.s1$history[[i]],c(branch_len, 0))
    b2=rbind(st.s2$history[[i]],c(branch_len, 0))
    p=0
    my_inds=c(1,1)
    segs1=dim(b1)[1]
    segs2=dim(b2)[1]
    sum_mat=matrix(c(0), nrow=segs1+segs2,ncol=4)
    sum_mat_ind=1
    while (my_flag<1)
    {
      if ((my_inds[1]+1)<=segs1 & (my_inds[2]+1)<=segs2)
      {
        W=c(b1[my_inds[1]+1], b2[my_inds[2]+1])
        next_p_ind=which(W == min(W), arr.ind = TRUE)
        next_p=min(W)
        sum_mat[sum_mat_ind,1]=p
        sum_mat[sum_mat_ind,2]=next_p
        if (length(next_p_ind)>1) {
          sum_mat[sum_mat_ind,3]=b1[my_inds[1],2]
          sum_mat[sum_mat_ind,4]=b2[my_inds[2],2]
          my_inds[1]=my_inds[1]+1
          my_inds[2]=my_inds[2]+1
          sum_mat_ind=sum_mat_ind+1
          #break
        }
        p=next_p
        sum_mat[sum_mat_ind,3]=b1[my_inds[1],2]
        sum_mat[sum_mat_ind,4]=b2[my_inds[2],2]
        my_inds[next_p_ind]=my_inds[next_p_ind]+1
        sum_mat_ind=sum_mat_ind+1
      }
      else {break}
    }
    for (k in 1:dim(sum_mat)[1])
    {
      if (k>1 & sum_mat[k,1]==1) {break}
      if (identical(sum_mat[k,3:4], c(1,1))) {my_lenght_chi[1,1]=my_lenght_chi[1,1]+(sum_mat[k,2]-sum_mat[k,1])}
      else if (identical(sum_mat[k,3:4], c(1,2))) {my_lenght_chi[1,2]=my_lenght_chi[1,2]+(sum_mat[k,2]-sum_mat[k,1])}
      else if (identical(sum_mat[k,3:4], c(2,1))) {my_lenght_chi[2,1]=my_lenght_chi[2,1]+(sum_mat[k,2]-sum_mat[k,1])}
      else if (identical(sum_mat[k,3:4], c(2,2))) {my_lenght_chi[2,2]=my_lenght_chi[2,2]+(sum_mat[k,2]-sum_mat[k,1])}
    }
  }
  exp_chi <- matrix (c(0, 0, 0,0), nrow=2)
  exp_chi[1,1] <- rowSums(my_lenght_chi)[1]*colSums(my_lenght_chi)[1]
  exp_chi[1,2] <- rowSums(my_lenght_chi)[1]*colSums(my_lenght_chi)[2]
  exp_chi[2,1] <- rowSums(my_lenght_chi)[2]*colSums(my_lenght_chi)[1]
  exp_chi[2,2] <- rowSums(my_lenght_chi)[2]*colSums(my_lenght_chi)[2]
  exp_chi <- exp_chi/sum(my_lenght_chi)
  my.array<-array(0,dim=c(2,2,2))
  my.array[,,1]=my_lenght_chi
  my.array[,,2]=exp_chi
  return(my.array)
}


# phy = curr_tree
chi_two_traits2 <- function(st.s1, st.s2, phy){
  phy$edge.length =  phy$edge.length/sum(phy$edge.length)
  
  all.branch.len.mat=matrix(0, nrow=2,ncol=2)
  for (i in 1:length(st.s1$history)) #for each branch
  {
    branch_len=phy$edge.length[i]  
    branch.len.mat=matrix(0, nrow=2,ncol=2)
    h1 = st.s1$history[[i]]
    h2 = st.s2$history[[i]]
    
    if (dim(h1)[1]==1 && dim(h2)[1]==1){
      branch.len.mat[h2[2],h1[2]] = 1
    }
    if (dim(h1)[1]==2 && dim(h2)[1]==1){      
      branch.len.mat[h2[2],h1[1,2]] = h1[2,1]
      branch.len.mat[h2[2],h1[2,2]] = branch_len-h1[2,1]
    }
    if (dim(h1)[1]==1 && dim(h2)[1]==2){      
      branch.len.mat[h2[1,2],h1[2]] = h2[2,1]
      branch.len.mat[h2[2,2],h1[2]] = branch_len-h2[2,1]
    }
    if (dim(h1)[1]==2 && dim(h2)[1]==2){      
      branch.len.mat[h2[1,2],h1[1,2]] = min(h1[2,1],h2[2,1])
      branch.len.mat[h2[2,2],h1[2,2]] = branch_len-max(h1[2,1],h2[2,1])
      if (h1[2,1] > h2[2,1]){
        branch.len.mat[h2[2,2],h1[1,2]] = h1[2,1] - h2[2,1]
      }else {
        branch.len.mat[h2[1,2],h1[2,2]] = h2[2,1] - h1[2,1]
      }      
    }
    
    all.branch.len.mat = all.branch.len.mat+branch.len.mat
  }
  
  obs_chi = all.branch.len.mat/sum(all.branch.len.mat)
  
  exp_chi <- matrix (c(0, 0, 0,0), nrow=2)
  exp_chi[1,1] <- rowSums(obs_chi)[1]*colSums(obs_chi)[1]
  exp_chi[1,2] <- rowSums(obs_chi)[1]*colSums(obs_chi)[2]
  exp_chi[2,1] <- rowSums(obs_chi)[2]*colSums(obs_chi)[1]
  exp_chi[2,2] <- rowSums(obs_chi)[2]*colSums(obs_chi)[2]
  
  my.array<-array(0,dim=c(2,2,2))
  my.array[,,1]=obs_chi
  my.array[,,2]=exp_chi
  return(my.array)
}

# mapping for dp/pp 
mapping1 <- function(curr_tree, states,plot.it=FALSE,title=""){  
  #curr_tree$edge.length =  curr_tree$edge.length/sum(curr_tree$edge.length)
  
  lik <- make.mk2(curr_tree, states)
  lik_constraint<-constrain(lik, q10~1e-6)
  
  samples = run_mcmc_emma(curr_tree, states,lik_constraint, root.p=c(1,0))
  #est.pars = c(median(samples[,2]),1e-6)
  est.pars = c(samples[sample(length(samples[,2]),1),2],1e-6)
  st.s <- asr.stoch(lik_constraint, est.pars[1], n=1) 
  
  if (plot.it){
    col <- c("blue", "red")    
    plot(st.s, curr_tree, col=col,show.node.state=FALSE,show.tip.label=FALSE,no.margin = TRUE, label.offset = 0.0,lwd =4)
    legend("topleft", c("dp","pp"),col=col,pch=19) 
    mtext(paste(title,"q01:",round(est.pars[1],2)),line=-1,cex=0.7)    
  }
  return(list(st.s,est.pars))
}

# mapping for dioecy/non-dioecy using mcmc 
mapping2 <- function(curr_tree,states,plot.it=FALSE,title=""){

  #curr_tree$edge.length =  curr_tree$edge.length/sum(curr_tree$edge.length)
   
  lik <- make.mk2(curr_tree, states)  
  
  samples = run_mcmc_emma(curr_tree, states,lik)
  est.pars = c(samples[sample(length(samples[,2]),1),2],
               samples[sample(length(samples[,3]),1),3])
  st.s <- asr.stoch(lik, est.pars, n=1)
  if (plot.it){
    col <- c("darkgreen", "magenta")
    plot(st.s, curr_tree, col=col,show.node.state=FALSE,show.tip.label=FALSE,no.margin = TRUE, label.offset = 0.0,lwd =4)
    legend("topleft", c("N","D"),col=col,pch=19) 
    mtext(paste(title,"q01:",round(est.pars[1],2),"  q10:",round(est.pars[2],2)),line=-1,cex=0.7)
    
  }
  return(list(st.s,est.pars))
}

# mapping for dioecy/non-dioecy using mcmc with herm constrained at the root
mapping3 <- function(curr_tree,states,plot.it=FALSE,title=""){
  
  #curr_tree$edge.length =  curr_tree$edge.length/sum(curr_tree$edge.length)
  
  lik <- make.mk2(curr_tree, states)  
  
  samples = run_mcmc_emma(curr_tree, states,lik, root.p=c(1,0))
  est.pars = c(samples[sample(length(samples[,2]),1),2],
               samples[sample(length(samples[,3]),1),3])
  st.s <- asr.stoch(lik, est.pars, n=1,root=ROOT.GIVEN, root.p=c(1,0))
  if (plot.it){
    col <- c("darkgreen", "magenta")
    plot(st.s, curr_tree, col=col,show.node.state=FALSE,show.tip.label=FALSE,no.margin = TRUE, label.offset = 0.0,lwd =4)
    legend("topleft", c("N","D"),col=col,pch=19) 
    mtext(paste(title,"q01:",round(est.pars[1],2),"  q10:",round(est.pars[2],2)),line=-1,cex=0.7)
    
  }
  return(list(st.s,est.pars))
}
# Given some mcmc samples, suggest control parameters
suggest.mcmc.params <- function(ans)
{
  i.par <- seq(from=2, to=ncol(ans)-1)
  
  w <- round(apply(as.matrix(ans[,i.par]), 2, max) - apply(as.matrix(ans[,i.par]), 2, min), 2)
  upper <- round(apply(as.matrix(ans[,i.par]), 2, max) * 5, 2)
  par.start <- as.numeric(ans[which.max(ans$p), i.par])
  
  return(list(w=w, upper=upper, par.start=par.start))
}

run_mcmc_emma <- function(phy, states, lik, root.p=NULL, NSTEPS=1000){
  age <- max(branching.times(phy))
  
  # For pilot chains
  nsteps <- 250
  burnin <- 50
  par.start <- rep(age/2, length(argnames(lik)))
  upper <- age*100
  w <- age
  
  prior <- make.prior.exponential(age)
  
  
  # pilot 1: equal rates, to determine prior - no need if there is only one param
  if (length(argnames(lik))==2){
    lik_constraint<-constrain(lik, q10~q01)
  } else {
    lik_constraint=lik
  }
  if (is.null(root.p)){ # regular run
    ans <- mcmc(lik_constraint, par.start[1], nsteps=nsteps, w=w, upper=upper,
                prior=prior, print.every=0, control=list(backend="CVODES")) 
  } else{               # const root
    ans <- mcmc(lik_constraint, par.start[1], nsteps=nsteps, w=w, upper=upper,
                prior=prior, print.every=0,
                root=ROOT.GIVEN, root.p=root.p,keep.func = FALSE, control=list(backend="CVODES"))
  } 
  prior.rate <- median(ans[-seq(burnin),2])   
  prior <- make.prior.exponential(1/prior.rate)
  
  
  # Pilot 2: use prior from equal rates to determine chain parameters, remove burnin
  if (is.null(root.p)){ # regular run
    ans <- mcmc(lik, par.start, nsteps=nsteps, w=w, upper=upper,
                prior=prior, print.every=0, control=list(backend="CVODES")) 
  } else{               # const root
    ans <- mcmc(lik, par.start, nsteps=nsteps, w=w, upper=upper,
                prior=prior, print.every=0, control=list(backend="CVODES"),
                root=ROOT.GIVEN, root.p=root.p,keep.func = FALSE)
  } 
  init <- suggest.mcmc.params(ans[-seq(burnin),])
  
  # Run the real chain   
  if (is.null(root.p)){ # regular run
    samples <- mcmc(lik, init$par.start, nsteps=NSTEPS, w=init$w,
                    upper=init$upper, prior=prior, print.every=0, control=list(backend="CVODES")) 
  } else{               # const root
    samples <- mcmc(lik, init$par.start, nsteps=NSTEPS, w=init$w,
                    upper=init$upper, prior=prior, print.every=0, control=list(backend="CVODES"),
                    root=ROOT.GIVEN, root.p=root.p,keep.func = FALSE)
  }  
  samples <- samples[(dim(samples)[1]/4+1):(dim(samples)[1]),] #removing 25%
  return(samples)
}


server_commands <- function(dir_name,to_run,main_dir,run_file,genus,time_out=864000){ # 864000 = 10 days
  
  dir.create(paste(main_dir,"/",dir_name,sep=""))
  dir.create(paste(main_dir,"/",dir_name,"/temp",sep=""))
  file.copy(run_file,paste(main_dir,"/",dir_name,sep=""))
  
  command_file = paste(main_dir,"/",dir_name,"/command.txt",sep="")
  file.create(command_file)
  
  dir_path = paste("/groups/itay_mayrose/nivsabath/mapping/",main_dir,"/",dir_name,"/",sep="")
  
  cat("/share/apps/R301/bin/R CMD BATCH '--args working_dir=\"",
      file= command_file,append = FALSE)
  cat(dir_path,
      file= command_file,append = TRUE)
  cat("\" genus=\"",
      file= command_file,append = TRUE)
  cat(genus,
      file= command_file,append = TRUE)
  cat("\"' ",
      file= command_file,append = TRUE)
  cat(dir_path,
      file= command_file,append = TRUE)
  cat(run_file,
      file= command_file,append = TRUE)
  cat(" ",
      file= command_file,append = TRUE)
  cat(dir_path,
      file= command_file,append = TRUE)
  cat(run_file,
      file= command_file,append = TRUE)  
  cat("out\tr",
      file= command_file,append = TRUE)
  cat(dir_name,
      file= command_file,append = TRUE)  
  
  cat("perl /groups/itay_mayrose/nivsabath/Scripts/run_cmds_in_q_WithNameInCmd_jekyl_p0.pl ",
        file= to_run,append = TRUE)    
  
  
  cat(dir_path,
      file= to_run,append = TRUE)
  cat("command.txt ",
      file= to_run,append = TRUE)
  cat(dir_path,
      file= to_run,append = TRUE)
  cat("temp/ itaym 1 yes ",
      file= to_run,append = TRUE)
  cat(time_out,
      file= to_run,append = TRUE)
  cat(" ",
      file= to_run,append = TRUE)
  cat(dir_name,
      file= to_run,append = TRUE)  
  cat("\n",
      file= to_run,append = TRUE)  
  
}

run_mcmc_bisse <- function(phy, states,sampling.f,NSTEPS=1000){
  # set up likelihood function
  lik.0 <- make.bisse(phy, states,sampling.f = sampling.f)
  lik <- constrain(lik.0, q10~0)
  age <- max(branching.times(phy))
  root.p = c(1,0)
  
  # For pilot chains
  nsteps <- 250
  burnin <- 50
  par.start <- rep(age/2, length(argnames(lik)))
  upper <- age*100
  w <- age
  prior <- make.prior.exponential(age)
  
  # pilot 1: equal rates, to determine prior 
  lik_constraint<-constrain(lik, lambda0~lambda1,mu0~mu1)
  ans <- mcmc(lik_constraint, par.start[c(1,3,5)], nsteps=nsteps, w=w, upper=upper,
              prior=prior, print.every=0,
              root=ROOT.GIVEN, root.p=root.p,keep.func = FALSE, control=list(backend="CVODES"))
  
  prior.rate.equal <- colMeans(ans[-seq(burnin),2:4])
  prior.rate = c(rep(prior.rate.equal[1],2),rep(prior.rate.equal[2],2),prior.rate.equal[3])
  prior <- make.prior.exponential(1/prior.rate)
  
  # run the chain
  samples <- mcmc(lik, par.start, nsteps=NSTEPS, w=1,
                  upper=max(prior.rate)*100, prior=prior, print.every=100, control=list(backend="CVODES"),
                  root=ROOT.GIVEN, root.p=root.p,keep.func = FALSE)
  
  samples <- samples[(dim(samples)[1]/4+1):(dim(samples)[1]),] #removing 25%
  # calc diversification
  samples$div0 = samples$lambda0-samples$mu0
  samples$div1 = samples$lambda1-samples$mu1
  return(samples)
}


merge_tree_states <- function(phy,states,species_num=1){
  df = data.frame(n.sp=NA,n.sp.tree=NA,n.sp.tree.inter=NA,pct.non.na=NA,pct.0=NA,pct.1=NA)
  
  df$n.sp.tree = length(phy$tip.label) 
  
  if (length(phy$tip.label) == 0){
    print(paste("empty tree: ",tree_file))
    return(NULL)
  } 
  
  # rescale
  phy <- multi2di(phy, random = TRUE)
  
  phy$edge.length<-
    phy$edge.length/max(branching.times(phy))
  
  #   rescale.function = rescale(phy,"depth")
  #   phy = rescale.function(1)
  
  # drop tips with subsp. or   var.
  if (length(grep("subsp.", phy$tip.label)) > 0) phy = drop.tip(phy,phy$tip.label[grep("subsp.", phy$tip.label)])
  if (length(grep("var.", phy$tip.label)) > 0) phy = drop.tip(phy,phy$tip.label[grep("var.", phy$tip.label)])
  
  df$n.sp.tree.inter = length(phy$tip.label) 
  
  if (length(phy$tip.label) == 0){
    print(paste("empty tree after subsp. removal: ",tree_file))
    return(NULL)
  } 
  
  # sampling calc:
  #max.taxa = length(union(phy$tip.label,names(states)))
  df$species_num = species_num
  sf = length(phy$tip.label)/species_num
  sampling.f = c(sf,sf)    
  
  # drop data not on tree
  states <- states[which(is.element(names(states),phy$tip.label))]
  
  # add the taxa not in states to states
  d1 = setdiff(phy$tip.label,names(states))
  states2 = rep(NA,length(d1))
  names(states2) = d1
  states = c(states2,states)
  
  # reorder according to tree tips
  states <- states[phy$tip.label]
  
  df$pct.non.na = round(100*length(which(!is.na(states)))/length(states)) 
  df$pct.0 = round(100*length(which(states==0))/length(which(!is.na(states)))) 
  df$pct.1 = round(100*length(which(states==1))/length(which(!is.na(states)))) 
  df$n.0 = length(which(states==0))
  df$n.1 = length(which(states==1))
  df$sampling.f = sf
  
  return(list(phy,states,df,sampling.f))     
}


get_res <- function(res.dir, NTREES.1 = 100, NTREES.2 = 20){
  
  genera = dir(res.dir)
  genera.names=gsub(".RData","",genera)
  res.mat=matrix(NA,nrow=length(genera),ncol=13)
  
  pdf(paste(res.dir,".res.pdf",sep=""))
  
  par(mfrow=c(4,5), pty="s")
  print(length(genera))
  if (length(genera)<6)
    par(mfrow=c(1,length(genera)), pty="s")
  for (genus.index in 1:length(genera)) {  
    
    par(mar=c(4,4,3,1))
    
    genus.out = genera[genus.index]
    load(paste(res.dir,"/",genus.out,sep="")) 
    g=strsplit(as.character(genus.out),split=".",fixed=TRUE)
    g=g[[1]][1]  
    dij = mean(dd[,1],na.rm=TRUE)
    D = mean(rowSums(abs(dd[,1:4]),na.rm=TRUE))
    
    # qDP->PP qPP->DP qN->D qD->N Obs(PP,D) Exp(PP,D)
    res.mat[genus.index,1:6]=colMeans(dd[,7:12],na.rm=TRUE)
    res.mat[genus.index,11]=mean(dd[,dim(dd)[2]],na.rm=TRUE)
    
    dij.sim=matrix(NA,1,NTREES.1)
    D.sim=matrix(NA,1,NTREES.1)
    tb.r.sim=matrix(NA,1,NTREES.1)
    for (t in 1:NTREES.1){
      ind=which(dd.sim[,1] == t & !is.na(dd.sim[,3]))
      sim.res = dd.sim[ind,3:6]
      if (length(ind) == 1){
        dij.sim[t] = sim.res[1]
        D.sim[t] = sum(abs(sim.res),na.rm=TRUE)
        tb.r.sim[t] =  dd.sim[ind,dim(dd.sim)[2]]
      }
      #print(dim(sim.res))
      if (length(ind) > 1 && dim(sim.res)[1]>1){
        dij.sim[t] = mean(sim.res[,1],na.rm=TRUE)
        D.sim[t] = mean(rowSums(abs(sim.res),na.rm=TRUE),na.rm=TRUE)
        tb.r.sim[t] =  mean(dd.sim[ind,dim(dd.sim)[2]],na.rm=TRUE)
      }
    }
    
    p=length(which(D<=D.sim))/length(!is.na(D.sim))
    #res.mat[genus.index,c(1,2)]=c(D,p)
    ylab=""
    if (any(c(1,6,11,16)==genus.index))
      ylab="frequency"
    xlab=""
    if (genus.index>13)
      xlab="d dp-ND"
    p=length(which(dij<=dij.sim))/length(!is.na(dij.sim))
    hist(dij.sim,
         main=g,
         breaks=seq(-0.5,0.5,0.02),
         cex.main=1,
         cex.axis=0.5,
         xlim=c(-0.15,0.15),
         ylab=ylab,
         #xlim = c(min(c(dij,dij.sim),na.rm=TRUE),max(c(dij,dij.sim),na.rm=TRUE)),
         xlab = xlab)
    points(dij,0,col="red",pch=17,cex=1)
    mtext(paste("p =",round(p,4)), line=0, cex=0.6)  
    
    res.mat[genus.index,c(7,8)]=c(dij,p)  
    
    # % dioecy in the root
    res.mat[genus.index,9]=100*length(which(dd[,6]==1))/length(dd[,6])
    res.mat[genus.index,10]=mean(dij.sim,na.rm=TRUE)
    
    res.mat[genus.index,12]=min(length(which(res.mat[genus.index,11]<=tb.r.sim))/length(!is.na(tb.r.sim)),
                                length(which(res.mat[genus.index,11]>=tb.r.sim))/length(!is.na(tb.r.sim)))
    res.mat[genus.index,13]=mean(tb.r.sim,na.rm=TRUE)
  }
  
  
  
  all.res=data.frame(genus=genera.names,round(res.mat,4))
  colnames(all.res) = c("genus","qDP->PP","qPP->DP",
                                  "qN->D","qD->N",
                              "Obs(PP,D)","Exp(PP,D)",
                              "d","p(d)","Dioecy at the root (%)","d.simulated",
                               "threshBayes.r","p(threshBayes.r)","threshBayes.r.simulated")
  
  par(mfrow=c(1,1), pty="m")
#   hist(all.res$d,xlab="d(diploid - non-dioecy)",main="stochastic mapping results of 18 genera")
#   w.t=wilcox.test(all.res$d)
#   mtext(paste("wilcox test p =",round(w.t$p.value,3)))
  
  br=seq(-0.11,0.15,0.02)
  br.names=round(br-0.01,2)
  dat=data.frame(v1=hist(all.res$d,breaks=br,plot=FALSE)$counts,
                 v2=hist(all.res$d.simulated,breaks=br,plot=FALSE)$counts)
  barplot(t(as.matrix(dat)),xlab="d(PP - Dioecy)", ylab= "Count", names.arg=br.names[2:length(br.names)],
          beside=TRUE, legend.text=c("True","Simulated"),           
          col=c("red","blue"),cex.names=0.7)
  w.t=wilcox.test(all.res$d,all.res$d.simulated,paired=TRUE)
  var=var.test(all.res$d,all.res$d.simulated)
  
  y <- c(all.res$d,all.res$d.simulated)
  group <- as.factor(c(rep(1, length(all.res$d)), rep(2, length(all.res$d.simulated))))
  levene = levene.test(y, group)
  
  mtext(paste("wilcox test p =",round(w.t$p.value,3),"   Levene's test p =",round(levene$p.value,4)),line=1)
   
  par(mfrow=c(1,1), pty="s")
  plot(all.res[,"Exp(PP,D)"],
       all.res[,"Obs(PP,D)"],
       xlim=c(0,0.7),ylim=c(0,0.7),
       main="Time shared by polyploid and dioecy",
       xlab="Expected(PP,Dioecy)",ylab="Observed(PP,Dioecy)")
  lines(c(0,0.7),c(0,0.7))
  
  br=seq(-1,1,0.1)
  br.names=br
  dat=data.frame(v1=hist(all.res$threshBayes.r,breaks=br,plot=FALSE)$counts,
                 v2=hist(all.res$threshBayes.r.simulated,breaks=br,plot=FALSE)$counts)
  barplot(t(as.matrix(dat)),xlab="threshBayes.r(PP - Dioecy)", ylab= "Count", 
          names.arg=br.names[2:length(br.names)],
          beside=TRUE, legend.text=c("True","Simulated"),           
          col=c("red","blue"),cex.names=0.7)
  w.t=wilcox.test(all.res$threshBayes.r,all.res$threshBayes.r.simulated,paired=TRUE)
  var=var.test(all.res$threshBayes.r,all.res$threshBayes.r.simulated)
  
  y <- c(all.res$threshBayes.r,all.res$threshBayes.r.simulated)
  group <- as.factor(c(rep(1, length(all.res$threshBayes.r)), 
                       rep(2, length(all.res$threshBayes.r.simulated))))
  levene = levene.test(y, group)
  
  mtext(paste("wilcox test p =",round(w.t$p.value,3),"   Levene's test p =",round(levene$p.value,4)),line=1)
  
  dev.off()
  
  write.csv(all.res,file=paste(res.dir,".mapping.results.csv",sep=""))
  print(all.res)
  print(all.res[which(all.res[,"p(d)"]<0.05),])
  
#   all.res = 100*all.res[which(all.res[,"Obs(PP,D)"]>0),]
#   print(2*sum(all.res[,"Obs(PP,D)"]*log(all.res[,"Obs(PP,D)"]/all.res[,"Exp(PP,D)"])))
#   ss=2*sum(all.res[,"Obs(PP,D)"]*log(all.res[,"Obs(PP,D)"]/all.res[,"Exp(PP,D)"]))
#   pchisq(ss, length(all.res[,"Obs(PP,D)"]), lower.tail = FALSE)
}

get_res_old <- function(res.dir, NTREES.1 = 100, NTREES.2 = 20){
  
  genera = dir(res.dir)
  genera.names=gsub(".RData","",genera)
  res.mat=matrix(NA,nrow=length(genera),ncol=13)
  
  pdf(paste(res.dir,".res.pdf",sep=""))
  
  par(mfrow=c(4,5), pty="s")
  print(length(genera))
  if (length(genera)<6)
    par(mfrow=c(1,length(genera)), pty="s")
  for (genus.index in 1:length(genera)) {  
    
    par(mar=c(5,3,4,2))
    
    genus.out = genera[genus.index]
    load(paste(res.dir,"/",genus.out,sep="")) 
    g=strsplit(as.character(genus.out),split=".",fixed=TRUE)
    g=g[[1]][1]  
    dij = mean(dd[,1],na.rm=TRUE)
    D = mean(rowSums(abs(dd[,1:4]),na.rm=TRUE))
    
    dij.sim=matrix(NA,1,NTREES.1)
    D.sim=matrix(NA,1,NTREES.1)
    for (t in 1:NTREES.1){
      ind=which(dd.sim[,1] == t & !is.na(dd.sim[,3]))
      sim.res = dd.sim[ind,3:6]
      if (length(ind) == 1){
        dij.sim[t] = sim.res[1]
        D.sim[t] = sum(abs(sim.res),na.rm=TRUE)
      }
      #print(dim(sim.res))
      if (length(ind) > 1 && dim(sim.res)[1]>1){
        dij.sim[t] = mean(sim.res[,1],na.rm=TRUE)
        D.sim[t] = mean(rowSums(abs(sim.res),na.rm=TRUE),na.rm=TRUE)
      }
    }
    
    p=length(which(D<=D.sim))/length(!is.na(D.sim))
    res.mat[genus.index,c(1,2)]=c(D,p)
    ylab=""
    if (any(c(1,6,11,16)==genus.index))
      ylab="frequency"
    xlab=""
    if (genus.index>13)
      xlab="d dp-ND"
    p=length(which(dij<=dij.sim))/length(!is.na(dij.sim))
    print(dij)
    print(which(dij<=dij.sim))
    print(round(dij.sim[which(dij<=dij.sim)],3))
    hist(dij.sim,
         main=g,
         breaks=seq(-0.5,0.5,0.01),
         cex.main=1,
         xlim=c(-0.2,0.2),
         ylab=ylab,
         #xlim = c(min(c(dij,dij.sim),na.rm=TRUE),max(c(dij,dij.sim),na.rm=TRUE)),
         xlab = xlab)
    points(dij,0,col="red",pch=17,cex=2)
    mtext(paste("p =",p), line=0, cex=0.8)  
    
    res.mat[genus.index,c(3,4)]=c(dij,p)  
    
  }
  
  par(mfrow=c(4,5))
  for (genus.index in 1:length(genera)) {  
    
    par(mar=c(5,4,4,0.5))
    
    genus.out = genera[genus.index]
    load(paste(res.dir,"/",genus.out,sep="")) 
    g=strsplit(as.character(genus.out),split=".",fixed=TRUE)
    g=g[[1]][1]  
    
    m.na.rm <- function(x) median(x,na.rm=TRUE)
    pagel.res = apply(pagel,2,m.na.rm)
    LR = pagel.res[1]
    
    LR.sim=matrix(NA,1,NTREES.1)  
    for (t in 1:NTREES.1){
      sim.res = pagel.sim[which(pagel.sim[,1] == t & !is.na(pagel.sim[,3])),3]
      if (length(sim.res)>1){
        LR.sim[t] = median(sim.res,na.rm=TRUE)
      }
    }
    
    if (length(which(!is.na(LR.sim)))>5){
      ylab=""
      if (any(c(1,6,11,16)==genus.index))
        ylab="frequency"
      xlab=""
      if (genus.index>13)
        xlab="Pagel's LR"
      p=length(which(LR<=LR.sim))/length(!is.na(LR.sim))
      hist(LR.sim,
           main=g,
           cex.main=1,
           #xlim=c(-0.2,0.2),
           ylab=ylab,
           xlim = c(min(c(LR,LR.sim),na.rm=TRUE),max(c(LR,LR.sim),na.rm=TRUE)),
           xlab = xlab)
      points(LR,0,col="red",pch=17,cex=2)
      mtext(paste("p =",p), line=0, cex=0.6)  
      res.mat[genus.index,c(5,6)] = c(LR,p)
      res.mat[genus.index,7:12] = pagel.res[7:12]
      res.mat[genus.index,13] = pagel.res[17]
    }
  }
  
  all.res=data.frame(genus=genera.names,round(res.mat,3))
  colnames(all.res) = c("genus","D","p(D)","d","p(d)","Pagel's LR","p(LR)",
                        "dp|N->dp|D","dp|N->pp|N","dp|D->dp|N","dp|D->pp|D","pp|N->pp|D","pp|D->pp|N",
                        "Pr(root=dp|N)")
  all.res
  par(mfrow=c(1,1))
  hist(all.res$d,xlab="d(diploid - non-dioecy)",main="stochastic mapping results of 18 genera")
  w.t=wilcox.test(all.res$d)
  mtext(paste("wilcox test p =",round(w.t$p.value,3)))
  
  dev.off()
  
  write.csv(all.res,file=paste(res.dir,".mapping.results.csv",sep=""))
  print(all.res)
  print(all.res[which(all.res[,3]<0.05 | all.res[,5]<0.05 | all.res[,7]<0.05),])
}

prep_mapping <-function(main_dir,genera,NTREES.1 = 20, NTREES.2 = 2, SIM.PCT.DIFF = 10, run_file = "map_genus.R"){
  
  setwd("/groups/itay_mayrose/nivsabath/mapping")
  
  dir.create(main_dir)
  dir.create(paste(main_dir,"/res",sep=""))
  
  source("functions.R")
  
  for (genus.index in 1:length(genera)) {
    
    genus = genera[genus.index]
    print(genus)
    to_run="mapping_to_run.txt"
    name=genus
    main_dir=main_dir
    run_file = run_file
    server_commands(name,to_run,main_dir,run_file,genus)    
    file.create("Data.RData")
    save(genus, NTREES.1,NTREES.2,SIM.PCT.DIFF,file="Data.RData")
    file.copy("Data.RData",paste(main_dir,"/",name,sep=""))
    file.copy("functions.R",paste(main_dir,"/",name,sep=""))  
    file.copy("fitPagel.const.R",paste(main_dir,"/",name,sep=""))  
  }
}

