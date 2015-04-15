args <- commandArgs( TRUE )
for( i in 1:length(args) ){
  eval( parse( text = args[[i]] ) )
}

library(phytools)
library(diversitree)
library(geiger)
require(R.utils)

setwd(working_dir)

source("functions.R")
source("fitPagel.const.R")

load("Data.RData")

NSIM = 1
MAX.TRIES = 10

my_table <- read.table(paste("../../traits//",genus,"//",genus,"_traits_dimorphism",sep=""),
                       stringsAsFactors = FALSE,na.strings ="-")

states1 <- my_table[which(!is.na(my_table$V2) & !is.na(my_table$V3)),2]
states2 <- my_table[which(!is.na(my_table$V2) & !is.na(my_table$V3)),3]
names(states1) <- my_table$V1[which(!is.na(my_table$V2) & !is.na(my_table$V3))]
names(states2) <- my_table$V1[which(!is.na(my_table$V2) & !is.na(my_table$V3))]

d_mat_full<-array(0,dim=c(2,2,NTREES.1))

# save small dij
dd = matrix(NA,ncol=13,nrow=NTREES.1)
# save small dij of simulation
dd.sim = matrix(NA,ncol=15,nrow=NTREES.1*NSIM*NTREES.2)
dd.sim.index=1

# save LR from pagel
pagel = matrix(NA,ncol=20,nrow=NTREES.1)
# save pagel LR of simulation
pagel.sim = matrix(NA,ncol=3,nrow=NTREES.1*NSIM*NTREES.2)

pdf(paste("../res/",genus,".mappings.pdf",sep=""))

for (tree.index in 1:NTREES.1) {
  curr_tree = read.tree(paste("../../pruned_trees//",genus,".tre",sep=""))[[tree.index]]
  
  if (length(setdiff(names(states1),curr_tree$tip.label))>0 ||
        length(setdiff(curr_tree$tip.label,names(states1)))>0){
    print("warning: tree and states do not match")
    treedata(curr_tree, states1, sort=TRUE, warnings=TRUE)
    treedata(curr_tree, states2, sort=TRUE, warnings=TRUE)  
  }
  
  curr_tree$edge.length =  curr_tree$edge.length/sum(curr_tree$edge.length)
  
  states1 = states1[curr_tree$tip.label]
  states2 = states2[curr_tree$tip.label]  
  
  pct.0.1 = 100*length(which(states1==0))/length(states1)
  pct.0.2 = 100*length(which(states2==0))/length(states2)
  
  plot.it=TRUE
  if (tree.index>100)
    plot.it=FALSE
  
  if (plot.it)
    layout(matrix(c(1,2),1,2))
  r1 <- NULL;
  r2 <- NULL;
  
  count=0
  while (count<MAX.TRIES && (is.null(r1) || is.null(r2))){
    tryCatch({
      res <- evalWithTimeout({
        r1 <- mapping1(curr_tree,states1,plot.it,paste(genus," tree",tree.index," ",sep="")) 
        st.s1 = r1[[1]]; par1 = r1[[2]];
        r2 <- mapping2(curr_tree,states2,plot.it)
        st.s2 = r2[[1]]; par2 = r2[[2]];
        
        states.1.2 = as.matrix(data.frame(states1=states1,states2=states2,row.names=names(states1)))
        mcmc = threshBayes(curr_tree, states.1.2, types=c("discrete","discrete"),ngen=100000)
        estimates<-colMeans(mcmc$par[251:nrow(mcmc$par),2:6])
        threshBayes.r = estimates[5]
        
      }, timeout=1000);
    }, TimeoutException=function(ex) {
      cat("Timeout. Skipping.\n");
    }, error = function(err) {      
      # error handler picks up where error was generated
      print(paste("tree.index",tree.index," ERROR",count,": ",err))     
    })
    count=count+1
  }  
  if (count==MAX.TRIES) next;
    
  dio.herm.root = st.s2$node.state[1]-1
  
  chi_mats <- chi_two_traits(st.s1,st.s2,curr_tree)
  d_mat_full[,,tree.index]=chi_mats[,,1]-chi_mats[,,2]
  dd[tree.index,] = c(as.vector(d_mat_full[,,tree.index]),
                      st.s1$node.state[1]-1,st.s2$node.state[1]-1,par1,par2,
                      chi_mats[2,2,1],chi_mats[2,2,2],threshBayes.r)
  
  if (plot.it)
    mtext(paste("Obs.(1,1):",round(chi_mats[2,2,1],2),
                "  Exp.(1,1):",round(chi_mats[2,2,2],2),
                "  d(1,1):",round(chi_mats[2,2,1]-chi_mats[2,2,2],3),
                "  r:",round(threshBayes.r,2)),side=1,line=-1,cex=0.7)
  
  for (sim.index in 1:NSIM) {
    
    # simulate states according to mapping root state and rate estimates
    sim.states1 <- sim.character(curr_tree, par1, x0=0, model="mk2")
    sim.pct.0.1 = 100*length(which(sim.states1==0))/length(sim.states1)
    
    sim.states2 <- sim.character(curr_tree, par2, x0=dio.herm.root, model="mk2")
    sim.pct.0.2 = 100*length(which(sim.states2==0))/length(sim.states2)
    
    # make sure there are tips in both states
    while (length(unique(sim.states1))<2 || abs(sim.pct.0.1-pct.0.1) > SIM.PCT.DIFF){
      print("repeat simulation 1")
      sim.states1 <- sim.character(curr_tree, par1, x0=0, model="mk2")
      sim.pct.0.1 = 100*length(which(sim.states1==0))/length(sim.states1)
    }
      
    while (length(unique(sim.states2))<2 || abs(sim.pct.0.2-pct.0.2) > SIM.PCT.DIFF){
      print("repeat simulation 2")
      sim.states2 <- sim.character(curr_tree, par2, x0=st.s2$node.state[1]-1, model="mk2")
      sim.pct.0.2 = 100*length(which(sim.states2==0))/length(sim.states2)
    }
          
    sim.d_mat_full<-array(0,dim=c(2,2,NTREES.2)) 
    
    for (sim.tree.index in 1:NTREES.2) {
      print(paste(genus," tree",tree.index," sim",sim.index," sim.tree",sim.tree.index))
      
      sim.curr_tree = read.tree(paste("../../pruned_trees//",genus,".tre",sep=""))[[sim.tree.index]]
      sim.curr_tree$edge.length =  sim.curr_tree$edge.length/sum(sim.curr_tree$edge.length)
      
      sim.states1 = sim.states1[sim.curr_tree$tip.label]
      sim.states2 = sim.states2[sim.curr_tree$tip.label]  
      
      plot.it=TRUE
      if (sim.index>1 || sim.tree.index>1)
        plot.it=FALSE
      
      if (plot.it)
        layout(matrix(c(1,2),1,2))
      r1 <- NULL;
      r2 <- NULL;
      
      count=0
      while (count<MAX.TRIES && (is.null(r1) || is.null(r2))){
        tryCatch({
          res <- evalWithTimeout({
            r1 <- mapping1(sim.curr_tree,sim.states1,plot.it,paste(genus," tree",tree.index," sim",sim.index," sim.tree",sim.tree.index," ",sep=""));  
            st.s1 = r1[[1]]; par1 = r1[[2]];
            r2 <- mapping2(sim.curr_tree,sim.states2,plot.it);
            st.s2 = r2[[1]]; par2 = r2[[2]];
            
            states.1.2 = as.matrix(data.frame(states1=sim.states1,states2=sim.states2,row.names=names(states1)))
            mcmc = threshBayes(sim.curr_tree, states.1.2, types=c("discrete","discrete"),ngen=100000)
            estimates<-colMeans(mcmc$par[251:nrow(mcmc$par),2:6])
            threshBayes.r = estimates[5]
          }, timeout=1000);
        }, TimeoutException=function(ex) {
          cat("Timeout. Skipping.\n");
        }, error = function(err) {      
          # error handler picks up where error was generated
          print(paste("sim.tree.index",sim.tree.index," ERROR",count,": ",err))       
        })
        count=count+1
      }  
      if (count==MAX.TRIES) next;
            
      print(c(par1,par2))
      
      chi_mats <- chi_two_traits(st.s1,st.s2,sim.curr_tree)
      sim.d_mat_full[,,sim.tree.index]=chi_mats[,,1]-chi_mats[,,2]
      
      dd.sim[dd.sim.index,] = c(tree.index,sim.index,
                                as.vector(sim.d_mat_full[,,sim.tree.index]),
                                st.s1$node.state[1]-1,st.s2$node.state[1]-1,
                                par1,par2,chi_mats[2,2,1],chi_mats[2,2,2],threshBayes.r)    
      if (plot.it)
        mtext(paste("Obs.(1,1):",round(chi_mats[2,2,1],2),
                    "  Exp.(1,1):",round(chi_mats[2,2,2],2),
                    "  d(1,1):",round(chi_mats[2,2,1]-chi_mats[2,2,2],3),
                    "  r:",round(threshBayes.r,2)),side=1,line=-1,cex=0.7)
      
      dd.sim.index = dd.sim.index+1
    } 
  }
}
dev.off()

D = mean(colSums(colSums(abs(d_mat_full), 3), 3))

save(dd,dd.sim,pagel,pagel.sim,file=paste("../res/",genus,".RData",sep=""))




#   tryCatch({
#     res <- evalWithTimeout({
#       fit.ape<-fitPagel(curr_tree,states1,states2)  
#       pagel[tree.index,] = c(fit.ape$lik.ratio,
#                              fit.ape$independent.logL,fit.ape$dependent.logL,
#                              as.vector(c(fit.ape$independent.Q[1,2:3],fit.ape$independent.Q[2,1])),
#                              as.vector(c(fit.ape$dependent.Q[1,2:3],fit.ape$dependent.Q[2,c(1,4)],fit.ape$dependent.Q[3,4],fit.ape$dependent.Q[4,3])),
#                              as.vector(fit.ape$independent.anc),
#                              as.vector(fit.ape$dependent.anc));
#       # 1 - LR
#       # 2,3 - logL
#       # 4:6 - q1, q2, q3 (independent)
#       # 7:12 - q1 - q6 (dependent)
#       # 13:16 - root state (independent 00 01 10 11)
#       # 17:20 - root state (dependent  00 01 10 11)
#     }, timeout=1000);
#   }, TimeoutException=function(ex) {
#     cat("Timeout. Skipping.\n");
#   }, error = function(err) {      
#     # error handler picks up where error was generated
#     print(paste("tree.index",tree.index," ERROR",count,": ",err))     
#   })


#       tryCatch({
#         res <- evalWithTimeout({
#           fit.ape<-fitPagel(sim.curr_tree,sim.states1,sim.states2)  
#           pagel.sim[dd.sim.index,] = c(tree.index,sim.index,fit.ape$lik.ratio);
#         }, timeout=1000);
#       }, TimeoutException=function(ex) {
#         cat("Timeout. Skipping.\n");
#       }, error = function(err) {      
#         # error handler picks up where error was generated
#         print(paste("tree.index",tree.index," ERROR",count,": ",err))     
#       })
