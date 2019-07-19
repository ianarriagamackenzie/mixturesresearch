# 2nd simulation script to generate populations
# used for specific ancestries and using the different SLSQP algorithms

simdat <- read.table('resources/5ancfinalsimdat.txt', header = TRUE)


genpoplist = function(pop1, pop2, pop3, pop4, pop5){
  retlist = c(pop1, pop2, pop3, pop4, pop5)
  return(retlist)
}

genpopfrac = function(pop_1_frac, pop_2_frac, pop_3_frac, pop_4_frac, pop_5_frac){
  retlist = numeric(5)
  if (pop_1_frac[1] == 1){
    retlist = c(pop_1_frac, pop_2_frac, pop_3_frac, pop_4_frac, pop_5_frac)
    return(retlist)
  }
  else{
    retlistini = c(pop_1_frac, pop_2_frac, pop_3_frac, pop_4_frac, pop_5_frac)
    if (length(retlistini) == 7){
      retlist[1] = runif(1, retlistini[1], retlistini[2])
      retlist[2] = 1 - retlist[1]
      return(retlist)
    }
    if (length(retlistini) == 8){
      retlist[1] = runif(1, retlistini[1], retlistini[2])
      retlist[2] = runif(1, retlistini[3], retlistini[4])
      retlist[3] = 1 - sum(retlist)
      return(retlist)
    }
    if (length(retlistini) == 9){
      retlist[1] = runif(1, retlistini[1], retlistini[2])
      retlist[2] = runif(1, retlistini[3], retlistini[4])
      retlist[3] = runif(1, retlistini[5], retlistini[6])
      retlist[4] = 1 - sum(retlist)
      return(retlist)
    }
    if (length(retlistini) == 10){
      retlist[1] = runif(1, retlistini[1], retlistini[2])
      retlist[2] = runif(1, retlistini[3], retlistini[4])
      retlist[3] = runif(1, retlistini[5], retlistini[6])
      retlist[4] = runif(1, retlistini[7], retlistini[8])
      retlist[5] = 1 - sum(retlist)
      return(retlist)
    }
  }
}

genindlist = function(pop1, pop2, pop3, pop4, pop5){
  retlist = numeric(5)
  retlistini = c(pop1, pop2, pop3, pop4, pop5)
  for (i in 1:5){
    retlist[i] = which(names(simdat) == retlistini[i])
  }
  return(retlist)
}

popnumlist = function(ntot, fracl){
  
  pop_number = sum(fracl != 0)
  tot_popnum_list <- numeric(5)
  
  for (i in 1:pop_number){
    if (i == pop_number){
      tot_popnum_list[i] <- (ntot - sum(tot_popnum_list))
    }
    else {
      tot_popnum_list[i] <- floor((ntot * fracl[i]))
    }
  }
  return(tot_popnum_list)
}

truepopfrac = function(ntot, popnum){
  retlist = popnum / ntot
  return(retlist)
}

simgen = function(sims, simdata, pop1, pop2, pop3, pop4, pop5, pop1f, pop2f, pop3f, pop4f, pop5f){
  
  MAF_thresh = 0.01 # MAF threshold to discard
  N_total = 10000 # simulated population number
  
  poplist = genpoplist(pop1, pop2, pop3, pop4, pop5)
  indlist = genindlist(pop1, pop2, pop3, pop4, pop5)
  
  
  for (i in 1:sims){
    
    fraclist = genpopfrac(pop1f, pop2f, pop3f, pop4f, pop5f)
    popnuml = popnumlist(N_total, fraclist)
    tfraclist = truepopfrac(N_total, popnum1)
    
    pop_number = sum(fraclist != 0)
    
    snpsamps = simdat[sample(1:nrow(simdat))[1:100000],]
    
    pop_frame = data.frame(indlist, poplist, fraclist, popnum1, tfraclist)
    
    pop_matrix = matrix(0, nrow = 100000, ncol = 3)
    
    for (i in 1:pop_number){
      popmatrixadd = t(sapply(snpsamps[[pop_frame$indlist[i]]], function(x){x2=as.numeric(x); rmultinom(1, pop_frame$popnum1[i], prob=(c(x2**2, 2*x2*(1-x2), (1-x2)**2)))}))
      pop_matrix = pop_matrix + popmatrixadd
    }
    
  }
  
}



