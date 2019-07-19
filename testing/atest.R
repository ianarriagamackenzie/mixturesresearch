install.packages('nloptr')
library(nloptr)

visframe = data.frame(numeric(0), numeric(0), numeric(0), numeric(0), numeric(0), numeric(0), numeric(0))

masterframe1 = Real_data2use_april18

for (j in 1:5){
  master_frame_final <- mf2[sample(1:nrow(mf2))[1:100000],]
  
  pop_number = 5
  
  # sets starting fractions, currently set to 1/N where N=number of populations
  starting <- numeric(pop_number)
  for (i in 1:pop_number){
    starting[i] <- 1/pop_number
  }
  
  # function that is being minimized
  fn.ancmixtures <- function(x){
    minfunc = 0
    for (i in 1:pop_number){
      minfunc = minfunc + x[i]*master_frame_final[i+4] 
    }
    minfunc = (minfunc - master_frame_final[pop_number + 9])**2
    return(sum(minfunc))
  }
  
  # inequality function, each proportion is greater then 0, eg x[i] > 0
  hin.ancmixtures <- function(x){
    h <- numeric(5)
    h[1] <- x[1]
    h[2] <- x[2]
    h[3] <- x[3]
    h[4] <- x[4]
    h[5] <- x[5]
    return(h)
  }
  
  # equality function, sum of proportions must equal 1, eg sum(x[i]) - 1 = 0
  heq.ancmixtures <- function(x){
    equality = 0
    for (i in 1:pop_number){
      equality = equality + x[i]
    }
    return(equality - 1)
  }
  
  # SLSQP function, nloptr library
  
  start_time = Sys.time()
  S <- slsqp(starting,
             fn = fn.ancmixtures,
             hin = hin.ancmixtures,
             heq = heq.ancmixtures)
  end_time = Sys.time()
  
  visframe = rbind(visframe, S$par)
}



master_frame_final <- mdat4[sample(1:nrow(mdat4))[1:100000],]

pop_number = 5

# sets starting fractions, currently set to 1/N where N=number of populations
starting <- numeric(pop_number)
for (i in 1:pop_number){
  starting[i] <- 1/pop_number
}

# function that is being minimized
fn.ancmixtures <- function(x){
  minfunc = 0
  for (i in 1:pop_number){
    minfunc = minfunc + x[i]*master_frame_final[i] 
  }
  minfunc = (minfunc - master_frame_final[8])**2
  return(sum(minfunc))
}

# inequality function, each proportion is greater then 0, eg x[i] > 0
hin.ancmixtures <- function(x){
  h <- numeric(5)
  h[1] <- x[1]
  h[2] <- x[2]
  h[3] <- x[3]
  h[4] <- x[4]
  h[5] <- x[5]
  return(h)
}

# equality function, sum of proportions must equal 1, eg sum(x[i]) - 1 = 0
heq.ancmixtures <- function(x){
  equality = 0
  for (i in 1:pop_number){
    equality = equality + x[i]
  }
  return(equality - 1)
}

# SLSQP function, nloptr library


for (j in 1:5){
  master_frame_final <- mdat4[sample(1:nrow(mdat4))[1:100000],]
  start_time = Sys.time()
  S <- slsqp(starting,
             fn = fn.ancmixtures,
             hin = hin.ancmixtures,
             heq = heq.ancmixtures)
  end_time = Sys.time()
  ad = c(S$par, S$value, (end_time - start_time))
  visframe = rbind(visframe, ad)
}

ad = c(S$par, S$value, (end_time - start_time))
S$par; S$value; end_time - start_time


f = ggplot(visframe, aes(EUR))

p = ggplot(stack(visframe), aes(x = factor(ind, levels = names(visframe)), y = values))

p + geom_jitter(height = 0.01, width = 0.1) +
  labs(x = 'Ancestries from 1000 Genomes Database', y = 'Estimated Proportions')

temp.b<-c()
for(chr in 1:22){
  load(paste("mixtures/genomic_resources/1000Genomes/global_populations/Global_ancestries_BROADfiltering.chr", chr,".RData", sep="")) ##global
  temp.b<-rbind(temp.b, global[!duplicated(global$pos),])
  print(chr)
}


obfl = numeric(length(mdat4$ob))

for (i in 1:length(mdat4$ob)){
  if (mdat4$ob[i] > 0.5){
    obfl[i] = 1 - mdat4$ob[i]
  }
  else {
    obfl[i] = mdat4$ob[i]
  }
}

