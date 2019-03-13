# USING PACKAGE NLOPTR
# install.packages('nloptr')
# library(nloptr)

# sets starting fractions, currently set to 1/N where N=number of populations
starting <- numeric(pop_number)
for (i in 1:pop_number){
  starting[i] <- 1/pop_number
}

# function that is being minimized
fn.ancmixtures <- function(x){
  minfunc = 0
  for (i in 1:pop_number){
    minfunc = minfunc + x[i]*master_frame_final[i+2] 
  }
  minfunc = (minfunc - master_frame_final[pop_number + 3])**2
  return(sum(minfunc))
}

# inequality function, each proportion is greater then 0, eg x[i] > 0
hin.ancmixtures <- function(x){
  inequality <- numeric(pop_number)
  inequality[pop_number] <- x[pop_number]
  return(inequality)
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
S <- slsqp(starting, fn = fn.ancmixtures, hin = hin.ancmixtures, heq = heq.ancmixtures)
end_time = Sys.time()

S$par; end_time - start_time
