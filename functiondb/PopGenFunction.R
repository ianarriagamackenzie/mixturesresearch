# random population generator

randpropvec = function(popnum){
  randvec = sample(1:10000,popnum)
  randvec = randvec / sum(randvec)
  return(randvec)
}

popgen = function(refmatrix, propvec){
  
}