finalframe = as.data.frame(matrix(data = 0, nrow = 0, ncol = 12))
names(finalframe) = c('SNP', 'CHR', 'A1', 'A2', 'AF_eur', 'AF_afr', 'AF_sas', 'AF_eas', 
                      'A1_nam', 'A2_nam', 'MAF_nam', 'NCHROBS_nam')


namdat = as.data.frame(read.table('~/mixtures/genomic_resources/mixtures_nam_data/nam_afreq.frq', header = TRUE))
namdat$CHR = NULL
names(namdat) = c('SNP','A1_nam','A2_nam','MAF_nam','NCHROBS_nam')


for (j in (1:22)){
  load(paste("~/mixtures/team_members/current_team/Greg/MakingGlobal/JUNE.2019_global_chr",j,".Rdata", sep = ''))
  tempchr = tmp2[,1:8]
  
  tempframe = merge(tempchr, namdat, by = 'SNP')
  finalframe = rbind(finalframe, tempframe)
}


write.table(finalframe, file = '~/work/nammerge2.txt')

nammerge2 <- read.csv("~/work/nammerge2.txt", sep="")


unique(nammerge2$CHR)

sum(duplicated(nammerge2$SNP))
sum(duplicated(namdat$SNP))

namtest = nammerge2[!duplicated(nammerge2$SNP),]

unique(namtest$CHR)

namtest$namAFflip = namtest$MAF_nam

count = 0
for (n in 1:length(namtest$MAF_nam)){
  if (namtest$A1[n] == namtest$A2_nam[n]){
    namtest$namAFflip[n] = 1 - namtest$MAF_nam[n]
    count = count + 1
  }
}

write.table(namtest, file = '~/work/namtestAFflip.txt')
refdat = namtestAFflip

refdat$MAF_nam = refdat$namAFflip
refdat$A1_nam = NULL; refdat$A2_nam = NULL; refdat$namAFflip = NULL; refdat$NCHROBS_nam = NULL

realdat <- read.delim("~/mixtures/team_members/current_team/Greg/RealDataRacas/Real_data2use_april18.txt")

merge1 = data.frame(refdat$SNP,refdat$A1, refdat$A2, refdat$AF_afr, refdat$AF_eur, refdat$AF_eas, refdat$AF_sas, refdat$MAF_nam)
names(merge1) = c('SNP', 'A1', 'A2', 'AF_afr', 'AF_eur', 'AF_eas', 'AF_sas', 'AF_nam')

merge2 = data.frame(realdat$SNP,realdat$V4, realdat$AF1, realdat$AF2, realdat$AF3)
names(merge2) = c('SNP','A2real', 'AF1', 'AF2', 'AF3')

merge3 = merge(merge1, merge2, by = 'SNP')

sum(duplicated(merge3$SNP))
merge3 = merge3[!duplicated(merge3$SNP),]

merge3$AF2real = merge3$AF2

count = 0
for (l in 1:length(merge3$AF2real)){
  if (merge3$A1[l] == merge3$A2real[l]){
    merge3$AF2real[l] = 1 - merge3$AF2[l]
    count = count + 1
  }
}


install.packages('nloptr')
library(nloptr)

master_frame_final = merge3[sample(1:nrow(merge3))[1:100000],]

master_frame_final = merge3

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
    minfunc = minfunc + x[i]*master_frame_final[i+3] 
  }
  minfunc = (minfunc - master_frame_final[13])**2
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

start_time = Sys.time()
S <- slsqp(starting,
           fn = fn.ancmixtures,
           hin = hin.ancmixtures,
           heq = heq.ancmixtures)
end_time = Sys.time()
