install.packages('nloptr')

finalframe = as.data.frame(matrix(data = 0, nrow = 0, ncol = 15))
names(finalframe) = c('SNP', 'CHR', 'A1', 'A2', 'AF_eur', 'AF_afr', 'AF_sas', 'AF_eas', 
                      'AF_nam', 'POS', 'A1real', 'A2real', 'AFreal_AFR', 'AFR_mod', 'AFR_mod2')

# read NAM + 1000G data
namtestAFflip <- read.csv("~/mixtures/team_members/current_team/Ian/NAMtestrun.txt", sep="")

# remove values where A1r != A2nam and A2r != A2nam
namtest2 = namtestAFflip[-which(namtestAFflip$A1[which(namtestAFflip$A2 != namtestAFflip$A2_nam)] != namtestAFflip$A2_nam[which(namtestAFflip$A2 != namtestAFflip$A2_nam)]),]
rm(namtestAFflip)

# consolidating merge data frame
refdat = namtest2
refdat$MAF_nam = refdat$namAFflip
refdat$A1_nam = NULL; refdat$A2_nam = NULL; refdat$namAFflip = NULL; refdat$NCHROBS_nam = NULL


#loading gregs gnomAD data and cleaning it
j = 22

tempfile = read.delim(paste("~/mixtures/genomic_resources/gnomad/realdata_summer2019/genome/chr",j,".african_JUNE2019.INFO", sep = ''))
tempfile$CHROM = NULL; tempfile$AC_afr = NULL; tempfile$AN_afr = NULL; tempfile$nhomalt_afr = NULL

tempfile2 = read.table(paste("~/mixtures/team_members/current_team/Greg/July11_vcf_gnomad_genomes/JUNE2019_vcf_chr",j,".recode.vcf", sep = ''), quote="\"")
tempfile2$V1 = NULL; tempfile2$V6 = NULL; tempfile2$V7 = NULL; tempfile2$V8 = NULL

tf3 = cbind(tempfile, tempfile2)

# isTRUE(all.equal(tf3$POS, tf3$V2))
# isTRUE(all.equal(tf3$REF, tf3$V4))
# isTRUE(all.equal(tf3$ALT, tf3$V5))

tf3$V2 = NULL; tf3$V4 = NULL; tf3$V5 = NULL

names(tf3) = c('POS', 'A1real', 'A2real', 'AFreal_AFR', 'SNP')

# merging nomad data with 1000g ref data
tempfile3 = tempfile[which(refdat$SNP %in% tf3$SNP),]

temptest = merge(refdat, tf3, by = 'SNP')
temptest = temptest[-which(duplicated(temptest$SNP)),]

temptest$AFreal_AFR = as.numeric(as.character(temptest$AFreal_AFR))

# removing non numeric values
#temptest = temptest[-which(is.na(as.numeric(as.character(temptest$AFreal_AFR)))),]

temptest$AFreal_AFR = as.numeric(as.character(temptest$AFreal_AFR))
temptest$AFR_mod = numeric(length(temptest$AFreal_AFR))
temptest$AFR_mod2 = numeric(length(temptest$AFreal_AFR))

samecount = 0
AFflipcount = 0
SFcount = 0
SFAFcount = 0
RMcount = 0


# A1 = A2 and A2 = A1

for (i in (1:length(temptest$SNP))){
  if (temptest$A1[i] == 'A' && temptest$A2[i] == 'G'){
    if (temptest$A2real[i] == 'A' && temptest$A1real[i] == 'G'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A2real[i] == 'G' && temptest$A1real[i] == 'A'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A2real[i] == 'T' && temptest$A1real[i] == 'C'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A2real[i] == 'C' && temptest$A1real[i] == 'T'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod2[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'A' && temptest$A2[i] == 'C'){
    if (temptest$A2real[i] == 'A' && temptest$A1real[i] == 'C'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A2real[i] == 'C' && temptest$A1real[i] == 'A'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A2real[i] == 'T' && temptest$A1real[i] == 'G'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A2real[i] == 'G' && temptest$A1real[i] == 'T'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod2[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'T' && temptest$A2[i] == 'G'){
    if (temptest$A2real[i] == 'T' && temptest$A1real[i] == 'G'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A2real[i] == 'G' && temptest$A1real[i] == 'T'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A2real[i] == 'A' && temptest$A1real[i] == 'C'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A2real[i] == 'C' && temptest$A1real[i] == 'A'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod2[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'T' && temptest$A2[i] == 'C'){
    if (temptest$A2real[i] == 'T' && temptest$A1real[i] == 'C'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A2real[i] == 'C' && temptest$A1real[i] == 'T'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A2real[i] == 'A' && temptest$A1real[i] == 'G'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A2real[i] == 'G' && temptest$A1real[i] == 'A'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod2[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'G' && temptest$A2[i] == 'A'){
    if (temptest$A2real[i] == 'G' && temptest$A1real[i] == 'A'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A2real[i] == 'A' && temptest$A1real[i] == 'G'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A2real[i] == 'C' && temptest$A1real[i] == 'T'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A2real[i] == 'T' && temptest$A1real[i] == 'C'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod2[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'G' && temptest$A2[i] == 'T'){
    if (temptest$A2real[i] == 'G' && temptest$A1real[i] == 'T'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A2real[i] == 'T' && temptest$A1real[i] == 'G'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A2real[i] == 'C' && temptest$A1real[i] == 'A'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A2real[i] == 'A' && temptest$A1real[i] == 'C'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod2[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'C' && temptest$A2[i] == 'A'){
    if (temptest$A2real[i] == 'C' && temptest$A1real[i] == 'A'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A2real[i] == 'A' && temptest$A1real[i] == 'C'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A2real[i] == 'G' && temptest$A1real[i] == 'T'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A2real[i] == 'T' && temptest$A1real[i] == 'G'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod2[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'C' && temptest$A2[i] == 'T'){
    if (temptest$A2real[i] == 'C' && temptest$A1real[i] == 'T'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A2real[i] == 'T' && temptest$A1real[i] == 'C'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A2real[i] == 'G' && temptest$A1real[i] == 'A'){
      temptest$AFR_mod2[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A2real[i] == 'A' && temptest$A1real[i] == 'G'){
      temptest$AFR_mod2[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod2[i] = NA
      RMcount = RMcount + 1
    }
  } else {
    temptest$AFR_mod2[i] = NA
    RMcount = RMcount + 1
  }
}

temptest = temptest[-which(is.na(temptest$AFR_mod2)),]

finalframe = rbind(finalframe, temptest)

finalframe2 = finalframe

finalframe2$AFR_mod = NULL
names(finalframe2) = c('SNP', 'CHR', 'A1', 'A2', 'AF_eur', 'AF_afr', 'AF_sas', 'AF_eas', 'AF_nam', 'POS', 'gnomad_REF', 'gnomad_ALT', 'gnomad_AF_afr', 'gnomad_AF_afr_correct')



write.table(finalframe, file = '~/mixtures/team_members/current_team/Ian/datafiles/raw1000GNAM_gnomad_merge.txt')

write.table(refdat, file = '~/mixtures/team_members/current_team/Ian/datafiles/1000GNAMreference.txt')

write.table(finalframe2, file = '~/mixtures/genomic_resources/referencedata/NAM1000GgnomADtest.txt')

write.table(`1000GNAMreference`, file = '~/mixtures/genomic_resources/referencedata/NAM_1000G_referencedat.txt')


# a1 = a1 and a2 = a2
# for (i in (1:length(temptest$SNP))){
#   if (temptest$A1[i] == 'A' && temptest$A2[i] == 'G'){
#     if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'G'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       samecount = samecount + 1
#     } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'A'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       AFflipcount = AFflipcount + 1
#     } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'C'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       SFcount = SFcount + 1
#     } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'T'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       SFAFcount = SFAFcount + 1
#     } else {
#       temptest$AFR_mod[i] = NA
#       RMcount = RMcount + 1
#     }
#   } else if (temptest$A1[i] == 'A' && temptest$A2[i] == 'C'){
#     if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'C'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       samecount = samecount + 1
#     } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'A'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       AFflipcount = AFflipcount + 1
#     } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'G'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       SFcount = SFcount + 1
#     } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'T'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       SFAFcount = SFAFcount + 1
#     } else {
#       temptest$AFR_mod[i] = NA
#       RMcount = RMcount + 1
#     }
#   } else if (temptest$A1[i] == 'T' && temptest$A2[i] == 'G'){
#     if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'G'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       samecount = samecount + 1
#     } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'T'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       AFflipcount = AFflipcount + 1
#     } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'C'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       SFcount = SFcount + 1
#     } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'A'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       SFAFcount = SFAFcount + 1
#     } else {
#       temptest$AFR_mod[i] = NA
#       RMcount = RMcount + 1
#     }
#   } else if (temptest$A1[i] == 'T' && temptest$A2[i] == 'C'){
#     if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'C'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       samecount = samecount + 1
#     } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'T'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       AFflipcount = AFflipcount + 1
#     } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'G'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       SFcount = SFcount + 1
#     } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'A'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       SFAFcount = SFAFcount + 1
#     } else {
#       temptest$AFR_mod[i] = NA
#       RMcount = RMcount + 1
#     }
#   } else if (temptest$A1[i] == 'G' && temptest$A2[i] == 'A'){
#     if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'A'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       samecount = samecount + 1
#     } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'G'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       AFflipcount = AFflipcount + 1
#     } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'T'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       SFcount = SFcount + 1
#     } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'C'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       SFAFcount = SFAFcount + 1
#     } else {
#       temptest$AFR_mod[i] = NA
#       RMcount = RMcount + 1
#     }
#   } else if (temptest$A1[i] == 'G' && temptest$A2[i] == 'T'){
#     if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'T'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       samecount = samecount + 1
#     } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'G'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       AFflipcount = AFflipcount + 1
#     } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'A'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       SFcount = SFcount + 1
#     } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'C'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       SFAFcount = SFAFcount + 1
#     } else {
#       temptest$AFR_mod[i] = NA
#       RMcount = RMcount + 1
#     }
#   } else if (temptest$A1[i] == 'C' && temptest$A2[i] == 'A'){
#     if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'A'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       samecount = samecount + 1
#     } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'C'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       AFflipcount = AFflipcount + 1
#     } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'T'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       SFcount = SFcount + 1
#     } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'G'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       SFAFcount = SFAFcount + 1
#     } else {
#       temptest$AFR_mod[i] = NA
#       RMcount = RMcount + 1
#     }
#   } else if (temptest$A1[i] == 'C' && temptest$A2[i] == 'T'){
#     if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'T'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       samecount = samecount + 1
#     } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'C'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       AFflipcount = AFflipcount + 1
#     } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'A'){
#       temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
#       SFcount = SFcount + 1
#     } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'G'){
#       temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
#       SFAFcount = SFAFcount + 1
#     } else {
#       temptest$AFR_mod[i] = NA
#       RMcount = RMcount + 1
#     }
#   } else {
#     temptest$AFR_mod[i] = NA
#     RMcount = RMcount + 1
#   }
# }


library(nloptr)

master_frame_final = finalframe2[sample(1:nrow(finalframe2))[1:100000],]

master_frame_final = finalframe2

#master_frame_final = master_frame_final[-which(master_frame_final$AFreal_AFR == 0),]

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
  minfunc = (minfunc - master_frame_final[14])**2
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
