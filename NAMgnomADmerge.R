# read NAM + 1000G data
namtestAFflip <- read.csv("~/work/namtestAFflip.txt", sep="")

# remove values where A1r != A2nam and A2r != A2nam
namtest2 = namtestAFflip[-which(namtestAFflip$A1[which(namtestAFflip$A2 != namtestAFflip$A2_nam)] != namtestAFflip$A2_nam[which(namtestAFflip$A2 != namtestAFflip$A2_nam)]),]
rm(namtestAFflip)

# consolidating merge data frame
refdat = namtest2
refdat$MAF_nam = refdat$namAFflip
refdat$A1_nam = NULL; refdat$A2_nam = NULL; refdat$namAFflip = NULL; refdat$NCHROBS_nam = NULL


#loading gregs gnomAD data and cleaning it
j = 1
tempfile = read.delim(paste("~/mixtures/genomic_resources/gnomad/realdata_summer2019/exome/chr",j,".african_JUNE2019.INFO", sep = ''))
tempfile$CHROM = NULL; tempfile$AC_afr = NULL; tempfile$AN_afr = NULL; tempfile$nhomalt_afr = NULL
names(tempfile) = c('SNP', 'A1real', 'A2real', 'AFreal_AFR')
tempfile$SNP = paste('rs',tempfile$SNP, sep = '')

# merging nomad data with 1000g ref data
tempfile2 = tempfile[which(refdat$SNP %in% tempfile$SNP),]

temptest = merge(refdat, tempfile, by = 'SNP')
temptest = temptest[-which(duplicated(temptest$SNP)),]

# removing non numeric values
temptest = temptest[-which(is.na(as.numeric(as.character(temptest$AFreal_AFR)))),]

temptest$AFreal_AFR = as.numeric(as.character(temptest$AFreal_AFR))
temptest$AFR_mod = numeric(length(temptest$AFreal_AFR))

samecount = 0
AFflipcount = 0
SFcount = 0
SFAFcount = 0
RMcount = 0

for (i in (1:length(temptest$SNP))){
  if (temptest$A1[i] == 'A' && temptest$A2[i] == 'G'){
    if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'G'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'A'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'C'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'T'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'A' && temptest$A2[i] == 'C'){
    if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'C'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'A'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'G'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'T'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'T' && temptest$A2[i] == 'G'){
    if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'G'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'T'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'C'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'A'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'T' && temptest$A2[i] == 'C'){
    if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'C'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'T'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'G'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'A'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'G' && temptest$A2[i] == 'A'){
    if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'A'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'G'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'T'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'C'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'G' && temptest$A2[i] == 'T'){
    if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'T'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'G'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'A'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'C'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'C' && temptest$A2[i] == 'A'){
    if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'A'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'C'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'T'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'G'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod[i] = NA
      RMcount = RMcount + 1
    }
  } else if (temptest$A1[i] == 'C' && temptest$A2[i] == 'T'){
    if (temptest$A1real[i] == 'C' && temptest$A2real[i] == 'T'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      samecount = samecount + 1
    } else if (temptest$A1real[i] == 'T' && temptest$A2real[i] == 'C'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      AFflipcount = AFflipcount + 1
    } else if (temptest$A1real[i] == 'G' && temptest$A2real[i] == 'A'){
      temptest$AFR_mod[i] = temptest$AFreal_AFR[i]
      SFcount = SFcount + 1
    } else if (temptest$A1real[i] == 'A' && temptest$A2real[i] == 'G'){
      temptest$AFR_mod[i] = 1 - temptest$AFreal_AFR[i]
      SFAFcount = SFAFcount + 1
    } else {
      temptest$AFR_mod[i] = NA
      RMcount = RMcount + 1
    }
  } else {
    temptest$AFR_mod[i] = NA
    RMcount = RMcount + 1
  }
}
