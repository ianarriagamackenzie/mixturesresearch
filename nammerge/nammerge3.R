## Native American - 1000 Genomes SNP AF merge
## 7/2019. Last editted: Ian Arriaga-MacKenzie

# initialize output data frame
finalframe = as.data.frame(matrix(data = 0, nrow = 0, ncol = 12))
names(finalframe) = c('SNP', 'CHR', 'A1', 'A2', 'AF_eur', 'AF_afr', 'AF_sas', 'AF_eas', 
                      'A1_nam', 'A2_nam', 'AF_nam', 'NCHROBS_nam')


# read in NAM data, clean it
namdat = as.data.frame(read.table('/nfs/storage/math/gross-s2/projects/mixtures/genomic_resources/mixtures_nam_data/nam_afreq.frq', header = TRUE))
namdat$CHR = NULL
names(namdat) = c('SNP','A1_nam','A2_nam','AF_nam','NCHROBS_nam')

# loop over 22 chromosomes
for (j in (1:22)){
  
  # load and clean chromosome
  load(paste("/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Greg/MakingGlobal/JUNE.2019_global_chr",j,".Rdata", sep = ''))
  tempchr = tmp2[,1:8]
  
  # merge by SNP
  tempframe = merge(tempchr, namdat, by = 'SNP')
  
  # bind to output frame
  finalframe = rbind(finalframe, tempframe)
}

# initialize vector for matched AF on A1, A2
finalframe$namAFflip = numeric(length(finalframe$AF_nam))

# incomplete A1 information given in NAM
# A2 NAM = A2 1000G, matched, original AF
# A2 NAM = A1 1000G, flipped AF
# else removed
for (n in 1:length(finalframe$AF_nam)){
  if (finalframe$A1[n] == finalframe$A2_nam[n]){
    finalframe$namAFflip[n] = 1 - finalframe$AF_nam[n]
  } else if (finalframe$A2[n] == finalframe$A2_nam[n]){
    finalframe$namAFflip[n] = finalframe$AF_nam[n]
  } else {
    finalframe$namAFflip[n] = NA
  }
}

# save raw data file for comparison, clean later
save(finalframe, file = '/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Ian/nammerge/refdat.Rdata')