## 1000 Genomes-Native American reference data clean
## 7/2019, last editted: Ian Arriaga-MacKenzie

# load original reference data
load("/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Ian/nammerge/refdat.Rdata", .GlobalEnv)

# remove non-matching SNPs from frame
referencedata = finalframe[-which(is.na(finalframe$namAFflip)),]

#remove old vectors, rename columns
referencedata$A1_nam = NULL; referencedata$A2_nam = NULL; referencedata$AF_nam = NULL; referencedata$NCHROBS_nam = NULL
names(referencedata) = c('SNP', 'CHR', 'A1', 'A2', 'AF_eur', 'AF_afr', 'AF_sas', 'AF_eas', 'AF_nam')

# save cleaned data file, txt and Rdata
save(referencedata, file = '/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Ian/nammerge/reference1000GNAM.Rdata')
write.table(referencedata, '/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Ian/nammerge/reference1000GNAM.txt')
