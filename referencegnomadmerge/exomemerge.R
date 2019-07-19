## Reference genomic data merge with gnomAD AFR data
## 7/2019, last editted: Ian Arriaga-MacKenzie
## exome gnomAD


## read in reference data, 1000 Genomes + NAM
refdat = read.csv("/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Ian/nammerge/reference1000GNAM.txt", sep="")

# initialize output merged data frame
finalframe = as.data.frame(matrix(data = 0, nrow = 0, ncol = 15))

# initialize output info data frame
mergeinfo = as.data.frame(matrix(data = 0, nrow = 0, ncol = 14))


# loop over 1-22 chromosomes
for (j in 1:22){
  
  # variables for information frame, set to 0 for each CHR
  
  # error check to make sure POS, A1, A2 are equal for each CHR merge
  # merge 2 seperate CHR files with cbind, value should = 0
  gnomadfilematcherror = 0
  
  # duplicate SNPs in gnomAD, indication of triallelic snps, removed from data
  gnomaddupsnps = 0
  
  # total number of SNPs merged between reference and gnomAD, per CHR
  totmergenum = 0
  
  # number of SNPs in reference, per chromosome
  sumsnpsref = 0
  
  # number of SNPs in gnomAD, per chromosome, not all SNPs have rsID info
  totsumsnpsgnom = 0
  
  # number of SNPs with rsID info in gnomAD
  sumsnpsgnomad = 0
  
  # A1 = A1, A2 = A2 matching
  samecount = 0
  
  # A1 = A2, A2 = A1 AF flipped
  AFflipcount = 0
  
  # A1 = A1, A2 = A2 strand flipped and matching
  SFcount = 0
  
  # A1 = A2, A2 = A1 strand flipped and AF flipped
  SFAFcount = 0
  
  # not matching, removed
  RMcount = 0
  
  # read in gnomAD data files
  # remove non-important columns, 2 seperate data files
  tempfile = read.delim(paste("/nfs/storage/math/gross-s2/projects/mixtures/genomic_resources/gnomad/realdata_summer2019/exome/chr",j,".african_JUNE2019.INFO", sep = ''))
  tempfile$CHROM = NULL; tempfile$AC_afr = NULL; tempfile$AN_afr = NULL; tempfile$nhomalt_afr = NULL
  
  tempfile2 = read.table(paste("/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Greg/July11_vcf_gnomad_exomes/JUNE2019_vcf_chr",j,".recode.vcf", sep = ''), quote="\"")
  tempfile2$V1 = NULL; tempfile2$V6 = NULL; tempfile2$V7 = NULL; tempfile2$V8 = NULL
  
  # bind data files together
  tf3 = cbind(tempfile, tempfile2)
  
  # check to make sure A1, A2, POS are equal for both data files
  if ((all.equal(tf3$POS, tf3$V2)) == FALSE || (all.equal(tf3$REF, tf3$V4)) == FALSE || (all.equal(tf3$ALT, tf3$V5)) == FALSE){
    gnomadfilematcherror = 1
  }
  
  #remove non-important columns, rename
  tf3$V2 = NULL; tf3$V4 = NULL; tf3$V5 = NULL
  names(tf3) = c('POS', 'A1real', 'A2real', 'AFreal_AFR', 'SNP')
  
  # counters listed above
  totsumsnpsgnom = length(tf3$SNP)
  sumsnpsgnomad = sum(tf3$SNP != '.')
  sumsnpsref = sum(refdat$CHR == j)
  
  # merge reference data with cleaned gnomAD data
  temptest = merge(refdat, tf3, by = 'SNP')
  
  # counter listed above
  gnomaddupsnps = sum(duplicated(temptest$SNP))
  
  # remove duplicated rsID, indication of triallelic SNPs
  removetriallele = temptest$SNP[which(duplicated(temptest$SNP))]
  temptest = temptest[-which(temptest$SNP %in% removetriallele),]
  
  # convert to numeric
  temptest$AFreal_AFR = as.numeric(as.character(temptest$AFreal_AFR))
  
  #initialize vector in which gnomAD is matched to reference
  temptest$AFR_mod2 = numeric(length(temptest$AFreal_AFR))
  
  # counter listed above
  totmergenum = length(temptest$SNP)
  
  # A1 reference = ALT gnomad and A2 reference = REF gnomad
  # Matches A1, A2 in our reference and gnomAD data
  # takes into account flips and strand flips, counters listed above
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
  
  # remove non matching A1, A2 SNPs
  # temptest = temptest[-which(is.na(temptest$AFR_mod2)),]
  
  # percent merged in reference and gnomad
  pctmergegnomad = totmergenum/totsumsnpsgnom
  pctmergeref = totmergenum/sumsnpsref
  
  # create row for information data frame
  mergeinfoframe = data.frame(j, totsumsnpsgnom, pctmergegnomad, sumsnpsgnomad, sumsnpsref, pctmergeref,gnomaddupsnps, totmergenum, samecount, AFflipcount, SFcount, SFAFcount, RMcount, gnomadfilematcherror)
  
  #merge both data frames per chromosome
  finalframe = rbind(finalframe, temptest)
  mergeinfo = rbind(mergeinfo, mergeinfoframe)
}

# removing SNPs where AF information was non-numeric
finalframe = finalframe[-which(is.na(finalframe$AFR_mod2)),]

# total percentages
pctmergegnomadtot = sum(mergeinfo$totmergenum) / sum(mergeinfo$totsumsnpsgnom)
pctmergereftot = sum(mergeinfo$totmergenum) / sum(mergeinfo$sumsnpsref)

# totals over all 22 chromosomes for info frame, bind to frame
mergeinfototals = data.frame('Totals', sum(mergeinfo$totsumsnpsgnom), pctmergegnomadtot, sum(mergeinfo$sumsnpsgnomad), sum(mergeinfo$sumsnpsref), pctmergereftot, sum(mergeinfo$gnomaddupsnps), sum(mergeinfo$totmergenum), sum(mergeinfo$samecount), sum(mergeinfo$AFflipcount), sum(mergeinfo$SFcount), sum(mergeinfo$SFAFcount), sum(mergeinfo$RMcount), NA)
names(mergeinfototals) = names(mergeinfo)
mergeinfo = rbind(mergeinfo, mergeinfototals)

# rename columns and data frames for writing files
names(finalframe) = c('SNP', 'CHR', 'A1', 'A2', 'AF_eur', 'AF_afr', 'AF_sas', 'AF_eas', 'AF_nam', 'POS', 'gnomad_REF', 'gnomad_ALT', 'gnomad_AF_afr', 'gnomad_AF_afr_matched')
names(mergeinfo) = c('CHR','Total_SNPnum_gnomad','Pct_merge_gnomad', 'SNPnum_gnomad_rsID', 'SNPnum_ref','Pct_merge_ref','triallelic_gnomad_merge_removed', 'tot_merge_num', 'samecount', 'flipcount', 'SFcount', 'SFflipcount', 'removecount', 'gnomadmatcherror')
gnomad_ref_merge_exome = finalframe
merge_info_exome = mergeinfo


#write files in txt and Rdata
write.table(gnomad_ref_merge_exome, file = '/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Ian/refgnomadmerge/exome/gnomad_ref_merge_exome.txt')

write.table(merge_info_exome, file = '/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Ian/refgnomadmerge/exome/merge_info_exome.txt')

save(gnomad_ref_merge_exome, file = '/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Ian/refgnomadmerge/exome/gnomad_ref_merge_exome.Rdata')

save(merge_info_exome, file = '/nfs/storage/math/gross-s2/projects/mixtures/team_members/current_team/Ian/refgnomadmerge/exome/merge_info_exome.Rdata')

