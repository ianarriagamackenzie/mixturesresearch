mergedf = as.data.frame(read.table(gzfile(paste0("~/mixtures/genomic_resources/TG_freqs/original/TGP.v5a.chr21.unrelateds.jan09-2017.IBS.frq.gz")),as.is=T, header = TRUE))


names(mergedf) = c('CHR', 'SNP', 'A1_IBS', 'A2_IBS', 'MAF_IBS', 'NCHROBS_IBS')
mergedf = mergedf[which(duplicated(mergedf$SNP) ==FALSE),]

pop_list = c("IBS", "TSI", "GWD", "ACB", "ITU", "ASW", "JPT", "BEB", "KHV", "CDX", "LWK", "CEU", "MSL", "CHB", "MXL", "CHS", "PEL", "CLM", "PJL", "ESN", "PUR", "FIN", "STU", "GBR", "GIH", "YRI")

saves = (numeric(25))
for (i in (1:25)){
  saves[i] = paste('mdat_', pop_list[i+1], sep = '')
}
count = 0

for (j in (2:26)){
  pop_temp = as.data.frame(read.table(gzfile(paste0("~/mixtures/genomic_resources/TG_freqs/original/TGP.v5a.chr21.unrelateds.jan09-2017.",pop_list[j],".frq.gz")),as.is=T, header = TRUE))
  pop_temp = pop_temp[which(duplicated(pop_temp$SNP) ==FALSE),]
  
  pop_temp$MAFflip = pop_temp$MAF
  
  for (n in 1:length(pop_temp$MAF)){
    if (pop_temp$A1[n] == mergedf$A2_IBS[n]){
      pop_temp$MAFflip[n] = 1 - pop_temp$MAF[n]
      count = count + 1
    }
  }
  
  pop_temp$MAF = pop_temp$MAFflip
  
  pop_temp$A1 = NULL; pop_temp$A2 = NULL; pop_temp$CHR = NULL; pop_temp$MAFflip = NULL
  names(pop_temp) = c('SNP', paste('MAF_', pop_list[j], sep = ''), paste('NCHROBS_', pop_list[j], sep = ''))
  
  assign(saves[j-1], pop_temp)
}


paste('mdat_', pop_list[4], sep = '')



mergedf = merge(mergedf, mdat_TSI , by = 'SNP')
mergedf = merge(mergedf, mdat_GWD , by = 'SNP')
mergedf = merge(mergedf, mdat_ACB , by = 'SNP')
mergedf = merge(mergedf, mdat_ITU , by = 'SNP')
mergedf = merge(mergedf, mdat_ASW , by = 'SNP')
mergedf = merge(mergedf, mdat_JPT , by = 'SNP')
mergedf = merge(mergedf, mdat_BEB , by = 'SNP')
mergedf = merge(mergedf, mdat_KHV , by = 'SNP')
mergedf = merge(mergedf, mdat_CDX , by = 'SNP')
mergedf = merge(mergedf, mdat_LWK , by = 'SNP')
mergedf = merge(mergedf, mdat_CEU , by = 'SNP')
mergedf = merge(mergedf, mdat_MSL , by = 'SNP')
mergedf = merge(mergedf, mdat_CHB , by = 'SNP')
mergedf = merge(mergedf, mdat_MXL , by = 'SNP')
mergedf = merge(mergedf, mdat_CHS , by = 'SNP')
mergedf = merge(mergedf, mdat_PEL , by = 'SNP')
mergedf = merge(mergedf, mdat_CLM , by = 'SNP')
mergedf = merge(mergedf, mdat_PJL , by = 'SNP')
mergedf = merge(mergedf, mdat_ESN , by = 'SNP')
mergedf = merge(mergedf, mdat_PUR , by = 'SNP')
mergedf = merge(mergedf, mdat_FIN , by = 'SNP')
mergedf = merge(mergedf, mdat_STU , by = 'SNP')
mergedf = merge(mergedf, mdat_GBR , by = 'SNP')
mergedf = merge(mergedf, mdat_GIH , by = 'SNP')
mergedf = merge(mergedf, mdat_YRI , by = 'SNP')

mergedf = mergedf[(which(nchar(mergedf$A1_IBS) == 1)),]
mergedf = mergedf[(which(nchar(mergedf$A2_IBS) == 1)),]


finaldat =  as.data.frame(mergedf$CHR)
names(finaldat) = c('CHR')
finaldat$SNP = mergedf$SNP

finaldat$A1_IBS = mergedf$A1_IBS; finaldat$A2_IBS = mergedf$A2_IBS

finaldat$eas_AF = (mergedf$MAF_JPT*mergedf$NCHROBS_JPT + mergedf$MAF_CHS*mergedf$NCHROBS_CHS + mergedf$MAF_CDX*mergedf$NCHROBS_CDX + mergedf$MAF_KHV*mergedf$NCHROBS_KHV + mergedf$MAF_CHB*mergedf$NCHROBS_CHB)/(mergedf$NCHROBS_JPT + mergedf$NCHROBS_CHS + mergedf$NCHROBS_CDX + mergedf$NCHROBS_KHV + mergedf$NCHROBS_CHB)
finaldat$sas_AF = (mergedf$MAF_GIH*mergedf$NCHROBS_GIH + mergedf$MAF_PJL*mergedf$NCHROBS_PJL + mergedf$MAF_BEB*mergedf$NCHROBS_BEB + mergedf$MAF_STU*mergedf$NCHROBS_STU + mergedf$MAF_ITU*mergedf$NCHROBS_ITU)/(mergedf$NCHROBS_GIH + mergedf$NCHROBS_PJL + mergedf$NCHROBS_BEB + mergedf$NCHROBS_STU + mergedf$NCHROBS_ITU)
finaldat$afr_AF = (mergedf$MAF_LWK*mergedf$NCHROBS_LWK + mergedf$MAF_GWD*mergedf$NCHROBS_GWD + mergedf$MAF_MSL*mergedf$NCHROBS_MSL + mergedf$MAF_ESN*mergedf$NCHROBS_ESN + mergedf$MAF_YRI*mergedf$NCHROBS_YRI)/(mergedf$NCHROBS_LWK + mergedf$NCHROBS_GWD + mergedf$NCHROBS_MSL + mergedf$NCHROBS_ESN + mergedf$NCHROBS_YRI)
finaldat$eur_AF = (mergedf$MAF_CEU*mergedf$NCHROBS_CEU + mergedf$MAF_TSI*mergedf$NCHROBS_TSI + mergedf$MAF_IBS*mergedf$NCHROBS_IBS + mergedf$MAF_GBR*mergedf$NCHROBS_GBR)/(mergedf$NCHROBS_CEU + mergedf$NCHROBS_TSI + mergedf$NCHROBS_IBS + mergedf$NCHROBS_GBR)


write.table(finaldat, 'trueAF_chr21_test')

trueAF_chr21_test <- read.csv("~/work/trueAF_chr21_test", sep="")
load("~/mixtures/team_members/current_team/Greg/MakingGlobal/JUNE.2019_global_chr21.Rdata")
test1 = tmp2[,c(1:8)]
rm(tmp2)

trueAF_chr21_test$CHR = NULL; test1$CHR = NULL

mergetest = merge(test1, trueAF_chr21_test, by = 'SNP')

isTRUE(all.equal(mergetest$AF_afr, mergetest$afr_AF))
isTRUE(all.equal(mergetest$AF_eur, mergetest$eur_AF))
isTRUE(all.equal(mergetest$AF_sas, mergetest$sas_AF))
isTRUE(all.equal(mergetest$AF_eas, mergetest$eas_AF))

install.packages('dplyr')
library(dplyr)

sample_n(mergetest, size = 15)
