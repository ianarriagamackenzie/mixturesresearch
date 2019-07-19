combini <- data.frame(total.c$CHR, total.c$SNP, total.c$A1,total.c$A2, total.c$eur_MAF, total.c$afr_MAF, total.c$eas_MAF, total.c$sas_MAF)

comb2 <- combini[complete.cases(combini), ]
names(comb2) <- c('CHR', 'SNP', 'A1', 'A2', 'eur_MAF', 'afr_MAF', 'eas_MAF', 'sas_MAF')
head(comb2)

namdat <- read.table('mixtures/genomic_resources/mixtures_nam_data/nam_afreq.frq', header = TRUE)

sum(is.na(namdat))

head(namdat)

load("~/mixtures/genomic_resources/mixtures_nam_data/1000G_nam_merged_SNP_only.Rdata")

comb4 <- all

names(namdat) <- c('CHRnam', 'SNP', 'A1nam', 'A2nam', 'nam_MAF', 'nam_NCHROBS')

comb3 <- merge(comb2, namdat, by = 'SNP', all.x = TRUE)
comb4 <- comb3[complete.cases(comb3), ]

comb4$nam_MAF_edit <- comb4$nam_MAF

indices <- numeric(length(comb4$A1))

for (i in 1:length(comb4$A1)){
  if (comb4$A1[i] == 'A' || comb4$A1[i] == 'T'){
    if (comb4$A2nam[i] == 'A' || comb4$A2nam[i] == 'T'){
      comb4$nam_MAF_edit[i] = 1 - comb4$nam_MAF[i]
      indices[i] <- i
    }
  }
  if (comb4$A1[i] == 'C' || comb4$A1[i] == 'G'){
    if (comb4$A2nam[i] == 'C' || comb4$A2nam[i] == 'G'){
      comb4$nam_MAF_edit[i] = 1 - comb4$nam_MAF[i]
      indices[i] <- i
    }
  }
}

count = 0

for (i in 1:length(comb4$A1)){
  if (comb4$A1[i] == comb4$A2nam[i]){
    if (comb4$A1nam[i] == comb4$A2[i]){
      count = count + 1
    }
  }
}

count2 <- count + sum(indices != 0)

test1 <- sample(1:length(comb4$SNP), 1)
comb4[test1 : (test1+15), ]

final_frame <- data.frame(comb4$SNP, comb4$CHR, comb4$A1, comb4$A2, comb4$eur_MAF, comb4$afr_MAF, comb4$eas_MAF, comb4$sas_MAF, comb4$nam_MAF_edit)
names(final_frame) <- c('SNP', 'CHR', 'A1', 'A2', 'eur_MAF', 'afr_MAF', 'eas_MAF', 'sas_MAF', 'nam_MAF')

write.table(comb4, file = 'resources/5ancunchangeddat.txt')
write.table(final_frame, file = 'resources/5ancfinalsimdat.txt')


