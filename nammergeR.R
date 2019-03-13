combini <- data.frame(total.c$CHR, total.c$A1,total.c$A2, total.c$eur_MAF, total.c$afr_MAF, total.c$eas_MAF, total.c$sas_MAF)

comb2 <- combini[complete.cases(combini), ]

head(comb2)

namdat <- read.table('mixtures/genomic_resources/mixtures_nam_data/nam_afreq.frq', header = TRUE)

sum(is.na(namdat))





indices <- numeric(length(totdatmerge$A1.x))

for (i in 1:length(totdatmerge$A1.x)){
  if (totdatmerge$A1.x[i] == 'A' || totdatmerge$A1.x[i] == 'T'){
    if (totdatmerge$A2.y[i] == 'A' || totdatmerge$A2.y[i] == 'T'){
      totdatmerge$NAM_MAF_edit[i] = 1 - totdatmerge$NAM_MAF[i]
      indices[i] <- i
    }
  }
  if (totdatmerge$A1.x[i] == 'C' || totdatmerge$A1.x[i] == 'G'){
    if (totdatmerge$A2.y[i] == 'C' || totdatmerge$A2.y[i] == 'G'){
      totdatmerge$NAM_MAF_edit[i] = 1 - totdatmerge$NAM_MAF[i]
      indices[i] <- i
    }
  }
}

count = 0

for (i in 1:length(totdatmerge$A1.x)){
  if (totdatmerge$A1.x[i] == totdatmerge$A2.y[i]){
    if (totdatmerge$A1.y[i] == totdatmerge$A2.x[i]){
      count = count + 1
    }
  }
}