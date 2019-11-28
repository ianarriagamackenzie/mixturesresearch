randsnpdat = read.csv('~/GitHub/mixturesresearch/AncEstAppBackups/randsnpdat.txt', sep='')
bbdat = read.csv('~/GitHub/mixturesresearch/AncEstAppBackups/bbdat.txt', sep='')

randsnpdat$TestNum = NULL

bbdat$TestType = rep('blockbootstrap', 33000)
bbdat$NumberSNPs = rep(NA, 33000)

bbdat2 =  bbdat %>% 
  select(names(randsnpdat))

findat = rbind(randsnpdat, bbdat2)

write.table(findat, file = '~/GitHub/mixturesresearch/AncEstTestApp/findat.txt')

dattest = dat %>% 
  filter(TestType %in% 'Random_SNPs_Genomewide') %>% 
  filter(NumberSNPs %in% 1000) %>% 
  filter(Exge %in% 'genome') %>% 
  filter(Gnomadanc %in% 'amr') %>% 
  select(AFR, EAS, EUR, NAM, SAS)
sdat = skim(dattest)
cdat = sdat %>% 
  select(skim_variable, names(sdat)[5:11])
names(cdat) = c('Ancestry', 'Mean', 'SD', 'P0', 'P25', 'P50', 'P75', 'P100')

sdat = dattest %>% 
  select(AFR, EAS, EUR, NAM, SAS) %>% 
  skim() %>% 
  select(skim_variable, numeric.mean, numeric.sd, numeric.p0, 
         numeric.p25, numeric.p50, numeric.p75, numeric.p100)
names(sdat) = c('Ancestry', 'Mean', 'SD', 'P0', 'P25', 'P50', 'P75', 'P100')
