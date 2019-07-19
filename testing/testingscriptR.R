source('work/functiondb.R')

install.packages('nloptr')
library(nloptr)

pop_1 = 'eas_MAF'
pop_1_fraction = c(0,.5)
pop_2 = 'afr_MAF'
pop_2_fraction = c(.5,1)
pop_3 = 'eur_MAF'
pop_3_fraction = 0
pop_4 = 'nam_MAF'
pop_4_fraction = 0
pop_5 = 'sas_MAF'
pop_5_fraction = 0

simulations = 1


# simgen(simulations, , , , )



snpsamps <- simdat[sample(1:nrow(simdat))[1:100000],]

pop_list = genpoplist(pop_1, pop_2, pop_3, pop_4, pop_5)

pop_ind = genindlist(pop_1, pop_2, pop_3, pop_4, pop_5)

pop_frac = genpopfrac(pop_1_fraction, pop_2_fraction, pop_3_fraction, pop_4_fraction, pop_5_fraction)

popnum = popnumlist(10000, pop_frac)
sum(popnum); sum(pop_frac)

tpf = truepopfrac(10000, popnum)

starttime = Sys.time()
pop_frac = genpopfrac(pop_1_fraction, pop_2_fraction, pop_3_fraction, pop_4_fraction, pop_5_fraction)
endtime = Sys.time()

pop_frac; endtime - starttime