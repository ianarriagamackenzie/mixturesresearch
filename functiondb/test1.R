#test1
library(plotly)
library(scatterplot3d)

load("~/mixtures/genomic_resources/gnomad/referencemergedata/gnomad_ref_merge_genome.Rdata")

sampmat = gnomad_ref_merge_genome[sample(1:nrow(gnomad_ref_merge_genome))[1:100000],]

refmat = sampmat[,5:9]; obsvec = sampmat[,14]

source('~/mixtures/team_members/current_team/Ian/functiondb/RSLSQPFunction.R')

rslsqp(refmat, obsvec)

refmatrix = master_frame_final[,c(3,4)]
testmatrix = master_frame_final[,3:5]
afrprop = seq(0,1,0.01); eurprop = seq(0,1,0.01)
guessmat = data.frame(eurprop, rev(afrprop))

gridsolve = numeric(101)
for (i in 1:101){
  mat = numeric(length(refmatrix[,1]))
  mat = (testmatrix[1]*guessmat[i,1] + testmatrix[2]*guessmat[i,2] - testmatrix[3])**2
  gridsolve[i] = sum(mat)
}
plotmat = data.frame(rev(eurprop), afrprop, rev(gridsolve))
plotmat2 = plotmat[55:71,]

scatterplot3d(plotmat, type = 'h', grid = TRUE, box = FALSE, color = 'steelblue',
              angle = 60, pch = 16, main = 'Grid Search Optimization',
              xlab = 'European Ancestry Proportion', ylab = 'African Ancestry Proportion',
              zlab = 'Minimization Value')

scatterplot3d(plotmat2, type = 'h', grid = TRUE, box = FALSE, color = 'chocolate1',
              angle = 60, pch = 16, main = 'SLSQP Optimization',
              xlab = 'European Ancestry Proportion', ylab = 'African Ancestry Proportion',
              zlab = 'Minimization Value')
