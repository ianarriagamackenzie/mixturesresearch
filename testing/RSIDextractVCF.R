JUNE2019_vcf_chr22.recode <- read.table("~/mixtures/team_members/current_team/Greg/July11_vcf_gnomad_exomes/JUNE2019_vcf_chr22.recode.vcf", quote="\"")

temp = JUNE2019_vcf_chr22.recode


testsub = sub('.*AF_afr=', '', temp$V8[150296])
testsub2 = substr(testsub,1,11)
testsub3 = as.numeric(testsub2)