# Sridevi Polavaram's Capstone Project ####
# read the provider aggregate file
npi_file <- "C:/Users/sridevi/Projects/TakeHome/DataIncubatorChallenge2017/PartD_Prescriber_PUF_NPI_14.txt"
npi_master <- read.csv(npi_file,header = TRUE, sep="\t",stringsAsFactors=FALSE,strip.white = TRUE)

unique(npi_master$MEDICARE_PRVDR_ENROLL_STATUS)
range(npi_master$BENE_COUNT)
# set the suppressed values to minimum = 10 for "Number of Medicare Beneficiaries"
npi_master$BENE_COUNT = setMinValues(npi_master$BENE_COUNT, 10)
npi_master$BRAND_CLAIM_COUNT = setMinValues(npi_master$BRAND_CLAIM_COUNT, 10)
npi_master$BRAND_DRUG_COST = setMinValues(npi_master$BRAND_DRUG_COST, 10)
npi_master$GENERIC_CLAIM_COUNT = setMinValues(npi_master$GENERIC_CLAIM_COUNT, 10)
npi_master$GENERIC_DRUG_COST = setMinValues(npi_master$GENERIC_DRUG_COST, 10)

npi_master$NPPES_PROVIDER_GENDER = setMissingValues(npi_master$NPPES_PROVIDER_GENDER, "O")
unique(npi_master$NPPES_PROVIDER_GENDER)
unique(npi_master$SPECIALTY_DESCRIPTION)
# group by "SPECIALTY_DESCRIPTION" ####
library(data.table)
library(lattice)
dt <- data.table(npi_master)
dim(dt)
names(dt)
# compute average brand and generic drug cost for providers in a given specialty
byspecialty <- dt[,
   .(BENE_CNT = sum(BENE_COUNT),
     NPI_CNT = length(NPI),
     BRAND_CNT = sum(BRAND_CLAIM_COUNT),
     BRAND_TOT_CST = sum(BRAND_DRUG_COST),
     BRAND_AVG_CST = sum(BRAND_DRUG_COST) / length(NPI),
     GENE_CNT = sum(GENERIC_CLAIM_COUNT),
     GENE_TOT_CST = sum(GENERIC_DRUG_COST),
     GENE_AVG_CST = sum(GENERIC_DRUG_COST)/ length(NPI)),
   by = .(SPECIALTY = SPECIALTY_DESCRIPTION)][BENE_CNT > 10][order(-BENE_CNT)]

dim(byspecialty)
byspecialty
byspecialty

#compute the drugs cost only for top 5 specialty providers
bytop5 <- dt[SPECIALTY_DESCRIPTION %in% c("Internal Medicine","Family Practice","Dentist","Nurse Practitioner","Physician Assistant"),
             .(NPI, NPPES_PROVIDER_GENDER, SPECIALTY_DESCRIPTION, NPPES_PROVIDER_STATE,BENE_COUNT,TOTAL_CLAIM_COUNT,TOTAL_DRUG_COST,BRAND_CLAIM_COUNT,BRAND_DRUG_COST,GENERIC_CLAIM_COUNT,GENERIC_DRUG_COST)][NPPES_PROVIDER_GENDER!="O"]
dim(bytop5)
names(bytop5)

# plot #beneficieries vs. #speciality providers showing the total and avg. cost grouped by speciality
library(ggplot2)

names(byspecialty)
p <- ggplot(byspecialty[1:20,], aes(x = factor(SPECIALTY), y = BENE_CNT))
p + geom_bar(stat = "identity",width=0.3) + labs(x= 'Specialty Providers', y = '#Beneficieries') + coord_flip() + theme_bw()

# complete data set showing the coverage of number of beneficieries and number of providers
a <- ggplot(byspecialty, aes(BENE_CNT, NPI_CNT)) +
  geom_point(na.rm = TRUE) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  
  theme_bw()

a+annotation_logticks()

# scatter plot of actual counts of Beneficieries and Providers differentiated by gender 
library(ggpubr)
ggscatter(bytop5,x = "GENERIC_DRUG_COST", y = "BRAND_DRUG_COST",
          color = "NPPES_PROVIDER_GENDER",
          ellipse=TRUE, mean.point = TRUE,size = 2)

ggscatter(bytop5,x = "GENERIC_DRUG_COST", y = "BRAND_DRUG_COST",
          color = "SPECIALTY_DESCRIPTION",
          ellipse=TRUE, mean.point = TRUE,size = 2)

library(MASS)
observeddata <- bytop5[,.(Provider=length(NPI),Beneficiary = sum(BENE_COUNT)),by=NPPES_PROVIDER_GENDER]
levels(observeddata)
factor(observeddata)


# chi-square test of significance between cost of prescription drugs and other parameters
nullhypothesis <- c(0.5,0.5)
chisq.test(observeddata, nullhypothesis)

# Data manipulation functions ####

# set minimum values for a given column####
setMinValues <- function(x, min){
  x[is.na(x)] <- min
  return(x)
}

setMissingValues <- function(x, val){
  x[x==""] <- val
  return(x)
}

# Problem 2. ####
#average per provider
byprovider <- dt[BENE_COUNT>10,.(beneficieries = mean(BENE_COUNT),count=),by=NPI]
sprintf("%.10f",mean(byprovider[,beneficieries]))

supplydays <- dt[TOTAL_CLAIM_COUNT>10,.(supply = TOTAL_DAY_SUPPLY),by=NPI]
median(supplydays$supply)
dt[is.na(dt$TOTAL_DAY_SUPPLY)==TRUE]
median(dt[,TOTAL_DAY_SUPPLY])

sprintf("%.10f",sd(byspecialty[BRAND_CNT>1000,.(brand_fraction = BRAND_CNT/(BRAND_CNT+GENE_CNT))]$brand_fraction))

bystate <- dt[OPIOID_BENE_COUNT>10 & ANTIBIOTIC_BENE_COUNT>10,
              .(ratio=OPIOID_BENE_COUNT/ANTIBIOTIC_BENE_COUNT),
              by=NPPES_PROVIDER_STATE]
sprintf("%.10f",max(bystate[,ratio])-min(bystate[,ratio]))

claimcnts <- dt[LIS_CLAIM_COUNT>10 & HRM_CLAIM_COUNT_GE65, 
                .(lis = LIS_CLAIM_COUNT/TOTAL_CLAIM_COUNT, ge65=HRM_CLAIM_COUNT_GE65/TOTAL_CLAIM_COUNT)]
sprintf("%.10f",cor(claimcnts$ge65,claimcnts$lis,method = "pearson"))

inflationrate <- dt[BENE_COUNT>10,
                    .(cost = TOTAL_DRUG_COST/TOTAL_DAY_SUPPLY, 
                      inflate = ((TOTAL_DRUG_COST/TOTAL_DAY_SUPPLY)-100)/100)]
sprintf("%.10f",mean(inflationrate$inflate))