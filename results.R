# Demographic data

a <- as.table(rbind(9, 15))
rowa <- c("Female", "Male")
dimnames(a) <- list(sex = rowa,
                    Number = "N")
a
round((prop.table(a, 2)*100), 2)

#############

# Frequency of clinical presentations

b <- as.table(rbind(24, 17, 14, 10, 10, 2, 13))
rowb <- c("Abdominal pain", "Jaundice",
          "Dark Urine", "Clay Stool",
          "Pruritis", "Abdominal Mass",
          "Weight Loss")
dimnames(b) <- list(symptoms = rowb,
                    Number = "N")
b
round((prop.table(b, 2)*100), 2)

###############

# Frequency of Histo-pathological data

c <- as.table(rbind(17, 1, 4, 1, 1))
rowc <- c("DA", "MCA",
          "P", "MCAC",
          "SC")
dimnames(c) <- list(pathology = rowc,
                    Number = "N")
c
round((prop.table(c, 2)*100), 2)

##############

# comparison of mass diameters
# detected by different modality

d <- as.table(cbind(c(0, 3, 4, 9, 4, 3, 1),
                    c(4, 0, 2, 11, 2, 4, 1)))
rowd <- c("Negative", "1-10",
          "11-20", "21-30",
          "31-40", "41-50",
          ">50")
cold <- c("EUS", "CT")
dimnames(d) <- list(size = rowd,
                    Modality = cold)
d
round((prop.table(d, 2)*100), 2)

################

# Comparison of mass chch between CT and Gold standard

# 1- Site: body
body_ct <- as.table(rbind(c(2, 0), c(0, 4),
                           c(1, 0)))
rowcat <- c("Benign", "Malignant", "Negative")
colcat <- c("Benign", "Malignant")
dimnames(body_ct) <- list(ct = rowcat,
                           pathology = colcat)
body_ct
fisher.test(body_ct)
# p = 0.02857
# Percentages
round((prop.table(body_ct, 2)*100),2)

# 1- Site: head
head_ct <- as.table(rbind(c(2, 0), c(0, 12),
                           c(1, 2)))
dimnames(head_ct) <- list(ct = rowcat,
                           pathology = colcat)
head_ct
fisher.test(head_ct)
# p = 0.005882
# Percentages
round((prop.table(head_ct, 2)*100),2)

# 2- Nature: Cystic
Cystic_ct <- as.table(rbind(c(2, 0), c(0, 1),
                            c(0, 0)))
dimnames(Cystic_ct) <- list(ct = rowcat,
                          pathology = colcat)
Cystic_ct
fisher.test(Cystic_ct)
# p = 0.3333
# Percentages
round((prop.table(Cystic_ct, 2)*100),2)

# 2- Nature: Solid
Solid_ct <- as.table(rbind(c(2, 0), c(0, 15),
                            c(2, 2)))
dimnames(Solid_ct) <- list(ct = rowcat,
                            pathology = colcat)
Solid_ct
fisher.test(Solid_ct)
# p = 0.00117
# Percentages
round((prop.table(Solid_ct, 2)*100),2)

#####################

# Comparison of mass chch between EUS and Gold standard

# 1- Site: body
body_eus <- as.table(rbind(c(3, 0), c(0, 4),
                          c(0, 0)))
rowcat <- c("Benign", "Malignant", "Negative")
colcat <- c("Benign", "Malignant")
dimnames(body_eus) <- list(eus = rowcat,
                          pathology = colcat)
body_eus
fisher.test(body_eus)
# p = 0.02857
# Percentages
round((prop.table(body_eus, 2)*100),2)

# 1- Site: head
head_eus <- as.table(rbind(c(3, 0), c(0, 13),
                          c(0, 1)))
dimnames(head_eus) <- list(eus = rowcat,
                          pathology = colcat)
head_eus
fisher.test(head_eus)
# p = 0.001471
# Percentages
round((prop.table(head_eus, 2)*100),2)

# 2- Nature: Cystic
Cystic_eus <- as.table(rbind(c(2, 0), c(0, 1),
                            c(0, 0)))
dimnames(Cystic_eus) <- list(eus = rowcat,
                            pathology = colcat)
Cystic_eus
fisher.test(Cystic_eus)
# p = 0.3333
# Percentages
round((prop.table(Cystic_eus, 2)*100),2)

# 2- Nature: Solid
Solid_eus <- as.table(rbind(c(4, 0), c(0, 16),
                           c(0, 1)))
dimnames(Solid_eus) <- list(eus = rowcat,
                           pathology = colcat)
Solid_eus
fisher.test(Solid_eus)
# p = 0.0001671
# Percentages
round((prop.table(Solid_eus, 2)*100),2)

####################

# Comparison of mass chch between CT and EUS

# 1- Site: body
body <- as.table(rbind(c(2, 0), c(0, 4),
                          c(1, 0)))
dimnames(body) <- list(ct = rowcat,
                          eus = colcat)
body
fisher.test(body)
# p = 0.02857

# 1- Site: head
head <- as.table(rbind(c(2, 0), c(0, 12),
                          c(1, 1)))
dimnames(head) <- list(ct = rowcat,
                          eus = colcat)
head
fisher.test(head)
# p = 0.007143

# 2- Nature: Cystic
Cystic <- as.table(rbind(c(2, 0), c(0, 1),
                            c(0, 0)))
dimnames(Cystic) <- list(ct = rowcat,
                            eus = colcat)
Cystic
fisher.test(Cystic)
# p = 0.3333

# 2- Nature: Solid
Solid <- as.table(rbind(c(2, 0), c(0, 15),
                           c(2, 1)))
dimnames(Solid) <- list(ct = rowcat,
                           eus = colcat)
Solid
fisher.test(Solid)
# p = 0.001032

###############################

# 3- Size
mass_ch3 <- as.table(rbind(c(2, 2), c(0, 0),
                           c(1, 1), c(2, 9),
                           c(0, 2), c(0, 4),
                           c(1, 0)))
rowcat3 <- c("Negative", "1-10", "11-20",
             "21-30", "31-40", "41-50",
             ">50")
dimnames(mass_ch3) <- list(size = rowcat3,
                           diagnosis = colcat)
mass_ch3
fisher.test(mass_ch3)
# p = 0.2167
# Percentages
round((prop.table(mass_ch3, 2)*100),2)

# 4- Density
mass_ch4 <- as.table(rbind(c(1, 0), c(1, 4),
                           c(0, 2), c(2, 10),
                           c(2, 2)))
rowcat4 <- c("Hyperdense", "Hypodense",
             "Isodense", "Heterogenous",
             "Negative")
dimnames(mass_ch4) <- list(Density = rowcat4,
                           diagnosis = colcat)
mass_ch4
fisher.test(mass_ch4)
# p = 0.2593
# Percentages
round((prop.table(mass_ch4, 2)*100),2)


#################
# Comparison of extra pancreatic extension by CT
# in relation to Final Diagnosis (Golden Standard by EUS-FNA Pathology)

# 1- LN
lnct <- as.table(rbind(c(1, 0), c(0, 5),
                       c(1, 3)))
dimnames(lnct) <- list(ct = rowcat,
                           pathology = colcat)
lnct
fisher.test(lnct)
# p = 0.08889
# Percentages
round((prop.table(lnct, 2)*100),2)

# 2- VI
vict <- as.table(rbind(c(0, 0), c(0, 3),
                       c(0, 4)))
dimnames(vict) <- list(ct = rowcat,
                       pathology = colcat)
vict
fisher.test(vict)
# p = 1
# Percentages
round((prop.table(vict, 2)*100),2)

# 3- Liver Mets
livermetsct <- as.table(rbind(c(0, 0), c(0, 1),
                              c(0, 0)))
dimnames(livermetsct) <- list(ct = rowcat,
                              pathology = colcat)
livermetsct
fisher.test(livermetsct)
# p = 1
# Percentages
round((prop.table(livermetsct, 2)*100),2)

# 4- Resectibility
resectct <- as.table(rbind(c(0, 0), c(0, 4),
                           c(0, 5)))
dimnames(resectct) <- list(ct = rowcat,
                           pathology = colcat)
resectct
fisher.test(resectct)
# p = 1
# Percentages
round((prop.table(resectct, 2)*100),2)

##################

# Comparison of extra pancreatic extension by EUS
# in relation to Final Diagnosis (Golden Standard by EUS-FNA Pathology)

# 1- LN
lneus <- as.table(rbind(c(2, 0), c(0, 7),
                       c(0, 1)))
dimnames(lneus) <- list(eus = rowcat,
                       pathology = colcat)
lneus
fisher.test(lneus)
# p = 0.02222
# Percentages
round((prop.table(lneus, 2)*100),2)

# 2- VI
vieus <- as.table(rbind(c(0, 0), c(0, 6),
                       c(0, 1)))
dimnames(vieus) <- list(eus = rowcat,
                       pathology = colcat)
vieus
fisher.test(vieus)
# p = 1
# Percentages
round((prop.table(vieus, 2)*100),2)

# 3- Liver Mets
livermetseus <- as.table(rbind(c(0, 0), c(0, 1),
                              c(0, 0)))
dimnames(livermetseus) <- list(eus = rowcat,
                              pathology = colcat)
livermetseus
fisher.test(livermetseus)
# p = 1
# Percentages
round((prop.table(livermetseus, 2)*100),2)

# 4- Resectibility
resecteus <- as.table(rbind(c(0, 0), c(0, 7),
                           c(0, 2)))
dimnames(resecteus) <- list(eus = rowcat,
                           pathology = colcat)
resecteus
fisher.test(resecteus)
# p = 1
# Percentages
round((prop.table(resecteus, 2)*100),2)

###########################

# Comparison of extra pancreatic extension by CT
# in relation to EUS

# 1- LN
ln <- as.table(rbind(c(1, 0), c(0, 5),
                       c(1, 2)))
dimnames(ln) <- list(ct = rowcat,
                       eus = colcat)
ln
fisher.test(ln)
# p = 0.1667

# 2- VI
vi <- as.table(rbind(c(0, 0), c(0, 3),
                       c(0, 3)))
dimnames(vi) <- list(ct = rowcat,
                       eus = colcat)
vi
fisher.test(vi)
# p = 1

# 3- Liver Mets
livermets <- as.table(rbind(c(0, 0), c(0, 1),
                              c(0, 0)))
dimnames(livermets) <- list(ct = rowcat,
                              eus = colcat)
livermets
fisher.test(livermets)
# p = 1

# 4- Resectibility
resect <- as.table(rbind(c(0, 0), c(0, 4),
                           c(0, 3)))
dimnames(resect) <- list(ct = rowcat,
                           eus = colcat)
resect
fisher.test(resect)
# p = 1


##############################
# Comparison of mass chch between EUS and Gold standard

# 3- Size
mass_ch7 <- as.table(rbind(c(0, 0), c(1, 2),
                           c(1, 3), c(2, 7),
                           c(1, 3), c(0, 3),
                           c(1, 0)))

dimnames(mass_ch7) <- list(size = rowcat3,
                           diagnosis = colcat)
mass_ch7
fisher.test(mass_ch7)
# p = 0.6769
# Percentages
round((prop.table(mass_ch7, 2)*100),2)

# 4- Density
mass_ch8 <- as.table(rbind(c(2, 0), c(1, 4),
                           c(1, 2), c(2, 12),
                           c(0, 0)))
dimnames(mass_ch8) <- list(Density = rowcat4,
                           diagnosis = colcat)
mass_ch8
fisher.test(mass_ch8)
# p = 0.07307
# Percentages
round((prop.table(mass_ch8, 2)*100),2)


################


# Association between site of the mass
# and its nature by FNA by EUS

site_nature1 <- as.table(rbind(c(3, 3), c(4, 14)))
rowsinat1 <- c("Benign", "Malignant")
colsinat1 <- c("Pancreatic Body", "Pancreatic Head")
dimnames(site_nature1) <- list(nature = rowsinat1,
                              site = colsinat1)
site_nature1
fisher.test(site_nature1)
# p = 0.3068

site_nature2 <- as.table(rbind(c(1, 3), c(6, 14)))
rowsinat2 <- c("Inflammatory", "Neoplastic")
dimnames(site_nature2) <- list(pathology = rowsinat2,
                               site = colsinat1)
site_nature2
fisher.test(site_nature2)
# p = 1

################
#Association between site of the mass
#and its pathology by FNA by EUS

site_path <- as.table(rbind(c(3, 14), c(1, 0),
                            c(1, 0), c(1, 3),
                            c(1, 0)))
rowpath <- c("DA", "MCA", "MCAC",
             "P", "SC")
dimnames(site_path) <- list(pathology = rowpath,
                            site = colsinat1)
site_path
fisher.test(site_path)
# p = 0.04888

#############

# Comparison between EUS and CT
# in detection of liver Metastasis

liver_eusct <- as.table(rbind(c(17, 0), c(0, 1))) 
eusct <- c("No", "Yes")
dimnames(liver_eusct) <- list(metct= eusct,
                            meteus= eusct)
liver_eusct
fisher.test(liver_eusct)
# p = 0.05556

library("vcd")

vcd::Kappa(liver_eusct)
# value = 1, p = 0

##############

# Comparison between EUS and CT
# in detection of vascular invasion

vi_eusct <- as.table(rbind(c(12, 3), c(0, 3)))
dimnames(vi_eusct) <- list(vict = eusct,
                           vieus = eusct)
vi_eusct
fisher.test(vi_eusct)
# p = 0.02451

vcd::Kappa(vi_eusct)
# value = 0.5714, p = 0.00511

###############

# Comparison between EUS and CT
# in detection of Lymph node involvement

ln_eusct <- as.table(rbind(c(15, 3), c(0, 6)))
dimnames(ln_eusct) <- list(LNCT = eusct,
                           LNEUS = eusct)
ln_eusct
fisher.test(vi_eusct)
# p = 0.02451

vcd::Kappa(ln_eusct)
# value = 0.7143, p = 1.362e-06

###############

# Accuracy of CT to detect Mass

k <- c("positive", "negative")
ct_acc <- matrix(c(16, 2, 2, 4), ncol = 2, byrow = T)
colnames(ct_acc) <- k
rownames(ct_acc) <- k
ct_acc
fisher.test(ct_acc)
# p = 0.01786

library(bdpv)

BDtest(xmat = ct_acc, pr=0.75,conf.level = 0.90)

ct_acc1 <- as.table(rbind(c(16, 2), c(2, 4)))
colnames(ct_acc1) <- k
rownames(ct_acc1) <- k
ct_acc1

library(caret)
confusionMatrix(ct_acc1)

# Accuracy of EUS FNA to detect Malignant Mass

fna_acc <- matrix(c(17, 4, 1, 2), ncol = 2, byrow = T)
colnames(fna_acc) <- k
rownames(fna_acc) <- k
fna_acc
fisher.test(fna_acc)
# p = 0.1433

BDtest(xmat = fna_acc, pr=0.75,conf.level = 0.90)

fna_acc1 <- as.table(rbind(c(17, 4), c(1, 2)))
colnames(fna_acc1) <- k
rownames(fna_acc1) <- k
fna_acc1

confusionMatrix(fna_acc1)
