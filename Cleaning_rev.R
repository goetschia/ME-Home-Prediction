rm(list = ls())
source("Packages.R")
source("Functions.R")

df1 <- read.spss('C:/Users/aljos/OneDrive/Dokumente/PhD/ME@home/HVS_22_07_20_final_complet_7.sav', to.data.frame = T)
df_atc <- read_excel('C:/Users/aljos/OneDrive/Dokumente/PhD/ME@home/Mehome_ATC_R.xlsx')

#delete unimportant drugs

df_atc <- df_atc[!is.na(df_atc[,7]),]
df_atc <- as.data.frame(df_atc)

#Reformatting
df1 <- df1 %>% mutate(across(c(15:54), as.character))

#Join ATC codes with data
df1 <- df1 %>% pivot_longer(cols = c(Medic_leaving_1:Medic_leaving_40))
df1 <- df1[,-285] #delete names --> medic_leavings
df1 <- full_join(df1, df_atc, by = c("value" = "medic"))
df1 <- subset(df1, 
              select = -c(value, no))
names(df1)[285:290] <- c("ROA", "MedicName", "ATCGroup", "ATCSubGroup", "ATCFull", "Dosage")

# Recode date from SPSS format
startdate <- as.POSIXct("1582-10-14")
df1$Date_in <- seconds(df1$Date_in) + startdate
df1$Date_Out <- as.POSIXct(df1$Date_Out)
df1$Stay_length <- as.numeric(difftime(df1$Date_Out, df1$Date_in, units = "days"))
df1$DateFact <- as.factor(df1$Date_Out)

## Create medication groups ##
#Any pain medication 
filter_string_pain_meds <- function(x){x == "M01" | x == "M02" | x == "N01" | x == "N02"}
df1$Painmeds <- filter_string_pain_meds(substr(df1$ATCFull, start = 1, stop = 3))
df1$Painmeds[df1$Painmeds == T] <- 1
df1$Painmeds[df1$Painmeds != T | is.na(df1$Painmeds)] <- 0

# Any opioid
df1$OpsFact <- substr(df1$ATCFull, start = 1, stop =4) == "N02A"
df1$OpsFact[df1$OpsFact == T] <- 1
df1$OpsFact[df1$OpsFact != 1 | is.na(df1$OpsFact)] <- 0

# Any NSAID
df1$NSARFact <- substr(df1$ATCFull, start = 1, stop =5) == "N02BA" | substr(df1$ATCFull, start = 1, stop =4) == "M01A"
df1$NSARFact[df1$NSARFact == T] <- 1
df1$NSARFact[df1$NSARFact != 1 | is.na(df1$NSARFact)] <- 0

# Calculate MME
opioid_names <- unique(df1$ATCFull[df1$OpsFact >=1])
opioid_conversion <- c(2, 1, 2, 0.1, 120, 50, 0.1, 0.1, 0.5, 0.2, 0.15, 7.5)
df_conversion <- data.frame(opioid_names, opioid_conversion)

mme_oral <- df1[df1$OpsFact ==1 & df1$ROA == "oral",] %>% dplyr::select(ATCFull) %>% unique
mme_oral <- mme_oral[!is.na(mme_oral$ATCFull),]
mme_oral$Conv <- NA
mme_oral$Conv[mme_oral$ATCFull == "N02AA05" | mme_oral$ATCFull == "N02AA55"] <- 1.5
mme_oral$Conv[mme_oral$ATCFull == "N02AX02" | mme_oral$ATCFull == "N02AJ13"] <- 0.1
mme_oral$Conv[mme_oral$ATCFull == "N02AB03"] <- 0.13
mme_oral$Conv[mme_oral$ATCFull == "N02AA01"] <- 1
mme_oral$Conv[mme_oral$ATCFull == "N02AE01"] <-30
mme_oral$Conv[mme_oral$ATCFull == "N02AJ06"] <- 0.15 
mme_oral$Conv[mme_oral$ATCFull == "N02AX06"] <- 0.4
mme_oral$Conv[mme_oral$ATCFull == "N02AA08"] <- 0.25
mme_oral$Conv[mme_oral$ATCFull == "N02AA03"] <- 4

mme_oral$Dosage <- NA
mme_oral$Dosage[mme_oral$ATCFull == "N02AA05" | mme_oral$ATCFull == "N02AA55"] <- 75
mme_oral$Dosage[mme_oral$ATCFull == "N02AX02" | mme_oral$ATCFull == "N02AJ13"] <- 300
mme_oral$Dosage[mme_oral$ATCFull == "N02AB03"] <- 0.6
mme_oral$Dosage[mme_oral$ATCFull == "N02AA01"] <- 100
mme_oral$Dosage[mme_oral$ATCFull == "N02AE01"] <- 120
mme_oral$Dosage[mme_oral$ATCFull == "N02AJ06"] <- 0.15 
mme_oral$Dosage[mme_oral$ATCFull == "N02AX06"] <- 0.4
mme_oral$Dosage[mme_oral$ATCFull == "N02AA08"] <- 150
mme_oral$Dosage[mme_oral$ATCFull == "N02AA03"] <- 20

mme_oral <- mme_oral %>% mutate(mme = Conv*Dosage)
mme_oral$ROA <- rep("oral", length(mme_oral$Dosage))
mme_oral <- mme_oral %>% select(ATCFull, ROA, mme)

mme_trans <-df1[df1$OpsFact ==1 & df1$ROA == "transdermal",] %>% dplyr::select(Dosage, ATCFull) %>% unique
mme_trans <- mme_trans[!is.na(mme_trans$Dosage),]
mme_trans$int <- c(1/3, 1/3, 1/3.5, 1/3, 1/3, 1/3.5, 1/3.5, 1/3)
mme_trans$Conv <- c(7.2,7.2, 12.6,7.2, 7.2, 12.6,12.6,7.2)
mme_trans <- mme_trans %>% mutate(mme = int*Conv*Dosage)
mme_trans$ROA <- rep("transdermal", length(mme_trans$Dosage))
mme_trans <- mme_trans %>% select(ATCFull, Dosage, ROA, mme)


# mme <- rbind(mme_oral, mme_trans)
# mme <- mme %>% dplyr::select(ATCFull, ROA, mme)


df1 <- full_join(df1, df_conversion, by = c("ATCFull" = "opioid_names"))
df1 <- df1 %>% mutate(OpsStrength = opioid_conversion*Dosage)

df1 <- full_join(df1, mme_oral, by = c("ATCFull" = "ATCFull", "ROA" = "ROA"))
df1 <- full_join(df1, mme_trans, by = c("ATCFull" = "ATCFull", "Dosage" = "Dosage" ,"ROA" = "ROA"))
df1 <- df1 %>% mutate(mme = coalesce(mme.x, mme.y)) %>% select(-c(mme.x, mme.y))

df1$OpsStrength[is.na(df1$OpsStrength)] <-0

# Opioid class
df1$OpsClass <- NA
df1$OpsClass[df1$ATCFull == "N02AA05" |df1$ATCFull == "N02AA01" |df1$ATCFull == "N02AA55" |df1$ATCFull == "N02AB03" |
               df1$ATCFull == "N02AE01" |df1$ATCFull == "N02AA03"] <- 2
df1$OpsClass[is.na(df1$OpsClass) & df1$OpsFact >=1] <- 1
df1$OpsClass[is.na(df1$OpsClass)] <- 0

# any tricyclic antidepressive 
df1$TCA <- substr(df1$ATCFull, start = 1, stop =5) == "N06AA" 
df1$TCA[df1$TCA == T] <- 1
df1$TCA[df1$TCA != T |  is.na(df1$TCA)] <- 0

# any SNRI
df1$SNRI <- substr(df1$ATCFull, start = 1, stop =7) == "N06AX16"  | substr(df1$ATCFull, start = 1, stop =7) == "N06AX21"
df1$SNRI[df1$SNRI == T] <- 1
df1$SNRI[df1$SNRI != T |  is.na(df1$SNRI)] <- 0

# carbamazepine
df1$Carba <- substr(df1$ATCFull, start = 1, stop =7) == "N03AF01"  
df1$Carba[df1$Carba == T] <- 1
df1$Carba[df1$Carba != T |  is.na(df1$Carba)] <- 0

# any muscle relaxant
df1$MRel <- substr(df1$ATCFull, start = 1, stop =3) == "M03"  
df1$MRel[df1$MRel == T] <- 1
df1$MRel[df1$MRel != T |  is.na(df1$MRel)] <- 0

# Paracetamol
df1$Paracet <- as.numeric(df1$ATCFull == "N02BE01")

# Metamizol
df1$Metamiz <- as.numeric(df1$ATCFull == "N02BB02")

# Gabapentinoids
df1$Gaba <- as.numeric(substr(df1$ATCFull, start = 1, stop = 5) == "N02BF")

# any benzodiazepine
df1$BenzFact <- substr(df1$ATCFull, start = 1, stop =5) == "N05CD" | substr(df1$ATCFull, start = 1, stop =5) == "N05BA"
df1$BenzFact[df1$BenzFact == T] <- 1
df1$BenzFact[df1$BenzFact != T |  is.na(df1$BenzFact)] <- 0

# any z drug
df1$ZFact <- substr(df1$ATCFull, start = 1, stop =5) == "N05CF"
df1$ZFact[df1$ZFact == T] <- 1
df1$ZFact[df1$ZFact != T |  is.na(df1$ZFact)] <- 0

# any hypnotic
df1$HypFact <- NA
df1$HypFact[df1$ZFact == 1 | df1$BenzFact == 1] <- 1
df1$HypFact[is.na(df1$HypFact) |  is.na(df1$HypFact)] <- 0

# Any medication that targets RAAS
df1$RAASFact <- as.numeric(substr(df1$ATCFull, start = 1, stop =3) == "C09" & substr(df1$ATCFull, start = 1, stop =4) != "C09X")

# Diuretics
df1$DiurFact <- as.numeric(substr(df1$ATCFull, start = 1, stop =5) == "C09BA" | df1$ATCFull == "C09BX01" |
                             df1$ATCFull == "C09Bx03" | substr(df1$ATCFull, start = 1, stop =5) == "C09DA" | df1$ATCFull == "C09Dx01" |
                             df1$ATCFull == "C09DX03" | df1$ATCFull == "C09DX06" | df1$ATCFull == "C09DX07" | df1$ATCFull == "C09DX08" | 
                             substr(df1$ATCFull, start = 1, stop =3) == "C03")

# Corticosteroids
df1$Cortico <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 3) == "H02"
)

# SSRIs
df1$SSRI <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 5) == "N06AB"
)

# Anticoagulants and Antithrombotics

df1$AntiCoa <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 4) == "B01A"
)

# Sex
df1$SexFact <- as.numeric(df1$Sex == "Feminin")

# Cancer
df1$TumFact <- as.numeric(df1$tum == " tumeur cim1")
df1$TumATC <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 4) == "L01A" | 
  (substr(df1$ATCFull, start = 1, stop = 4) == "L01B" & df1$ATCFull != "L01BA01") |
  substr(df1$ATCFull, start = 1, stop = 4) == "L01C" | substr(df1$ATCFull, start = 1, stop = 4) == "L01D" |
  substr(df1$ATCFull, start = 1, stop = 4) == "L01X" | substr(df1$ATCFull, start = 1, stop = 5) == "A04AA" |
  (substr(df1$ATCFull, start = 1, stop = 4) == "L02B" & df1$ATCFull != "L02BG02")
)
df1$TumATC[is.na(df1$TumATC)] <- 0

# Chronic pain
df1$CPFact <- df1$Chronic.pain.053 == "Oui"
df1$CPFact <- as.numeric(df1$CPFact)

# CNCP
# df1$CNCPFact <- as.numeric(df1$TumATC == 0 & df1$TumFact== 0 & df1$CPFact == 1)

## Create diagnosis ##
# Chronic infections
df1$HIV <- as.numeric(df1$ATCFull == "J05AE03" | df1$ATCFull == "J05AE10" | 
                             df1$ATCFull == "J05AF06" | df1$ATCFull == "J05AF13" | df1$ATCFull == "J05AG01" |
                             df1$ATCFull == "J05AG04" | df1$ATCFull == "J05AR02" | df1$ATCFull == "J05AR03" |
                             df1$ATCFull == "J05AR13" | df1$ATCFull == "J05AR17")

df1$TB <- as.numeric(df1$ATCFull == "J04AM05" | df1$ATCFull == "J04AM06")

df1$Noso <- as.numeric(
  (df1$ATCFull == "J04AB02" & substr(df1$ATCFull, start = 1, stop = 5) == "J01MA") | 
  (df1$ATCFull == "J04AB02" & df1$ATCFull == "J01FF01") |
  (df1$ATCFull == "J04AB02" & df1$ATCFull == "J01XX09 ")
)

df1$SkinInf <- as.numeric(df1$ATCFull == "J01XX09")

df1$ChronInf <- as.numeric(df1$HIV == 1 | df1$TB == 1 | df1$Noso == 1 | df1$SkinInf == 1)

# Chronic inflammatory diseases
df1$ChronInflamma <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 4) == "H02A" | df1$ATCFull == "L04AB01" | 
    df1$ATCFull == "L04AB04" | df1$ATCFull == "P01BA02" | df1$ATCFull == "L01BA01 " |
    substr(df1$ATCFull, start = 1, stop = 5) == "A07EC"
)

# Renal diseases
df1$RenalDis <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 5) == "V03AE" |
  df1$ATCFull == "A11CC04"
)

# End-stage renal disease
df1$EndRenDis <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 5) == "B03XA"
)

# Diabetes
df1$diabetes <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 5) == "A10AB" |
  substr(df1$ATCFull, start = 1, stop = 5) == "A10AC" |
    substr(df1$ATCFull, start = 1, stop = 5) == "A10AD" |
    substr(df1$ATCFull, start = 1, stop = 5) == "A10AE" |
    substr(df1$ATCFull, start = 1, stop = 5) == "A10BA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "A10BB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "A10BD" |
    substr(df1$ATCFull, start = 1, stop = 5) == "A10BF" |
    substr(df1$ATCFull, start = 1, stop = 5) == "A10BG" |
    substr(df1$ATCFull, start = 1, stop = 5) == "A10BH" |
    substr(df1$ATCFull, start = 1, stop = 5) == "A10BX" 
)

# Pulmonary diseases
df1$PulDis <- as.numeric(
  df1$ATCFull == "R07AX02" |
  substr(df1$ATCFull, start = 1, stop = 5) == "R03AC" |
    substr(df1$ATCFull, start = 1, stop = 5) == "R03AK" |
    substr(df1$ATCFull, start = 1, stop = 5) == "R03BA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "R03BB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "R03DC" |
    substr(df1$ATCFull, start = 1, stop = 5) == "R03DX" 
    
)

# Liver failure
df1$LiverF <- as.numeric(
  df1$ATCFull == "A06AD11" | df1$ATCFull == "H01CB02" | df1$ATCFull == "H01BA04"
)

# Organ transplant
df1$OrgTrans <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 5) == "L04AA" |
    df1$ATCFull == "L04AD01" | df1$ATCFull == "L04AD02"
)

# Neurological diseases
df1$NeurDis <- as.numeric(
  (df1$ATCFull == "N07XX02" |
  df1$ATCFull == "L04AA23" | df1$ATCFull == "L04AA27" |
  substr(df1$ATCFull, start = 1, stop = 5) == "N04BA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N04BB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N04BD" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N04BX " ) |
    ((substr(df1$ATCFull, start = 1, stop = 5) == "N04BC") != 
       (df1$ATCFull == "N04BC01" | df1$ATCFull == "N04BC06" |
          df1$ATCFull == "N04BC07" | df1$ATCFull == "N04BC08"))
)

# Cardiovascular diseases
df1$VascDis <- as.numeric(
  (df1$ATCFull == "C04AD03" |
  substr(df1$ATCFull, start = 1, stop = 5) == "B01AA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "B01AB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "B01AD" |
    substr(df1$ATCFull, start = 1, stop = 5) == "B01AE" |
    substr(df1$ATCFull, start = 1, stop = 5) == "B01AF" |
    substr(df1$ATCFull, start = 1, stop = 5) == "B01AX" ) |
  ((substr(df1$ATCFull, start = 1, stop = 5) == "B01AC") != 
     (df1$ATCFull == "B01AC06" | df1$ATCFull == "B01AC08" | df1$ATCFull == "B01AC09" |
        df1$ATCFull == "B01AC11" | df1$ATCFull == "B01AC15" | df1$ATCFull == "B01AC19" |
        df1$ATCFull == "B01AC21"))
)

df1$CardDis <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 5) == "C01AA" | 
    substr(df1$ATCFull, start = 1, stop = 5) == "C01BA" | 
    substr(df1$ATCFull, start = 1, stop = 5) == "C01BB" | 
    substr(df1$ATCFull, start = 1, stop = 5) == "C01BC" | 
    substr(df1$ATCFull, start = 1, stop = 5) == "C01BD" | 
    substr(df1$ATCFull, start = 1, stop = 5) == "C01BG" | 
    df1$ATCFull == "C01EB10"
)


df1$Ischemia <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 5) == "C01DA" |
  df1$ATCFull == "C01DX16"
)

df1$Hypertens <- as.numeric(
  df1$ATCFull == "C02AA02" | df1$ATCFull == "C02AB02" |
  substr(df1$ATCFull, start = 1, stop = 5) == "C02AC" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C02BA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C02BB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C02CA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C02CC" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C02DA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C02DB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C02DD" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C02DG" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C03AA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C03AB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C03AX" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C03DA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C03DB" |
    substr(df1$ATCFull, start = 1, stop = 4) == "C02L" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07AA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07AB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07AG" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07BA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07BB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07BG" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07CA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07CB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07CG" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07DA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07DB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07EA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07EB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07FA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C07FB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C08CA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C08CX" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C08DA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C08DB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C08EA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C08EX" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C08GA"
  )

df1$HeartF <- as.numeric(
  df1$ATCFull == "C01CA07" | df1$ATCFull == "C01CE01" | df1$ATCFull == "C01CE02" |
    df1$ATCFull == "C01EB09" | df1$ATCFull == "C09DX04 " |
    substr(df1$ATCFull, start = 1, stop = 5) == "C03CA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C03CB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C03CC" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C09AA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C09BA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C09BB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C09CA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C09DA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "C09DB" 
)

df1$CVDis <- as.numeric(
  df1$VascDis == 1 | df1$CardDis == 1 | df1$Hypertens == 1 | df1$HeartF == 1 | df1$Ischemia == 1
)

# Thyroid disorders
df1$ThyrDis <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 5) == "H03AA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "H03BA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "H03BB" 
)

# Gout
df1$Gout <- as.numeric(
  df1$ATCFull == "M04AC01" |
  substr(df1$ATCFull, start = 1, stop = 5) == "M04AA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "M04AB"
)

# Psychiatric diseases
df1$Depression <- as.numeric(
  (substr(df1$ATCFull, start = 1, stop = 5) == "N06AA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N06AB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N06AF" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N06AG") |
    ((substr(df1$ATCFull, start = 1, stop = 5) == "N06AX") != 
       (df1$ATCFull == "N06AX01" | df1$ATCFull == "N06AX02"))
)

df1$Psychosis <- as.numeric(
  (substr(df1$ATCFull, start = 1, stop = 5) == "N05AA" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N05AB" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N05AC" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N05AD" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N05AE" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N05AF" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N05AG" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N05AH" |
    substr(df1$ATCFull, start = 1, stop = 5) == "N05AL") != 
    (df1$ATCFull == "N05AD08" | df1$ATCFull == "N05AL01")
)

df1$BiPol <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 5) == "N05AN"
)

df1$PsychDis <- as.numeric(
  df1$Depression == 1 | df1$Psychosis == 1 | df1$BiPol == 1
)

# Dementias
df1$Dementia <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 4) == "N06D" | 
    df1$ATCFull == "N06BX13"
)

# Acid peptic ulcer diseases
df1$PepUlc <- as.numeric(
  substr(df1$ATCFull, start = 1, stop = 3) == "A02"
)

## Summarise nursing data ##

# Functional performance
df1 <- df1 %>% mutate(Frail_Moving = case_when(Mobility.moving.001 == "Entiere capacite" ~ 0,
                                               Mobility.moving.001 == "incapacite a se deplacer/alitement" ~ 1,
                                               Mobility.moving.001 == "Fortement restreinte" ~ 0.7,
                                               Mobility.moving.001 == "Peu restreinte" ~ 0.3))


df1 <- df1 %>% mutate(Frail_Position = case_when(Mobility.position.change.002 == "Entiere capacite" ~ 0,
                                                 Mobility.position.change.002 == "Peu restreinte" ~ 0.3,
                                                 Mobility.position.change.002 == "incapacite a changer de position" ~ 1,
                                                 Mobility.position.change.002 == "Fortement restreinte" ~ 0.7))

df1 <- df1 %>% mutate(Frail_Gait = case_when(Altered.gait.004 == "non" ~ 0,
                                             Altered.gait.004 == "oui" ~ 1))

df1 <- df1 %>% mutate(Frail_Exh = case_when(Exhaustion.008 == "force physique et mentale" ~ 0,
                                            Exhaustion.008 == "act. ponctuelles possibles" ~ 0.3,
                                            Exhaustion.008 == "quelques act. autonomes possibles avec recup." ~ 0.7,
                                            Exhaustion.008 == "pas d'activite possible" ~ 1))

df1 <- df1 %>% mutate(Frail_UpCare = case_when(Body.Care.Upper.Body.009 == "Entiere capacite" ~ 0,
                                               Body.Care.Upper.Body.009 == "peu restreinte aide pour installation" ~ 0.3,
                                               Body.Care.Upper.Body.009 == "incapacite" ~ 1,
                                               Body.Care.Upper.Body.009 == "fortement restreinte(aide)" ~ 0.7))

df1 <- df1 %>% mutate(Frail_LowCare = case_when(Body.Care.Lower.Body.010 == "Entiere capacite" ~ 0,
                                                Body.Care.Lower.Body.010 == "peu restreinte aide pour installation" ~ 0.3,
                                                Body.Care.Lower.Body.010 == "incapacite" ~ 1,
                                                Body.Care.Lower.Body.010 == "fortement restreinte(aide)" ~ 0.7))

df1 <- df1 %>% mutate(Frail_DressUp = case_when(Dress.undress.upper.body.013 == "Entiere capacite" ~ 0,
                                                Dress.undress.upper.body.013 == "peu restreinte aide pour prep." ~ 0.3,
                                                Dress.undress.upper.body.013 == "fortement restreinte(aide)" ~0.7,
                                                Dress.undress.upper.body.013 == "incapacite" ~ 1))

df1 <- df1 %>% mutate(Frail_DressLow = case_when(Dress.undress.lower.body.014 == "Entiere capacite" ~ 0,
                                                 Dress.undress.lower.body.014 == "peu restreinte aide pour prep." ~ 0.3,
                                                 Dress.undress.lower.body.014 == "fortement restreinte(aide)" ~ 0.7,
                                                 Dress.undress.lower.body.014 == "incapacite" ~ 1))

df1 <- df1 %>% mutate(Frail_Eat = case_when(Eating.013 == "Entiere capacite" ~ 0,
                                            Eating.013 == "peu restreinte" ~ 0.3,
                                            Eating.013 == "fortement restreinte" ~0.7,
                                            Eating.013 == "incapacite" ~ 1))

df1 <- df1 %>% mutate(Frail_Drink = case_when(Drinking.018 == "Entiere capacite" ~ 0,
                                              Drinking.018 == "peu restreinte" ~ 0.3,
                                              Drinking.018 == "fortement restreinte" ~0.7,
                                              Drinking.018 == "incapacite" ~ 1))

df1 <- df1 %>% mutate(Frail_Micturition = case_when(Micturition.025 == "Entiere capacite" ~ 0,
                                                    Micturition.025 == "peu restreinte" ~ 0.3,
                                                    Micturition.025 == "fortement restreinte" ~0.7,
                                                    Micturition.025 == "incapacite" ~ 1))

df1 <- df1 %>% mutate(Frail_Defaec = case_when(Defecation.027 == "Entiere capacite" ~ 0,
                                               Defecation.027 == "peu restreinte" ~ 0.3,
                                               Defecation.027 == "fortement restreinte" ~0.7,
                                               Defecation.027 == "incapacite" ~ 1))

df1 <- df1 %>% mutate(Frail_Hearing = case_when(Hearing.001 == "Pas de deficience auditive" ~ 0,
                                                 Hearing.001 == "deficience auditive" ~ 0.5,
                                                 Hearing.001 == "surdite" ~1))

df1 <- df1 %>% mutate(Frail_View = case_when(View.002 == "Pas de deficience visuelle" ~ 0,
                                             View.002 == "deficience visuelle" ~ 0.5,
                                             View.002 == "Cecite" ~ 1))

df1 <- df1 %>% mutate(Frail_Verbal = case_when(Verbal.expression.003 == "Entiere capacite" ~ 0,
                                               Verbal.expression.003 == "restreinte" ~ 0.3,
                                               Verbal.expression.003 == "incapacite" ~ 1))

df1 <- df1 %>% mutate(Frail_Falls = case_when(Risk.of.falling.006 == 0 ~ 0,
                                              Risk.of.falling.006 == 1 ~ 0.5,
                                              Risk.of.falling.006 > 1 ~ 1))

# Psychological performance

df1 <- df1 %>% mutate(Psy_Vigil = case_when(Perception.vigilance.033 == "Eveille" ~ 0,
                                              Perception.vigilance.033 == "Somnolent" ~ 0.3,
                                              Perception.vigilance.033 == "Stuporeux" ~0.7,
                                              Perception.vigilance.033 == "Comateux" ~ 1))

df1 <- df1 %>% mutate(Psy_Orient = case_when(Orientation.034 == "Entiere faculte" ~ 0,
                                               Orientation.034 == "Possede 3 facultes" ~ 0.3,
                                               Orientation.034 == "Possede 1a2 faculte" ~ 0.7,
                                               Orientation.034 == "Aucune faculte" ~ 1))

df1 <- df1 %>% mutate(Psy_Learn = case_when(Ability.to.learn.035 == "Entiere capacite" ~ 0,
                                              Ability.to.learn.035 == "peu restreinte" ~ 0.3,
                                              Ability.to.learn.035 == "fortement restreinte" ~ 0.7,
                                              Ability.to.learn.035 == "incapacite" ~ 1))

df1 <- df1 %>% mutate(Psy_Skills = case_when(Skills.of.daily.life.036 == "non restreinte" ~ 0,
                                               Skills.of.daily.life.036 == "peu restreinte" ~ 0.3,
                                               Skills.of.daily.life.036 == "fortement restreinte" ~ 0.7,
                                               Skills.of.daily.life.036 == "Inexistante" ~ 1))

df1$Psy_Attention <- as.numeric(df1$Attention.045 == "Restreinte en permanence")

# Calculate scores for functional and psychological performance and take into account missing data for each individual
df1$funcIndSums <- df1 %>% select(starts_with("Frail_")) %>% rowSums(na.rm = T)
df1$funcIndCounts <- df1 %>% select(starts_with("Frail_")) %>% is.na() %>% rowSums()
df1$funcIndCounts <-16 - df1$funcIndCounts
df1$funcInd <- df1$funcIndSums/df1$funcIndCounts

df1$psycIndSums <- df1 %>% select(starts_with("Psy_")) %>% rowSums(na.rm = T)
df1$psyIndCounts <- df1 %>% select(starts_with("Psy_")) %>% is.na() %>% rowSums()
df1$psyIndCounts <-5 - df1$psyIndCounts
df1$psyInd <- df1$psycIndSums/df1$psyIndCounts

# Convert in long format and select needed variables
df_gr_full <- df1 %>%
  dplyr::select(
    V1, PID, OpsFact, CPFact, TumATC , age, Date_Out, SexFact, NSARFact,
    Stay_length, Medic_leaving_nbr, Risk.of.falling.006, rehosp_num, OpsClass,
    Paracet, Metamiz, Gaba, Destination_rec, mme, TCA, SNRI, Carba, MRel,
    HypFact, RAASFact, DiurFact, ChronInf, ChronInflamma, RenalDis, 
    EndRenDis, diabetes, PulDis, LiverF, OrgTrans, NeurDis, CVDis, 
    ThyrDis, Gout, PsychDis, funcInd, psyInd, AntiCoa, SSRI,
    Cortico, PepUlc, Dementia, Painmeds
                                    ) %>%
  group_by(
    V1, PID, Date_Out, SexFact, age, rehosp_num,  CPFact, Stay_length,
    Medic_leaving_nbr, Risk.of.falling.006, Destination_rec, funcInd, psyInd
    ) %>%
  summarise(
    as.numeric(mean(OpsFact,na.rm = T) >0), as.numeric(mean(NSARFact,na.rm = T) >0),
    as.numeric(mean(Paracet,na.rm = T) >0), as.numeric(mean(Metamiz,na.rm = T) >0),
    as.numeric(mean(Gaba,na.rm = T) >0), as.numeric(mean(TCA,na.rm = T) >0),
    as.numeric(mean(SNRI,na.rm = T) >0), as.numeric(mean(Carba,na.rm = T) >0),
    as.numeric(mean(MRel,na.rm = T) >0), max(OpsClass, na.rm = T),
    sum(mme, na.rm = T), as.numeric(mean(HypFact,na.rm = T) >0),
    as.numeric(mean(RAASFact,na.rm = T) >0), as.numeric(mean(DiurFact,na.rm = T) >0),
    as.numeric(mean(ChronInf,na.rm = T) >0), as.numeric(mean(ChronInflamma,na.rm = T) >0),
    as.numeric(mean(RenalDis,na.rm = T) >0), as.numeric(mean(EndRenDis,na.rm = T) >0),
    as.numeric(mean(diabetes,na.rm = T) >0), as.numeric(mean(PulDis,na.rm = T) >0),
    as.numeric(mean(LiverF,na.rm = T) >0), as.numeric(mean(OrgTrans,na.rm = T) >0),
    as.numeric(mean(NeurDis,na.rm = T) >0), as.numeric(mean(CVDis,na.rm = T) >0),
    as.numeric(mean(ThyrDis,na.rm = T) >0), as.numeric(mean(Gout,na.rm = T) >0),
    as.numeric(mean(PsychDis,na.rm = T) >0), as.numeric(mean(AntiCoa,na.rm = T) >0),
    as.numeric(mean(SSRI,na.rm = T) >0), as.numeric(mean(Cortico,na.rm = T) >0),
    as.numeric(mean(PepUlc,na.rm = T) >0), as.numeric(mean(Dementia,na.rm = T) >0),
    sum(Painmeds, na.rm = T), as.numeric(mean(TumATC, na.rm = T)>0)) 



names(df_gr_full) <- c(
  "V1", "PID","Date","Sex", "age", "Rehosp", "CPFact" ,"Stay_length", "MedTot", "FallRisk",
  "Destination", "funcInd", "psyInd", "OpsFact", "NSARFact", "Paracet", "Metamiz", "Gaba","TCA", "SNRI",
  "Carba", "MRel", "OpsClass","mme", "Hypnotics", "RAAS", "Diur", "Infect", "Inflam",
  "RenalDis", "EndRenDis", "Diabetes", "PulDis", "LiverF", "Transplant", "NeurDis",
  "CVDis", "ThyrDis", "Gout", "PsychDis", "AntiCoa", "SSRI", "Cortico", "PepUlc", "Dementia", "Painmeds", "TumATC"
                       )
# Delete line of NA's that was introduced in formation of df
df_gr_full <- df_gr_full[-which(is.na(df_gr_full$V1)),]

# Code CNCP
df_gr_full$CNCPFact <- as.numeric(df_gr_full$TumATC == 0 & df_gr_full$CPFact == 1)

#Calculating time to rehosp

df_gr_full <- df_gr_full %>% ungroup() %>% group_by(PID) %>% arrange(PID,desc(Date)) %>% 
  mutate(TT_Rehosp = c(NA, diff(Date)),
         TT_Rehosp = -TT_Rehosp) %>% ungroup()

# Censoring of those without event
df_gr_full$Censoring <- as.numeric(!is.na(df_gr_full$TT_Rehosp))

# Adding to time to rehosp, for those that are censored at the end of study period, their observation time
df_gr_full$TT_Rehosp[is.na(df_gr_full$TT_Rehosp)] <- difftime(max(df_gr_full$Date), df_gr_full$Date[is.na(df_gr_full$TT_Rehosp)], units = "days")
  


# Binning of other continuous variables: Age, Rehospitalisations, total drugs taken, stay length
df_gr_full <- df_gr_full %>% mutate(age_bin = cut(age, breaks = c(-Inf, 69,79,89,Inf)))
df_gr_full <- df_gr_full %>% mutate(rehosp_bin = cut(Rehosp, breaks = c(-Inf, 0, 5,  Inf)))
df_gr_full <- df_gr_full %>% mutate(med_bin = cut(MedTot, breaks = c(-Inf, 6, 11, Inf)))
df_gr_full <- df_gr_full %>% mutate(stay_bin = cut(Stay_length, breaks = c(-Inf, 7, Inf))) 
df_gr_full <- df_gr_full %>% mutate(mme_bin = cut(mme, breaks = c(-Inf, 0, 15,50,90, Inf)))

# Round months to integers 
df_gr_full <- df_gr_full %>% arrange(Date) %>% ungroup %>% group_by(Date)
df_gr_full$Date_no <- as.integer(factor(df_gr_full$Date))

# Replace NAs in medication columns with 0
df_gr_full <-  df_gr_full %>% mutate(Paracet = ifelse(is.na(Paracet), 0, Paracet),
                                     Metamiz = ifelse(is.na(Metamiz), 0, Metamiz),
                                     Gaba = ifelse(is.na(Gaba), 0, Gaba))

# coding if any co-analgesic

df_gr_full$Coanal <- as.numeric(df_gr_full$TCA == 1 | df_gr_full$SNRI == 1 | 
                                  df_gr_full$Carba == 1 | df_gr_full$Gaba == 1 |
                                  df_gr_full$MRel == 1)

# coding Variables as factor
df_gr_full <- df_gr_full %>% mutate(Sex = ifelse(Sex == 1, "Female", "Male"),
                                    Sex = factor(Sex, levels = c("Male", "Female")),
                                    OpsClass = case_when(OpsClass == 0 ~ "No Opioids",
                                                         OpsClass == 1 ~ "Weak Opioids",
                                                         OpsClass == 2 ~ "Strong Opioids"))

# Number of comorbidities
df_gr_full$Comorb <- df_gr_full %>%
  ungroup %>%
  select(
    Infect, Inflam, RenalDis, EndRenDis, Diabetes, PulDis, LiverF, 
    Transplant, NeurDis, CVDis, ThyrDis, Gout, PsychDis, PepUlc, Dementia
  ) %>% 
  replace(is.na(.),0) %>%
  rowSums()

# Replace NAs (generated in Comorbidities)
df_gr_full %>%
  ungroup() %>%
  select(
    Infect, Inflam, RenalDis, EndRenDis, Diabetes, PulDis, LiverF, 
    Transplant, NeurDis, CVDis, ThyrDis, Gout, PsychDis
  ) %>%
  replace(is.na(.),0) -> df_gr_full[,28:40]


## Coding inadequate combinations ##

# drug-drug interactions
df_gr_full$OpHyp <- as.numeric(
  df_gr_full$OpsFact == 1 & df_gr_full$Hypnotics == 1)

df_gr_full$OpGab <- as.numeric(
  df_gr_full$OpsFact == 1  & df_gr_full$Gaba == 1
)

df_gr_full$NSARAntiCoa <- as.numeric(
  df_gr_full$NSARFact == 1 & df_gr_full$AntiCoa == 1
)

df_gr_full$OpsMRel <- as.numeric(
  df_gr_full$OpsFact == 1 & df_gr_full$MRel == 1
)

df_gr_full$NSARRAAS <- as.numeric(
  df_gr_full$NSARFact == 1 & df_gr_full$RAAS == 1
)

df_gr_full$NSARDiur <- as.numeric(
  df_gr_full$NSARFact == 1 & df_gr_full$Diur == 1
)

df_gr_full$NSARSSRI <- as.numeric(
  df_gr_full$NSARFact == 1 & df_gr_full$SSRI == 1
)

df_gr_full$NSARCortico <- as.numeric(
  df_gr_full$NSARFact == 1 & df_gr_full$Cortico == 1
)

df_gr_full$whammy <- as.numeric(
  df_gr_full$NSARFact == 1 & df_gr_full$RAAS == 1 & df_gr_full$Diur == 1
)

df_gr_full$OpsTCA <- as.numeric(
  df_gr_full$OpsFact == 1 & df_gr_full$TCA == 1
)

# drug disease interactions
df_gr_full$NSARRenal <- as.numeric(
  (df_gr_full$EndRenDis == 1 | df_gr_full$RenalDis == 1) &
    df_gr_full$NSARFact == 1
)

df_gr_full$NSARCV <- as.numeric(
  df_gr_full$NSARFact == 1 & df_gr_full$CVDis == 1
)

df_gr_full$NSARPep <- as.numeric(
  df_gr_full$NSARFact == 1 & df_gr_full$PepUlc
)


# Kaplan Meyer Curve
df_cncp <- df_gr_full[df_gr_full$CNCPFact == 1,]
surv.obj <- survival::Surv(df_cncp$TT_Rehosp, df_cncp$Censoring)
survfit2(Surv(TT_Rehosp, Censoring) ~ 1, data = df_cncp) %>%
  ggsurvfit()


# Sample size for 30 day readmission
pmsampsize(type = "b", rsquared = 0.1, parameters = 20, prevalence = 0.04)
