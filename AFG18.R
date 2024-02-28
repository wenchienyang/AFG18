library ("haven")
library ("survey")
library ("dplyr")
library ("reshape2")
library ("tidyr")
library("ggplot2")

# AFG 2018-2019
# Wen-Chien Yang 
# Update 2024.02.28

################################################################################
############        < 1 >  AFG18 facility recode dataset           #############
################################################################################
# setwd("C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/data/AFG 2018-19/Facility Recode/AFFC7ASDSR")
data<-read_sas("Facility recode/AFFC7ASDSR/AFFC7AFLSR.SAS7BDAT")

################################################################################
#                   variables used to identify facility                        #  
################################################################################
# Questionnaire	  Recode     Label
# FACIL        	  INV_ID     ID of facility
# REGION        	  V001	   Region(Country-specific) 
# FTYPE         	  V003	   Urban/rural   
# FACIL  	          V004	   Facility identification 
# FACWT             V005	   Sample weight  (Divide by 1000000)
# FACTYPE       	  V007	   Facility type                        
# MGA               V008	   Managing authority(ownership)                   

# create a list of facility ID 
fac.id<-data$INV_ID

################################################################################
#                   variables used to determine levels                  
################################################################################
# Q102(5)        	 V014A 	   Services available: Antenatal(ANC) care 
# Q102(7)	         V015A	   Services available: Normal delivery 
# Q102(16) 	       V015C		 Services available: Cesarean delivery

################################################################################
#               P1 Table 2 service utilization variables                       #
################################################################################
# ACS service utilization variables
# Q102(7)	         V015A	   Services available: Normal delivery 
# Q1604A,Q1604B(8) V554H	   Ever or past 3 months: corticosteroids for preterm labor 
# create new var V554h for skip patterns
data <- data%>% mutate(V554h=case_when(
        V015A==1 ~ V554H,
        V015A==0|is.na(V015A) ~ 0))
################################################################################
#              P1 Table 3 Corticosteroids availability variables               #
################################################################################
# Corticosteroid availability in general (medicines for NCD)
# Q903(5)         V904_04    General med: Betamethasone injection               
# Q903(7)         V906_03    General med: Dexamethasone injection
# create new var V904_04new, V906_03new, steroid to indicate either Betamethasone or Dexamethasone
data <- data%>% mutate(V904_04new=case_when(
        V904_04==0 ~ 0,
        V904_04==2 ~ 2,
        V904_04==3 ~ 3,
        V904_04==4 ~ 4,
        V904_04==5 ~ 5,
        is.na(V904_04) ~0, 
        V015A==0|is.na(V015A) ~ 0))
data <- data%>% mutate(V906_03new=case_when(
        V906_03==0 ~ 0,
        V906_03==2 ~ 2,
        V906_03==3 ~ 3,
        V906_03==4 ~ 4,
        V906_03==5 ~ 5,
        is.na(V906_03) ~0, 
        V015A==0|is.na(V015A) ~ 0))
data <- data%>% mutate(steroid=case_when(
        V904_04new==2 ~ 1,
        V906_03new==2 ~ 1,
        V904_04new!=2 & V906_03new!=2  ~0 ))

steroid.avai<-c("at least 1 betamethasone or dexamethasone valid", "others")
data$steroid<-factor(data$steroid, levels= c(1, 0), steroid.avai)

# Corticosteroid availability in surgical and delivery services 
# Q5596(14)	      SF5596N    Med: Betamethasone injection (country specific module for surgical and delivery services)
# Q5596(15)	      SF5596O    Med: Dexamethasone injection (country specific module for surgical and delivery services)

################################################################################
#          P1 Table 4-1, 4-2, 4-3, 4-4 readiness variables to be used          #
################################################################################
# Ultrasound availability/functioning variables 
# Q880             	V863	   Performs diagnostic X-rays, ultrasound, computerized tomography (Yes, No -> 1, 0)
# Q881A,Q881B(4)    V864C    ultrasound availability
# Q881C(4)          V865C    ultrasound working or not 
# create new var V864c V865c for skip patterns
data<-data%>% mutate(V864c=case_when(
      V863==1 & V864C==0 ~ 0,
      V863==1 & V864C==1 ~ 1,
      V863==1 & V864C==2 ~ 2,
      V863==1 & V864C==3 ~ 3,
      V863==1 & V864C==4 ~ 4,
      V863==0|is.na(V863)~ 0))

data<-data%>% mutate(V865c=case_when(
      V863==1 & V865C==0 ~ 0,
      V863==1 & V865C==1 ~ 1,
      V863==1 & V865C==8 ~ 8,
      V863==1 & is.na(V865C) ~ 0,
      V863==0|is.na(V863)~ 0))

# Maternal infection equipment variables 
# Q102(7)	          V015A	   Services available: Normal delivery 
# Q1622A,Q1622B(9)  V533I    thermometer (for identifying maternal infection)
# create new var V533i for skip patterns  
data<-data%>% mutate(V533i=case_when(
      V015A==1 ~ V533I,
      V015A==0|is.na(V015A) ~ 0))

# Maternal infection diagnostics variables 
# hematology analyzer: availability/functioning 
# Q102(17)	        V034		 Services available: Laboratory diagnostic services(including any rapid dx testing in lab)  (No, Yes, Respondent unavailable -> 0, 1, 7)
# Q801             	V840D		 Anemia testing     
# Q802A,Q802B(1)    V845A	   Test conducted and items available:Hematology analyzer
# Q802C(1)          V846A	   Working order:Hematology analyzer
# create new var V840d V845a for skip patterns
data<-data%>% mutate(V840d=case_when(
      V034==1 & V840D==1 ~ 1,
      V034==1 & V840D==0 ~ 0,
      V034==0|is.na(V034) ~ 0 ))
data<-data%>% mutate(V845a=case_when(
      V840d==1 & V845A==0 ~ 0,
      V840d==1 & V845A==1 ~ 1,
      V840d==1 & V845A==2 ~ 2,
      V840d==1 & V845A==3 ~ 3,
      V840d==1 & V845A==4 ~ 4,
      V840d==0 ~ 0))

# Maternal infection diagnostics variables (RDT) 
# Q102(5)        	  V014A 	 Services available: Antenatal(ANC) care 
# Q1406(1)          V462A    Test as part of ANC: HIV RDT                      
# Q1406(5)          V462E    Test as part of ANC: Syphilis RDT                 
# create new var V462a V462e for skip patterns
data<-data%>% mutate(V462a=case_when(
      V014A==1 ~ V462A,
      V014A==0|is.na(V014A) ~ 0))
data<-data%>% mutate(V462e=case_when(
      V014A==1 ~ V462E,
      V014A==0|is.na(V014A) ~ 0))

# Childbirth care (CEmONC), obstetric equipment variables 
# Q1623(2)          V531XH	 Delivery:Delivery pack                            
# Q1623(3)          V531XL   Delivery:Cord clamp                               
# Q1622A,Q1622B(6)	V533G		 Other equip:Manual vacuum extractor               
# Q1622A,Q1622B(7)  V533H    Other equip:Vacuum aspirator or D&C kit        
# Q1623(9)          V531XJ   Delivery:Forceps large                            
# Q1623(10)         V531XK   Delivery:Forceps medium
# create new var V531xh V531xl V533g V533h V531xj V531xk for skip patterns  
data<-data%>% mutate(V531xh=case_when(
      V015A==1 ~ V531XH,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>% mutate(V531xl=case_when(
      V015A==1 ~ V531XL,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>% mutate(V533g=case_when(
      V015A==1 ~ V533G,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>% mutate(V533h=case_when(
      V015A==1 ~ V533H,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>% mutate(V531xj=case_when(
      V015A==1 ~ V531XJ,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>% mutate(V531xk=case_when(
      V015A==1 ~ V531XK,
      V015A==0|is.na(V015A) ~ 0))

# Childbirth care (CEmONC), medicines and commodities variables 
# Q1625(2)          V534G	   Medications:injectable antibiotic (ceftriaxone)   
# Q1625(5)          V534D	   Medications:injectable diazepam                   
# Q1625(3)          V534C	   Medications:injectable oxytocin   
# create new var V534g V534d V534c for skip patterns 
data<-data%>% mutate(V534g=case_when(
      V015A==1 ~ V534G,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>% mutate(V534d=case_when(
      V015A==1 ~ V534D,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>% mutate(V534c=case_when(
      V015A==1 ~ V534C,
      V015A==0|is.na(V015A) ~ 0))

# Child birth care (CEmONC), staff and guideline variables 
# Q2501,Q2502,Q2503 V558     C-section provider present or on-call 24 hours   
# Q2504,Q2505,Q2506 V559     Anesthetist present or on-call 24 hours          
# Q1605,Q1606       V537E	   National guidelines for BEmONC observed      
# Q1607,Q1608       V537C	   National guidelines for CEmONC observed      
# create new var V558new V559new V537e V537c
data<-data%>% mutate(V558new=case_when(
      V015C==1 ~ V558,
      V015C==0|is.na(V015C) ~ 0))
data<-data%>% mutate(V559new=case_when(
      V015C==1 ~ V559,
      V015C==0|is.na(V015C) ~ 0))
data<-data%>% mutate(V537e=case_when(
      V537E==0 ~0,
      V537E==1 ~1,
      V537E==2 ~2,
      is.na(V537E) ~ 0))
data<-data%>% mutate(V537c=case_when(
      V537C==0 ~0,
      V537C==1 ~1,
      V537C==2 ~2,
      is.na(V537C) ~ 0))

# Preterm newborn care, equipment for resuscitation variables
# (also combined with childbirth care (CEmONC), newborn care equipment variables) 
# Q1622A,Q1622B(5)  V536E	   Newborn:Suction bulb for mucus extraction         
# Q1622A,Q1622B(15) V533D	   Stethoscope         
# Q1622A,Q1622B(8)  V536A	   Newborn:Infant resuscitation bag/mask or tube/mask 
# create new var V536e V533d V536a for skip patterns
data<-data%>% mutate(V536e=case_when(
      V015A==1 ~ V536E,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>% mutate(V533d=case_when(
      V015A==1 ~ V533D,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>% mutate(V536a=case_when(
      V015A==1 ~ V536A,
      V015A==0|is.na(V015A) ~ 0))

# Preterm newborn care, equipment for thermal care variables  
# Q1622A,Q1622B(1)  V536B		 Newborn:Incubator                                 
# Q1622A,Q1622B(2)  V536C	   Newborn:Other source of heat for premature infants
# create new var V536b V536c for skip patterns
data<-data%>% mutate(V536b=case_when(
  V015A==1 ~ V536B,
  V015A==0|is.na(V015A) ~ 0))
data<-data%>% mutate(V536c=case_when(
  V015A==1 ~ V536C,
  V015A==0|is.na(V015A) ~ 0))

# Preterm newborn care, equipment for respiratory care variables 
# Q5588(17)	        SF5588Q	 Equip: Pulse oximeter                            
# Q5255(2)	        SF5255B	 General equip: Oxygen concentrator: functioning 
# Q5255(3)	        SF5255C	 General equip: Oxygen tank with cylinder head: functioning 
# Q5255(1)	        SF5255A	 General equip: Outlets for central oxygen supply: functioning 

# (from section 7, basic supplies client examination/waiting room, used for cross country comparison)
# Q700A,Q700B(16)  	V166N    General OPD area: Pulse oximeter available and functioning                          
# Q700A,Q700B(17)  	V166O    General OPD area: Oxygen concentrators available and functioning                   
# Q700A,Q700B(18)  	V166P    General OPD area: Filled oxygen cylinder available and functioning                  
# Q700A,Q700B(19)  	V166Q    General OPD area: Oxygen distribution system available and functioning              

# Preterm newborn care, commodities for hypoglycemia monitoring variables 
# Q102(17)	        V034		 Services available: Laboratory diagnostic services(including any rapid dx testing in lab) (No, Yes, Respondent unavailable -> 0, 1, 7)
# Q830	            V840F1	 Lab tests: Blood Glucose(see V849) 
# Q831A,Q831B(1)    V849B	   Test conducted & equipment available: Glucometer
# Q831C(1)          V850B		 Glucometer functioning     
# Q831A,Q831B(2)	  V849B1	 Test conducted & equipment available:Glucometer test strips
# Q831C(2)          V850B1	 Glucometer test strips unexpired  
# create new var V840f1, V849b, V850, V849b1, V850b1 for skip patterns 	
data<-data%>% mutate(V840f1=case_when(
      is.na(V840F1) ~ 0, 
      V034==1 ~ V840F1,
      V034==0 ~ 0,
      V034==7 ~ 0))
data<-data%>% mutate(V849b=case_when(
      V840f1==1	~ V849B,
      V840f1==0 ~ 0))
data<-data%>% mutate(V850b=case_when(
      V850B==0	~ 0,
      V850B==1	~ 1,
      V850B==8	~ 8,
      is.na(V850B) ~ 0))
data<-data%>% mutate(V849b1=case_when(
      V840f1==1	~ V849B1,
      V840f1==0 ~ 0))
data<-data%>% mutate(V850b1=case_when(
      V850B1==0	~ 0,
      V850B1==1	~ 1,
      V850B1==8	~ 8,
      is.na(V850B1) ~ 0))

# Preterm newborn care, commodities for infection control variables  
# Q210             	V035	   Store meds(ARVs, vaccines, contraceptives) - See V900s      
# Q912(6)           V946_13	 Other supply: Hand washing soap       
# Q912(4)           V946_09	 Other supply: Latex gloves   
# create new var V946_13new, V946_09new for skip patterns
data<-data%>% mutate(V946_13new=case_when(
      V946_13==0 ~0,
      V946_13==1 ~1,
      V946_13==2 ~2,
      V035==1|V035==0|is.na(V035) ~0))
data<-data%>% mutate(V946_09new=case_when(
      V946_09==0 ~0,
      V946_09==1 ~1,
      V946_09==2 ~2,
      V035==1|V035==0|is.na(V035) ~0))

# Preterm newborn care, guidelines regarding preterm newborn care variables 
# Q1609,Q1610       V537D		 Guidelines for management of preterm labor observed
# Create new var V537d for skip patterns
data<-data%>% mutate(V537d=case_when(
      V537D==0 ~ 0, 
      V537D==1 ~ 1,
      V537D==2 ~ 2,
      is.na(V537D) ~0))
###############################################################################
#           P2 Table 1-4, 1-5 provider competence variables to be used         #
################################################################################
# Provider competence in providing adequate delivery care (CEmONC signal functions)             
# Q1604A,Q1604B(1)  V554D	   Ever or past 3 months:Used parenteral antibiotics
# Q1604A,Q1604B(2)  V554A	   Ever or past 3 months:Used parental oxytocin 
# Q1604A,Q1604B(3)  V554B	   Ever or past 3 months:Used parental anticonvulsant
# Q1604A,Q1604B(4)  V554E    Ever or past 3 months:Assisted vaginal delivery 
# Q1604A,Q1604B(5)  V554C	   Ever or past 3 months:Used manual placental removal
# Q1604A,Q1604B(6)  V554F	   Ever or past 3 months:Removal of retained products
# Q1604A,Q1604B(7)  V554G	   Ever or past 3 months:Neonatal resuscitation   
#                   V554J    Ever or past 3 months:Cesarean section
# Q2702             V554I    Ever or past 3 months:Blood transfusion in delivery area 
# create new var V554d V554a V554b V554e V554c V554f V554g V554j V554i for skip patterns
data<-data%>%
      mutate(V554d=case_when(
      V015A==1 ~ V554D,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>%
      mutate(V554a=case_when(
      V015A==1 ~ V554A,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>%
      mutate(V554b=case_when(
      V015A==1 ~ V554B,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>%
      mutate(V554e=case_when(
      V015A==1 ~ V554E,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>%
      mutate(V554c=case_when(
      V015A==1 ~ V554C,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>%
      mutate(V554f=case_when(
      V015A==1 ~ V554F,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>%
      mutate(V554g=case_when(
      V015A==1 ~ V554G,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>%
      mutate(V554j=case_when(
      V015A==1 ~ V554J,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>%
      mutate(V554i=case_when(
      V015A==1 ~ V554I,
      V015A==0|is.na(V015A) ~ 0))

# Provider competence in providing adequate preterm newborn care
# Q1624(2)          V507I	   Newborn rtn:Dry and wrap newborns to keep warm    
# Q1624(3)          V507J	   Newborn rtn:Initiate breastfeeding in first hour after birth 
# Q1611             V506C	   Routine Kangaroo Mother Care in low-weight babies  
# create new var V507i V507j V506c for skip patterns
data<-data%>%
      mutate(V507i=case_when(
      V015A==1 ~ V507I,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>%
      mutate(V507j=case_when(
      V015A==1 ~ V507J,
      V015A==0|is.na(V015A) ~ 0))
data<-data%>%
      mutate(V506c=case_when(
      V015A==1 ~ V506C,
      V015A==0|is.na(V015A) ~ 0))
################################################################################
#                                 Data management                              #
################################################################################
# create new var level to differentiate levels of facilities
# Q102(5)         	V014A 	 Services available: Antenatal(ANC) care (1, 0-> Yes, No)
# Q102(7)	          V015A	   Services available: Normal delivery     (1, 0-> Yes, No)
# Q102(16) 	        V015C		 Services available: Cesarean delivery   (1, 0-> Yes, No)
# level 1: do ANC, don't do NSD or CS     
# level 2: do NSD, don't do CS       
# level 3: do CS

# line 362-365, still miss some obs. Some OBS do not have a level 
# data$level<-ifelse(data$V015C==1, "level 3", 
#             ifelse(data$V015A==1 & data$V015C!=1, "level 2", 
#             ifelse(data$V014A==1 & data$V015A!=1 &data$V015C!=1, "level 1", "undetermined")))

# create level0 
data$level0<-ifelse(data$V015C==1, "level 3", 
             ifelse(data$V015A==1 & (data$V015C!=1|is.na(data$V015C)), "level 2", 
             ifelse(data$V014A==1 & (data$V015A!=1|is.na(data$V015A)) & (data$V015C!=1|is.na(data$V015C)), "level 1", "undetermined")))

# create level 
data<-data%>%mutate(level=case_when(
      level0== "level 1" ~ "level 1",
      level0== "level 2" ~ "level 2",
      level0== "level 3" ~ "level 3",
      level0== "undetermined" ~ "undetermined",
      is.na(V014A)&is.na(V015A)&is.na(V015C) ~"undetermined"))

# Create weight using V005/1000000
data$weight<-data$V005/1000000     

# Convert region V001, urban variable V003, facility type var V007, facility managing authority V008 to factor and label variable
data$V001<- factor(data$V001, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("Balkh", "Hirat", "Kabul", "Kandahar", "Kunduz", "Nangarhar", "Paktya"))
data$V003 <- factor(data$V003, levels = c(1, 2), labels = c("urban", "rural"))
data$V007 <- factor(data$V007, levels = c(1, 2, 3, 4, 5), labels = c("Regional/ National Hospital",
                                                                     "Provincial Hospital",
                                                                     "Special Hospital",
                                                                     "Private Hospital",
                                                                     "Private Clinic"))
data$V008<- factor (data$V008, levels = c (1, 2, 3), labels = c("Government/public",
                                                                "Private not-for-profit",
                                                                "Private for-profit"))
# Labeling variables
service.ever <- c("Never provided",
                  "Provided in past 3 months", 
                  "Ever provided, not in past 3 months",
                  "Ever provided, DK/missing if past 3 months",
                  "Don't know if ever provided")
med.avai<-c("Never available", 
            "Yes, observed, at least 1 valid",
            "Yes, observed, none valid",
            "Yes, reported available, not seen",
            "Not available today")
acs.avai<-c("Never available",
            "Observed at least 1 valid & stockout in last 3 months",
            "Observed at least 1 valid & not-stockout in last 3 months",
            "Obsered available non valid",
            "Reported available & stockout in last 3 months",
            "Reported available & not-stockout in last 3 months",
            "Not available today")
equi.fun.complex<- c("Not used",
                     "Equipment used and observed, but not working",
                     "Equipment used and observed, and working", 
                     "Equipment used and observed, dont konw if working", 
                     "Equipment used and not observed, but not working", 
                     "Equipment used and not observed, and working", 
                     "Equipment used and not observed, dont know if working",  
                     "Equipment used not available today",       
                     "Equipment used, missing if items available") 
avai <- c("Not available",
          "Yes, observed",
          "Yes, reported")
equi.fun <- c ("Not available",
               "Observed, functioning",
               "Observed, missing functioning",
               "Observed, not/DK if functioning",
               "Reported, functioning", 
               "Reported, missing functioning", 
               "Reported, not/DK if functioning")
rdt.avai <-c ("Never available",
              "Yes, observed, at least 1 valid",
              "Yes, observed, none valid",
              "Yes, reported available, not seen",
              "Not available today",
              "Available elsewhere in facility")
provider.avai <- c ("No", 
                    "Yes, observed schedule", 
                    "Yes, reported", 
                    "24-hour schedule not  maintained")
supp.avai <- c ("Never available",
                "Observed",
                "Reported available, not seen")
yesnoDK <- c("No", "Yes", "Don't know")
yesno <- c("No", "Yes")

# Q1604A,Q1604B(1)  V554D	   Ever or past 3 months:Used parenteral antibiotics
# Q1604A,Q1604B(2)  V554A	   Ever or past 3 months:Used parental oxytocin 
# Q1604A,Q1604B(3)  V554B	   Ever or past 3 months:Used parental anticonvulsant
# Q1604A,Q1604B(4)  V554E    Ever or past 3 months:Assisted vaginal delivery 
# Q1604A,Q1604B(5)  V554C	   Ever or past 3 months:Used manual placental removal
# Q1604A,Q1604B(6)  V554F	   Ever or past 3 months:Removal of retained products
# Q1604A,Q1604B(7)  V554G	   Ever or past 3 months:Neonatal resuscitation   
#                   V554J    Ever or past 3 months:Cesarean section
# Q2702             V554I    Ever or past 3 months:Blood transfusion in delivery area 
#                   V554H    Ever or past 3 months:Corticosteroids for preterm labor (not signal function)

list1<-c("V554a", "V554b", "V554c", "V554d", "V554e", "V554f", "V554g", "V554j", "V554i", "V554h")
for (i in list1 ) {data[[i]] <- factor(data[[i]], levels = c(0, 1, 2, 3, 8), labels = service.ever)}

# 3. corticosteroids availability: beta(V904_04new, SF5596N), dexa(V906_03new , SF5596O)    
list2<-c("V904_04new", "V906_03new")
for (i in list2){data[[i]] <- factor(data[[i]], levels = c(0, 2, 3, 4, 5), labels = med.avai)} 

data$SF5596N <- factor(data$SF5596N, levels = c(0, 1, 2, 3, 4, 5, 6), labels = acs.avai)  
data$SF5596O <- factor(data$SF5596O, levels = c(0, 1, 2, 3, 4, 5, 6), labels = acs.avai) 

# 4. functioning ultrasound : create new var ultrafun using 
#    1) availability (V864c) 
#    2) functionality (V865c)            
# 5. 1 maternal infection diagnostics functioning: create new var hemafun 
#    1) availability (V845a)
#    2) functionality (V846A)
#       availability  0 = "Not used"    
#                     1 = "Equipment used and observed"
#                     2 = "Equipment used reported, not seen"
#                     3 = "Equipment used, not available today"
#                     4 = "Equipment used, missing if items available"
#       functionality 0 = "no"      
#                     1 = "yes"  
#                     8 = "don't know"
data <- data %>% mutate(ultrafun = case_when(
        V864c==0            ~ 0,
        V864c==1 & V865c==0 ~ 1,
        V864c==1 & V865c==1 ~ 2,
        V864c==1 & V865c==8 ~ 3,
        V864c==2 & V865c==0 ~ 4,
        V864c==2 & V865c==1 ~ 5,
        V864c==2 & V865c==8 ~ 6,
        V864c==3            ~ 7,
        V864c==4            ~ 8))

data <- data %>% mutate(hemafun = case_when(
        V845a==0            ~ 0,
        V845a==1 & V846A==0 ~ 1,
        V845a==1 & V846A==1 ~ 2,
        V845a==1 & V846A==8 ~ 3,
        V845a==2 & V846A==0 ~ 4,
        V845a==2 & V846A==1 ~ 5,
        V845a==2 & V846A==8 ~ 6,
        V845a==3            ~ 7,
        V845a==4            ~ 8))

# 5. 2 maternal infection diagnostics functioning: HIV (V462a) and syphilis (V462e) RDT 
list3<-c("V462a", "V462e")
for (i in list3){data[[i]] <- factor(data[[i]], levels = c(0, 2, 3, 4, 5, 6), labels = rdt.avai)}

# 6. Adequate childbirth care (CEmONc) 
#    6.1 CEmONC obstetric related equipment - delivery pack (V531XH)
#                                             cord clamp (V535XL)
#                                             forceps large (V531XJ), forceps medium (V531XK)
list4<-c("V531xh", "V531xl", "V531xj", "V531xk")
for (i in list4){data[[i]] <- factor(data[[i]], levels = c(0, 1, 2), labels = avai)}

#    6.2 injectable Abx (V534g), diazepam (V534d), oxytoxin (V534c) 
list5<-c("V534g",  "V534d", "V534c")
for (i in list5){data[[i]]<-factor(data[[i]], levels = c(0, 2, 3, 4, 5), labels = med.avai)}

#    6.3 equipment: manual vacuum aspirator (V533g), vacuum aspirator or DnC kit (V533h), thermometer (V533i)
list6<-c("V533g", "V533h", "V533i")
for (i in list6) {data[[i]] <- factor(data[[i]], levels = c(0, 1, 2, 3, 4, 5, 6), labels = equi.fun)}

#    6.4 Staff and guideline: C/S worker(V558), anesthetist(V559), BEmONC(V537E), CEmONC(V537C)	 	 
list7<-c("V558new", "V559new")
for (i in list7) {data[[i]] <- factor(data[[i]], levels = c(0, 1, 3, 5), labels = provider.avai)}

list8<-c("V537e", "V537c")
for (i in list8) {data[[i]] <- factor(data[[i]], levels = c(0, 1, 2), labels = avai)}

# 7. Adequate preterm care 
#    7.1 equipment for resuscitation (also CEmONC newborn equipment) - infant resuscitation bag/mask or tube/mask (V536A)
#                                                                      suction bulb (V536E) 
#    7.2 equipment for thermal care - incubator (V536B)
#                                     external heat (V536C)
#                                     stethoscope (V533D)
#    7.3 equipment for respiratory care - V166N Pulse oximeter available and functioning                           	
#                                         V166O Oxygen concentrators available and functioning                   
#                                         V166P Filled oxygen cylinder available and functioning                  
#                                         V166Q Oxygen distribution system available and functioning
#        Country specifc: pulse oximeter(SF5588Q)   data$SF5588Q <- factor(data$SF5588Q, levels = c(0, 1, 2, 3, 4, 5, 6), labels = equi.fun)
#        O2 distribution system(SF5255A)            data$SF5255A <- factor(data$SF5255A, levels = c(0, 1, 2, 3, 4, 5, 6), labels = equi.fun)
#        O2 concentrator(SF5255B)                   data$SF5255B <- factor(data$SF5255B, levels = c(0, 1, 2, 3, 4, 5, 6), labels = equi.fun)
#        O2 tank with cylinder(SF5255C)             data$SF5255C <- factor(data$SF5255C, levels = c(0, 1, 2, 3, 4, 5, 6), labels = equi.fun)
list9<-c("V536a", "V536e", "V533d", "V536b", "V536c", "V166N", "V166O", "V166P", "V166Q")
for (i in list9) {data[[i]] <- factor(data[[i]], levels = c(0, 1, 2, 3, 4, 5, 6), labels = equi.fun)}

#    7.4 supply for infection management - hand washing soap(V946_13new), latex glove(V946_09new)
list10<-c("V946_13new", "V946_09new")
for (i in list10) {data[[i]] <- factor(data[[i]], levels = c(0, 1, 2), labels = supp.avai)}

#    7.5 commodities/supply for hypoglycemia monitoring
#        7.5.1 create new var glucofun using glucometer availability (V849B), working or not glucometer (V850B) 
#        7.5.2 create new var stripfun using strips availability (V849B1), expired or not strips (V850B1) 

data<- data %>% mutate(glucofun = case_when(
                V849b==0            ~ 0,
                V849b==1 & V850b==0 ~ 1,
                V849b==1 & V850b==1 ~ 2,
                V849b==1 & V850b==8 ~ 3,
                V849b==2 & V850b==0 ~ 4,
                V849b==2 & V850b==1 ~ 5,
                V849b==2 & V850b==8 ~ 6,
                V849b==3            ~ 7,
                V849b==4            ~ 8)) 
data <-data %>% mutate(stripfun = case_when(
                V849b1==0             ~ 0,
                V849b1==1 & V850b1==0 ~ 1,
                V849b1==1 & V850b1==1 ~ 2,
                V849b1==1 & V850b1==8 ~ 3,
                V849b1==2 & V850b1==0 ~ 4,
                V849b1==2 & V850b1==1 ~ 5,
                V849b1==2 & V850b1==8 ~ 6,
                V849b1==3             ~ 7,
                V849b1==4             ~ 8))
list11<-c("ultrafun", "hemafun", "glucofun", "stripfun")
for (i in list11){data[[i]] <- factor(data[[i]], levels =c(0, 1, 2, 3, 4, 5, 6, 7, 8), labels = equi.fun.complex)}

#    7.5 guidelines  - guidelines for management of preterm labor (V537D)
data$V537d <- factor (data$V537d, levels = c(0, 1, 2), labels = avai)

# 8. Provider competence about preterm newborn care:
#       routinely drying and wrapping newborns to keep them warm (V507I)
#       routinely initiate BF with an hour (V507J)
#       routine KMC (V506C)                                    
list12<-c("V507i", "V507j")
for (i in list12) {data[[i]] <- factor(data[[i]],levels = c(0, 1, 8), labels = yesnoDK)}
data$V506c <- factor (data$V506c, levels = c(0, 1), labels = yesno)

################################################################################
#              P1 Data analysis : Table 1 Survey characteristics               #
################################################################################
# N of facilities sampled and used
length (data$INV_ID)                 # N of facilities sampled

# create subset with facility weight > 0 
d0<-subset(data, weight > 0)       
nrow (d0)                            # N of facilities used (exclude observation with 0 weights)

# create survey design object desv0
desv0 <- svydesign (id = ~ 1, weights = ~ weight, data = d0)

# create dataset that removed facilities without level
d<-subset(d0, level=="level 1"|level=="level 2"|level=="level 3")

# create subset datasets for three levels of facilities 
d1<-subset (d, level=="level 1")
d2<-subset (d, level=="level 2")
d3<-subset (d, level=="level 3")

# Create survey design object
des <- svydesign (id = ~ 1, weights = ~ weight, data = d)
des1 <- svydesign (id = ~ 1, weights = ~ weight, data = d1)
des2 <- svydesign (id = ~ 1, weights = ~ weight, data = d2)
des3 <- svydesign (id = ~ 1, weights = ~ weight, data = d3)

# Table 1 the proportion of urban versus rural (remove undetermined levels)
# Proportion of urban/rural facilities and weighted %
table (d$V003)                               # use d dataset to cal frequency
prop.table(svytable(~ V003, des)) * 100      # use design object desv0 to cal weighted proportion

# Table 1 the proportion of levels 
# 1. The Proportion of facilities of different levels and weighted % (remove undetermined levels)
table (d$level)                              # use d dataset to cal frequency
prop.table(svytable(~ level, des)) * 100     # use design object desv0 to cal weighted proportion

# 2. The Proportion of facilities of different levels and weighted % (keep undetermined levels)
table (d0$level)                              # use d dataset to cal frequency
prop.table(svytable(~ level, desv0)) * 100    # use design object desv0 to cal weighted proportion

################################################################################
#                P1 Data analysis: Table 2 Service utilization                 #
################################################################################
# The proportion of facilities that have ever given ACS to pregnant women at risk of PL
# overall
table (d$V554h)                                   # use d dataset to cal frequency
prop.table(svytable(~ V554h, des)) * 100          # use design object to cal weighted proportion
# level 1 
table (d1$V554h)                                  # use d1 dataset to cal frequency
prop.table(svytable(~ V554h, des1)) * 100         # use design object des1 to cal weighted proportion
# level 2   
table (d2$V554h)                                  # use d1 dataset to cal frequency
prop.table(svytable(~ V554h, des2)) * 100         # use design object des2 to cal weighted proportion
# level 3
table (d3$V554h)                                  # use d1 dataset to cal frequency
prop.table(svytable(~ V554h, des3)) * 100         # use design object des3 to cal weighted proportion

################################################################################
#                 P1 Data analysis : Table 3 ACS availability                  #
################################################################################
# The proportion of facilities that has beta or dexa
#  Betamethasone availability (V904_04new in meds for NCD section) 
table (d$V904_04new)                              # freq
prop.table(svytable(~ V904_04new, des)) * 100     # weighted prop
# level 1 
table (d1$V904_04new)                             # freq
prop.table(svytable(~ V904_04new, des1)) * 100    # weighted prop
# level 2 
table (d2$V904_04new)                             # freq
prop.table(svytable(~ V904_04new, des2)) * 100    # weighted prop
# level 3 
table (d3$V904_04new)                             # freq
prop.table(svytable(~ V904_04new, des3)) * 100    # weighted prop

#  Dexamethasone availability (V906_03new in meds for NCD section) 
table (d$V906_03new)                            # freq
prop.table(svytable(~ V906_03new, des) ) * 100    # weighted prop
# level 1 
table (d1$V906_03new)                             # freq
prop.table(svytable(~ V906_03new, des1)) * 100    # weighted prop
# level 2 
table (d2$V906_03new)                             # freq
prop.table(svytable(~ V906_03new, des2)) * 100    # weighted prop
# level 3 
table (d3$V906_03new)                             # freq
prop.table(svytable(~ V906_03new, des3)) * 100    # weighted prop

# steroid availability 
table (d$steroid)                              # freq
prop.table(svytable(~ steroid, des) ) * 100    # weighted prop
# level 1 
table (d1$steroid)                             # freq
prop.table(svytable(~ steroid, des1)) * 100    # weighted prop
# level 2 
table (d2$steroid)                             # freq
prop.table(svytable(~ steroid, des2)) * 100    # weighted prop
# level 3 
table (d3$steroid)                             # freq
prop.table(svytable(~ steroid, des3)) * 100    # weighted prop

# # AFG 18 specific
#  Betamethasone availability (SF5596N country-specific module 5: meds for surgical and delivery services)
table (data$SF5596N)                           # freq
prop.table(svytable(~ SF5596N, des))* 100      # weighted prop
#  Dexamethasone availability (SF5596O country-specific module 5: meds for surgical and delivery services)
table (data$SF5596O)                           # frequency
prop.table(svytable(~ SF5596O, des)) * 100     # weighted prop

################################################################################
#                 P1 Data analysis : Table 4-1, GA assessment                  #
################################################################################
# 1. The proportion of facilities that has a functioning ultrasound
table(d$ultrafun)                              # freq
prop.table(svytable(~ultrafun, des)) * 100     # weighted prop
# level 1 
table (d1$ultrafun)                            # use d1 dataset to cal frequency
prop.table(svytable(~ ultrafun, des1)) * 100   # use design object des1 to cal weighted proportion
# level 2   
table (d2$ultrafun)                            # use d2 dataset to cal frequency
prop.table(svytable(~ ultrafun, des2)) * 100   # use design object des2 to cal weighted proportion
# level 3
table (d3$ultrafun)                            # use d3 dataset to cal frequency
prop.table(svytable(~ ultrafun, des3)) * 100   # use design object des3 to cal weighted proportion

################################################################################
#        P1 Data analysis : Table 4-2, maternal infection diagnostics          #
################################################################################
# 1. The proportion of facilities that has thermometer
table (d$V533i)                                # freq
prop.table(svytable(~V533i, des)) * 100        # weighted prop
# level 1 
table (d1$V533i)                               # freq
prop.table(svytable(~V533i, des1)) * 100       # weighted prop
# level 2 
table (d2$V533i)                               # freq
prop.table(svytable(~ V533i, des2)) * 100      # weighted prop
# level 3 
table (d3$V533i)                               # freq
prop.table(svytable(~ V533i, des3)) * 100      # weighted prop

# 2. The proportion of facilities that has a functioning hematology analyzer 
table(d$hemafun)                              # freq
prop.table(svytable(~hemafun, des)) * 100      # weighted prop
# level 1 
table (d1$hemafun)                             # freq
prop.table(svytable(~ hemafun, des1)) * 100    # weighted prop
# level 2 
table (d2$hemafun)                             # freq
prop.table(svytable(~ hemafun, des2)) * 100    # weighted prop
# level 3 
table (d3$hemafun)                             # freq
prop.table(svytable(~ hemafun, des3)) * 100    # weighted prop

# 3. The proportion of facilities that has HIV RDT
table (d$V462a)                                # freq
prop.table(svytable(~V462a, des)) * 100        # weighted prop
# level 1 
table (d1$V462a)                               # freq
prop.table(svytable(~V462a, des1)) * 100       # weighted prop
# level 2 
table (d2$V462a)                               # freq
prop.table(svytable(~ V462a, des2)) * 100      # weighted prop
# level 3 
table (d3$V462a)                               # freq
prop.table(svytable(~ V462a, des3)) * 100      # weighted prop

# 4. The proportion of facilities that has syphilis RDT
table (d$V462e)                                # freq
prop.table(svytable(~V462e, des)) * 100        # weighted prop
# level 1 
table (d1$V462e)                               # freq
prop.table(svytable(~V462e, des1)) * 100       # weighted prop
# level 2 
table (d2$V462e)                               # freq
prop.table(svytable(~ V462e, des2)) * 100      # weighted prop
# level 3 
table (d3$V462e)                               # freq
prop.table(svytable(~ V462e, des3)) * 100      # weighted prop

################################################################################
#           P1 Data analysis : Table 4-3, adequate childbirth care             #
################################################################################
# 1. The proportion of facilities that has needed equipment
# needed equipment for obstetric care    
#　　1.1 The proportion of facilities that has delivery pack
table (d$V531xh)                               # freq
prop.table(svytable(~V531xh, des)) * 100       # weighted prop 
# level 1 
table (d1$V531xh)                              # freq
prop.table(svytable(~V531xh, des1)) * 100      # weighted prop
# level 2 
table (d2$V531xh)                              # freq
prop.table(svytable(~ V531xh, des2)) * 100     # weighted prop
# level 3 
table (d3$V531xh)                              # freq
prop.table(svytable(~ V531xh, des3)) * 100     # weighted prop

#    1.2 The proportion of facilities that has cord clamp
table (d$V531xl)                               # freq
prop.table(svytable(~V531xl, des)) * 100       # weighted prop
# level 1 
table (d1$V531xl)                              # freq
prop.table(svytable(~V531xl, des1)) * 100      # weighted prop
# level 2 
table (d2$V531xl)                              # freq
prop.table(svytable(~ V531xl, des2)) * 100     # weighted prop
# level 3 
table (d3$V531xl)                              # freq
prop.table(svytable(~ V531xl, des3)) * 100     # weighted prop

#    1.3 The proportion of facilities that has manual vacuum aspirator or extractor
table (d$V533g)                                # freq
prop.table(svytable(~V533g, des)) * 100        # weighted prop
# level 1 
table (d1$V533g)                               # freq
prop.table(svytable(~V533g, des1)) * 100       # weighted prop
# level 2 
table (d2$V533g)                               # freq
prop.table(svytable(~ V533g, des2)) * 100      # weighted prop
# level 3 
table (d3$V533g)                               # freq
prop.table(svytable(~ V533g, des3)) * 100      # weighted prop

#    1.4 The proportion of facilities that has vacuum aspiration kit or D&C kit 
table(d$V533h)                                 # freq
prop.table(svytable(~V533h, des)) * 100        # weighted prop
# level 1 
table (d1$V533h)                               # freq
prop.table(svytable(~V533h, des1)) * 100       # weighted prop
# level 2 
table (d2$V533h)                               # freq
prop.table(svytable(~ V533h, des2)) * 100      # weighted prop
# level 3 
table (d3$V533h)                               # freq
prop.table(svytable(~ V533h, des3)) * 100      # weighted prop

#    1.5 The proportion of facilities that has forceps large (V531XJ)
table(d$V531xj)                                # freq
prop.table(svytable(~V531xj, des)) * 100       # weighted prop
# level 1 
table (d1$V531xj)                              # freq
prop.table(svytable(~V531xj, des1)) * 100      # weighted prop
# level 2 
table (d2$V531xj)                              # freq
prop.table(svytable(~ V531xj, des2)) * 100     # weighted prop
# level 3 
table (d3$V531xj)                              # freq
prop.table(svytable(~ V531xj, des3)) * 100     # weighted prop

#    1.6 The proportion of facilities that has forceps medium (V531XK) 
table(d$V531xk)                                # freq
prop.table(svytable(~V531xk, des)) * 100       # weighted prop
# level 1 
table (d1$V531xk)                              # freq
prop.table(svytable(~V531xk, des1)) * 100      # weighted prop
# level 2 
table (d2$V531xk)                              # freq
prop.table(svytable(~ V531xk, des2)) * 100     # weighted prop
# level 3 
table (d3$V531xk)                              # freq
prop.table(svytable(~ V531xk, des3)) * 100     # weighted prop

# 2. The proportion of facilities that has needed medicines
#    2.1 The proportion of facilities that has needed injectable antibiotic
table(d$V534g)                                 # freq
prop.table(svytable(~V534g, des)) * 100        # weighted prop
# level 1 
table (d1$V534g)                               # freq
prop.table(svytable(~V534g, des1)) * 100       # weighted prop
# level 2 
table (d2$V534g)                               # freq
prop.table(svytable(~ V534g, des2)) * 100      # weighted prop
# level 3 
table (d3$V534g)                               # freq
prop.table(svytable(~ V534g, des3)) * 100      # weighted prop

#    2.2 The proportion of facilities that has needed injectable diazepam  
table(d$V534d)                                 # freq
prop.table(svytable(~V534d, des)) * 100        # weighted prop
# level 1 
table (d1$V534d)                               # freq
prop.table(svytable(~V534d, des1)) * 100       # weighted prop
# level 2 
table (d2$V534d)                               # freq
prop.table(svytable(~ V534d, des2)) * 100      # weighted prop
# level 3 
table (d3$V534d)                               # freq
prop.table(svytable(~ V534d, des3)) * 100      # weighted prop

#    2.3 The proportion of facilities that has needed injectable uterotonic (oxytocin) 
table(d$V534c)                                 # freq
prop.table(svytable(~V534c, des)) * 100        # weighted prop
# level 1 
table (d1$V534c)                               # freq
prop.table(svytable(~V534c, des1)) * 100       # weighted prop
# level 2 
table (d2$V534c)                               # freq
prop.table(svytable(~ V534c, des2)) * 100      # weighted prop
# level 3 
table (d3$V534c)                               # freq
prop.table(svytable(~ V534c, des3)) * 100      # weighted prop

# 3. The proportion of facilities that has needed providers and guidelines 
#    < Needed providers > 
#    3.1 The proportion of facilities that has health workers who can do c/s  
table(d$V558new)                                  # freq
prop.table(svytable(~V558new, des)) * 100         # weighted prop
# level 1 
table (d1$V558new)                                # freq
prop.table(svytable(~ V558new, des1)) * 100       # weighted prop
# level 2 
table (d2$V558new)                                # freq
prop.table(svytable(~ V558new, des2)) * 100       # weighted prop
# level 3 
table (d3$V558new)                                # freq
prop.table(svytable(~ V558new, des3)) * 100       # weighted prop

#    3.2 The proportion of facilities that has anesthetist   
table(d$V559new)                                  # freq
prop.table(svytable(~V559new, des)) * 100         # weighted prop
# level 1 
table (d1$V559new)                                # freq
prop.table(svytable(~ V559new, des1)) * 100       # weighted prop
# level 2 
table (d2$V559new)                                # freq
prop.table(svytable(~ V559new, des2)) * 100       # weighted prop
# level 3 
table (d3$V559new)                                # freq
prop.table(svytable(~ V559new, des3)) * 100       # weighted prop

#    < Needed guidelines> 
#    3.3 The proportion of facilities that has BemONC guidelines 
table(d$V537e)                                 # freq
prop.table(svytable(~V537e, des)) * 100        # weighted prop
    # level 1 
table (d1$V537e)                               # freq
prop.table(svytable(~ V537e, des1)) * 100      # weighted prop
    # level 2 
table (d2$V537e)                               # freq
prop.table(svytable(~ V537e, des2)) * 100      # weighted prop
    # level 3 
table (d3$V537e)                               # freq
prop.table(svytable(~ V537e, des3)) * 100      # weighted prop

#    3.4 The proportion of facilities that has cEmONc guidelines 
table(d$V537c)                                 # freq
prop.table(svytable(~V537c, des)) * 100        # weighted prop
    # level 1 
table (d1$V537c)                               # freq
prop.table(svytable(~ V537c, des1)) * 100      # weighted prop
    # level 2 
table (d2$V537c)                               # freq
prop.table(svytable(~ V537c, des2)) * 100      # weighted prop
    # level 3 
table (d3$V537c)                               # freq
prop.table(svytable(~ V537c, des3)) * 100      # weighted prop

################################################################################
#         P1 Data analysis : Table 4-4, adequate preterm newborn care          #
################################################################################
# 1. The proportion of facilities that has equipment for neonatal resuscitation 
#    1.1 The proportion of facilities has suction bulb or penguin sucker
table(d$V536e)                                 # freq
prop.table(svytable(~V536e, des)) * 100        # weighted prop
# level 1 
table (d1$V536e)                               # freq
prop.table(svytable(~ V536e, des1)) * 100      # weighted prop
# level 2 
table (d2$V536e)                               # freq
prop.table(svytable(~ V536e, des2)) * 100      # weighted prop
# level 3 
table (d3$V536e)                               # freq
prop.table(svytable(~ V536e, des3)) * 100      # weighted prop

#    1.2 The proportion of facilities has stethoscope
table(d$V533d)                                 # freq
prop.table(svytable(~V533d, des)) * 100        # weighted prop     
# level 1 
table (d1$V533d)                               # freq
prop.table(svytable(~ V533d, des1)) * 100      # weighted prop
# level 2 
table (d2$V533d)                               # freq
prop.table(svytable(~ V533d, des2)) * 100      # weighted prop
# level 3 
table (d3$V533d)                               # freq
prop.table(svytable(~ V533d, des3)) * 100      # weighted prop

#    1.3 The proportion of facilities has newborn masks (size 0 and 1) and neonatal size self inflating bag
table(d$V536a)                                 # freq
prop.table(svytable(~V536a, des)) * 100        # weighted prop
# level 1 
table (d1$V536a)                               # freq
prop.table(svytable(~ V536a, des1)) * 100      # weighted prop
# level 2 
table (d2$V536a)                               # freq
prop.table(svytable(~ V536a, des2)) * 100      # weighted prop  
# level 3 
table (d3$V536a)                               # freq
prop.table(svytable(~ V536a, des3)) * 100      # weighted prop

# 2. The proportion of facilities that has equipment for thermal care 
#    2.1 The proportion of facilities has functioning incubator 
table(d$V536b)                                 # freq
prop.table(svytable(~V536b, des)) * 100        # weighted prop
# level 1 
table (d1$V536b)                               # freq
prop.table(svytable(~ V536b, des1)) * 100      # weighted prop
# level 2 
table (d2$V536b)                               # freq
prop.table(svytable(~ V536b, des2)) * 100      # weighted prop  
# level 3 
table (d3$V536b)                               # freq
prop.table(svytable(~ V536b, des3)) * 100      # weighted prop

#    2.2 The proportion of facilities has functioning external heat source 
table(d$V536c)                                 # freq
prop.table(svytable(~V536c, des)) * 100        # weighted prop
# level 1 
table (d1$V536c)                               # freq
prop.table(svytable(~ V536c, des1)) * 100      # weighted prop
# level 2 
table (d2$V536c)                               # freq
prop.table(svytable(~ V536c, des2)) * 100      # weighted prop  
# level 3 
table (d3$V536c)                               # freq
prop.table(svytable(~ V536c, des3)) * 100      # weighted prop

# 3. The proportion of facilities that has equipment for respiratory support (from country specific module 5: Surgical and Delivery Services)
# #    3.1 The proportion of facilities has functioning pulse oximeter 
# table(d$SF5588Q)                               # freq          
# prop.table(svytable(~SF5588Q, des)) * 100      # weighted prop
#     # level 1 
# table (d1$VSF5588Q)                            # freq
# prop.table(svytable(~ SF5588Q, des1)) * 100    # weighted prop
#     # level 2 
# table (d2$SF5588Q)                             # freq
# prop.table(svytable(~ SF5588Q, des2)) * 100    # weighted prop  
#     # level 3 
# table (d3$SF5588Q)                             # freq
# prop.table(svytable(~ SF5588Q, des3)) * 100    # weighted prop
# 
# #    3.2 The proportion of facilities has functioning oxygen concentrator  
# table(d$SF5255B)                               # freq 
# prop.table(svytable(~SF5255B, des)) * 100      # weighted proportion 
#     # level 1 
# table (d1$SF5255B)                             # freq
# prop.table(svytable(~ SF5255B, des1)) * 100    # weighted prop
#     # level 2 
# table (d2$SF5255B)                             # freq
# prop.table(svytable(~ SF5255B, des2)) * 100    # weighted prop  
#     # level 3 
# table (d3$SF5255B)                             # freq
# prop.table(svytable(~ SF5255B, des3)) * 100    # weighted prop
# 
# #    3.3 The proportion of facilities has functioning O2 tank with cylinder 
# table(d$SF5255C)                               # freq
# prop.table(svytable(~SF5255C, des)) * 100      # weighted prop
#     # level 1 
# table (d1$SF5255C)                             # freq
# prop.table(svytable(~ SF5255C, des1)) * 100    # weighted prop
#     # level 2 
# table (d2$SF5255C)                             # freq
# prop.table(svytable(~ SF5255C, des2)) * 100    # weighted prop  
#     # level 3 
# table (d3$SF5255C)                             # freq
# prop.table(svytable(~ SF5255C, des3)) * 100    # weighted prop
# 
# #    3.4 The proportion of facilities has functioning central oxygen distribution system  
# table(d$SF5255A)
# prop.table(svytable(~SF5255A, des)) * 100 
#     # level 1 
# table (d1$SF5255A)                             # freq
# prop.table(svytable(~ SF5255A, des1)) * 100    # weighted prop
#     # level 2 
# table (d2$SF5255A)                             # freq
# prop.table(svytable(~ SF5255A, des2)) * 100    # weighted prop  
#     # level 3 
# table (d3$SF5255A)                             # freq
# prop.table(svytable(~ SF5255A, des3)) * 100    # weighted prop
# #    3.5 The proportion of facilities has functioning oxygen analyzer (could not find)

#    3.1 The proportion of facilities has functioning pulse oximeter 
table(d$V166N)                                 # freq
prop.table(svytable(~V166N, des)) * 100        # weighted prop
# level 1 
table (d1$V166N)                               # freq
prop.table(svytable(~ V166N, des1)) * 100      # weighted prop
# level 2 
table (d2$V166N)                               # freq
prop.table(svytable(~ V166N, des2)) * 100      # weighted prop  
# level 3 
table (d3$V166N)                               # freq
prop.table(svytable(~ V166N, des3)) * 100      # weighted prop

#    3.2 The proportion of facilities has functioning oxygen concentrator  
table(d$V166O)                                 # freq
prop.table(svytable(~V166O, des)) * 100        # weighted prop
# level 1 
table (d1$V166O)                               # freq
prop.table(svytable(~ V166O, des1)) * 100      # weighted prop
# level 2 
table (d2$V166O)                               # freq
prop.table(svytable(~ V166O, des2)) * 100      # weighted prop  
# level 3 
table (d3$V166O)                               # freq
prop.table(svytable(~ V166O, des3)) * 100      # weighted prop

#    3.3 The proportion of facilities has functioning O2 tank with cylinder 
table(d$V166P)
prop.table(svytable(~V166P, des)) * 100        # weighted prop
# level 1 
table (d1$V166P)                               # freq
prop.table(svytable(~ V166P, des1)) * 100      # weighted prop
# level 2 
table (d2$V166P)                               # freq
prop.table(svytable(~ V166P, des2)) * 100      # weighted prop  
# level 3 
table (d3$V166P)                               # freq
prop.table(svytable(~ V166P, des3)) * 100      # weighted prop

#    3.4 The proportion of facilities has functioning central oxygen distribution system  
table(d$V166Q)
prop.table(svytable(~V166Q, des)) * 100        # weighted prop
# level 1 
table (d1$V166Q)                               # freq
prop.table(svytable(~ V166Q, des1)) * 100      # weighted prop
# level 2 
table (d2$V166Q)                               # freq
prop.table(svytable(~ V166Q, des2)) * 100      # weighted prop  
# level 3 
table (d3$V166Q)                               # freq
prop.table(svytable(~ V166Q, des3)) * 100      # weighted prop

# 4. The proportion of facilities that has needed commodities to monitor sugar 
#    4.1 The proportion of facilities that has a functioning glucometer 
table(d$glucofun)                                 # freq 
prop.table(svytable(~glucofun, des)) * 100        # weighted prop
# level 1 
table (d1$glucofun)                               # freq
prop.table(svytable(~ glucofun, des1)) * 100      # weighted prop
# level 2 
table (d2$glucofun)                               # freq
prop.table(svytable(~ glucofun, des2)) * 100      # weighted prop  
# level 3 
table (d3$glucofun)                               # freq
prop.table(svytable(~ glucofun, des3)) * 100      # weighted prop

#    4.2 The proportion of facilities that has unexpired glucometer strips
table(d$stripfun)                                 # freq
prop.table(svytable(~stripfun, des)) * 100        # weighted prop
# level 1 
table (d1$stripfun)                               # freq
prop.table(svytable(~ stripfun, des1)) * 100      # weighted prop
# level 2 
table (d2$stripfun)                               # freq
prop.table(svytable(~ stripfun, des2)) * 100      # weighted prop  
# level 3 
table (d3$stripfun)                               # freq
prop.table(svytable(~ stripfun, des3)) * 100      # weighted prop

# 5. The proportion of facilities that has needed commodities for infection control
#    5.1 The proportion of facilities has hand washing soap
table(d$V946_13new)                                  # freq
prop.table(svytable(~V946_13new, des)) * 100         # weighted prop
# level 1 
table (d1$V946_13new)                                # freq
prop.table(svytable(~ V946_13new, des1)) * 100       # weighted prop
# level 2 
table (d2$V946_13new)                                # freq
prop.table(svytable(~ V946_13new, des2)) * 100       # weighted prop  
# level 3 
table (d3$V946_13new)                                # freq
prop.table(svytable(~ V946_13new, des3)) * 100       # weighted prop

#    5.2 The proportion of facilities has disposable latex gloves 
table(d$V946_09new)                                  # freq
prop.table(svytable(~V946_09new, des)) * 100         # weighted prop
# level 1 
table (d1$V946_09new)                                # freq
prop.table(svytable(~ V946_09new, des1)) * 100       # weighted prop
# level 2 
table (d2$V946_09new)                                # freq
prop.table(svytable(~ V946_09new, des2)) * 100       # weighted prop  
# level 3 
table (d3$V946_09new)                                # freq
prop.table(svytable(~ V946_09new, des3)) * 100       # weighted prop

# 6. Preterm newborn care: guidelines and training 
#    6.1 The proportion of facilities that has guidelines for management of preterm labor
table(d$V537d)                                    # freq
prop.table(svytable(~V537d, des)) * 100           # weighted prop
    # level 1 
table (d1$V537d)                                  # freq
prop.table(svytable(~ V537d, des1)) * 100         # weighted prop
    # level 2 
table (d2$V537d)                                  # freq
prop.table(svytable(~ V537d, des2)) * 100         # weighted prop  
    # level 3 
table (d3$V537d)                                  # freq
prop.table(svytable(~ V537d, des3)) * 100         # weighted prop

#    The proportion of facilities with at least one provider who has received following training about:  
#        Routine care for labor and normal vaginal delivery
#        Integrated Management of Pregnancy and Childbirth (IMPAC) 
#        Comprehensive Emergency Obstetric and Newborn Care (CEmONC)
# (The above 3 would be obtained from health worker interview dataset)  

################################################################################
# P2 Data analysis : Table 1-4, provider competence-providing childbirth care  #
################################################################################
# 1. Ever provided CEmONC signal functions  
#    1.1 The proportion of facilities has ever given parenteral Abx
table(d$V554d)                                    # freq
prop.table(svytable(~V554d, des)) * 100           # weighted prop
# level 1 
table (d1$V554d)                                  # freq
prop.table(svytable(~ V554d, des1)) * 100         # weighted prop
# level 2 
table (d2$V554d)                                  # freq
prop.table(svytable(~ V554d, des2)) * 100         # weighted prop  
# level 3 
table (d3$V554d)                                  # freq
prop.table(svytable(~ V554d, des3)) * 100         # weighted prop

#    1.2 The proportion of facilities has ever given parenteral oxytocin 
table(d$V554a)                                    # freq
prop.table(svytable(~V554a, des)) * 100           # weighted prop
# level 1 
table (d1$V554a)                                  # freq
prop.table(svytable(~ V554a, des1)) * 100         # weighted prop
# level 2 
table (d2$V554a)                                  # freq
prop.table(svytable(~ V554a, des2)) * 100         # weighted prop  
# level 3 
table (d3$V554a)                                  # freq
prop.table(svytable(~ V554a, des3)) * 100         # weighted prop

#    1.3 The proportion of facilities has ever given parenteral diazepem 
table(d$V554b)                                    # freq
prop.table(svytable(~V554b, des)) * 100           # weighted prop
# level 1 
table (d1$V554b)                                  # freq
prop.table(svytable(~ V554b, des1)) * 100         # weighted prop
# level 2 
table (d2$V554b)                                  # freq
prop.table(svytable(~ V554b, des2)) * 100         # weighted prop  
# level 3 
table (d3$V554b)                                  # freq
prop.table(svytable(~ V554b, des3)) * 100         # weighted prop

#    1.4 The proportion of facilities has ever performed assisted delivery 
table(d$V554e)                                    # freq
prop.table(svytable(~V554e, des)) * 100           # weighted prop
# level 1 
table (d1$V554e)                                  # freq
prop.table(svytable(~ V554e, des1)) * 100         # weighted prop
# level 2 
table (d2$V554e)                                  # freq
prop.table(svytable(~ V554e, des2)) * 100         # weighted prop  
# level 3 
table (d3$V554e)                                  # freq
prop.table(svytable(~ V554e, des3)) * 100         # weighted prop

#    1.5 The proportion of facilities has ever performed manual removal of placenta 
table(d$V554c)                                    # freq
prop.table(svytable(~V554c, des)) * 100           # weighted prop
# level 1 
table (d1$V554c)                                  # freq
prop.table(svytable(~ V554c, des1)) * 100         # weighted prop
# level 2 
table (d2$V554c)                                  # freq
prop.table(svytable(~ V554c, des2)) * 100         # weighted prop  
# level 3 
table (d3$V554c)                                  # freq
prop.table(svytable(~ V554c, des3)) * 100         # weighted prop

#    1.6 The proportion of facilities has ever removed retained products
table(d$V554f)                                    # freq
prop.table(svytable(~V554f, des)) * 100           # weighted prop
# level 1 
table (d1$V554f)                                  # freq
prop.table(svytable(~ V554f, des1)) * 100         # weighted prop
# level 2 
table (d2$V554f)                                  # freq
prop.table(svytable(~ V554f, des2)) * 100         # weighted prop  
# level 3 
table (d3$V554f)                                  # freq
prop.table(svytable(~ V554f, des3)) * 100         # weighted prop

#    1.7 The proportion of facilities has ever performed neonatal resuscitation
table(d$V554g)                                    # freq
prop.table(svytable(~V554g, des)) * 100           # weighted prop
# level 1 
table (d1$V554g)                                  # freq
prop.table(svytable(~ V554g, des1)) * 100         # weighted prop
# level 2 
table (d2$V554g)                                  # freq
prop.table(svytable(~ V554g, des2)) * 100         # weighted prop  
# level 3 
table (d3$V554g)                                  # freq
prop.table(svytable(~ V554g, des3)) * 100         # weighted prop

#    1.8 The proportion of facilities has ever performed C/S 
table(d$V554j)                                    # freq
prop.table(svytable(~V554j, des)) * 100           # weighted prop
    # level 1 
table (d1$V554j)                                  # freq
prop.table(svytable(~ V554j, des1)) * 100         # weighted prop
    # level 2 
table (d2$V554j)                                  # freq
prop.table(svytable(~ V554j, des2)) * 100         # weighted prop  
    # level 3 
table (d3$V554j)                                  # freq
prop.table(svytable(~ V554j, des3)) * 100         # weighted prop

#    1.9 The proportion of facilities has ever performed blood transfusion
table(d$V554i)                                    # freq
prop.table(svytable(~V554i, des)) * 100           # weighted prop
# level 1 
table (d1$V554i)                                  # freq
prop.table(svytable(~ V554i, des1)) * 100         # weighted prop
# level 2 
table (d2$V554i)                                  # freq
prop.table(svytable(~ V554i, des2)) * 100         # weighted prop  
# level 3 
table (d3$V554i)                                  # freq
prop.table(svytable(~ V554i, des3)) * 100         # weighted prop

################################################################################
# P2 Data analysis : Table 1-5, provider competence-providing preterm care     #
################################################################################
# 1. The proportion of facilities providing services in the minimum package of care for preterm newborn 
#    1.1 The proportion of facilities that routinely perform drying and wrapping newborns to keep warm 
table(d$V507i)                                    # freq
prop.table(svytable(~V507i, des)) * 100           # weighted prop
# level 1 
table (d1$V507i)                                  # freq
prop.table(svytable(~ V507i, des1)) * 100         # weighted prop
# level 2 
table (d2$V507i)                                  # freq
prop.table(svytable(~ V507i, des2)) * 100         # weighted prop  
# level 3 
table (d3$V507i)                                  # freq
prop.table(svytable(~ V507i, des3)) * 100         # weighted prop

#    1.2 The proportion of facilities that routinely initiate of breastfeeding within the first hour	
table(d$V507j)                                    # freq
prop.table(svytable(~V507j, des)) * 100           # weighted prop
# level 1 
table (d1$V507j)                                  # freq
prop.table(svytable(~ V507j, des1)) * 100         # weighted prop
# level 2 
table (d2$V507j)                                  # freq
prop.table(svytable(~ V507j, des2)) * 100         # weighted prop  
# level 3 
table (d3$V507j)                                  # freq
prop.table(svytable(~ V507j, des3)) * 100         # weighted prop

#    1.3 The proportion of facilities that ever provided KMC for low birth weight infants
table(d$V506c)                                    # freq
prop.table(svytable(~V506c, des)) * 100           # weighted prop
# level 1 
table (d1$V506c)                                  # freq
prop.table(svytable(~ V506c, des1)) * 100         # weighted prop
# level 2 
table (d2$V506c)                                  # freq
prop.table(svytable(~ V506c, des2)) * 100         # weighted prop  
# level 3 
table (d3$V506c)                                  # freq
prop.table(svytable(~ V506c, des3)) * 100         # weighted prop

################################################################################
#                             Generate P1 table 4                              # 
################################################################################
# 1. Define the list of datasets and survey design objects
data_list <- list(d, d1, d2, d3)
design_list <- list(des, des1, des2, des3)

# 2. Initialize empty vectors to store results
beta_result<-character()
dexa_result<-character()
steroid_result<-character()
ultrafun_result<-character()
thermo_result<-character()
hemafun_result<-character()
hiv.rdt_result<-character()
syp.rdt_result<-character()
del.pack_result<-character()
c.clamp_result<-character()
v.extractor_result<-character()
dnc.kit_result<-character()
l.forcep_result<-character()
m.forcep_result<-character()
abx_result<-character()
diazepam_result<-character()
oxytocin_result<-character()
cs.worker_result<-character()
anes.worker_result<-character()
bemonc_result<-character()
cemonc_result<-character()
suction_result<-character()
stetho_result<-character()
bagnmask_result<-character()
incubator_result<-character()
heat_result<-character()
oximeter_result<-character()
concen_result<-character()
cylinder_result<-character()
dis.system_result<-character()
glucofun_result<-character()
stripfun_result<-character()
soap.supply_result<-character()
glove.supply_result<-character()
guideline_result<-character()

# 3. Create for loop through the datasets and survey design objects
for (i in seq_along(data_list)) {
  # Calculate beta values
  beta_freq<- table(data_list[[i]]$V904_04new)[2]
  beta_prop <- round((prop.table(svytable(~ V904_04new, design_list[[i]])) * 100)[2], 1)
  beta <- paste(beta_freq, "(", beta_prop, "%)", sep = "")
  beta_result <- c(beta_result, beta)
  
  # Calculate dexa values
  dexa_freq <- table(data_list[[i]]$V906_03new)[2]
  dexa_prop <- round((prop.table(svytable(~ V906_03new, design_list[[i]])) * 100)[2], 1)
  dexa <- paste(dexa_freq, "(", dexa_prop, "%)", sep = "")
  dexa_result <- c(dexa_result, dexa)
  
  # Calculate steroid values
  steroid_freq <- table(data_list[[i]]$steroid)[1]
  steroid_prop <- round((prop.table(svytable(~ steroid, design_list[[i]])) * 100)[1], 1)
  steroid <- paste(steroid_freq, "(", steroid_prop, "%)", sep = "")
  steroid_result <- c(steroid_result, steroid)
  
  # Calculate ultrafun values
  ultrafun_freq <- table(data_list[[i]]$ultrafun)[3]
  ultrafun_prop <- round((prop.table(svytable(~ ultrafun, design_list[[i]])) * 100)[3], 1)
  ultrafun <- paste(ultrafun_freq, "(", ultrafun_prop, "%)", sep = "")
  ultrafun_result <- c(ultrafun_result, ultrafun)
  
  # Calculate thermometer values
  thermo_freq <- table(data_list[[i]]$V533i)[2]
  thermo_prop <- round((prop.table(svytable(~ V533i, design_list[[i]])) * 100)[2], 1)
  thermo <- paste(thermo_freq, "(", thermo_prop, "%)", sep = "")
  thermo_result <- c(thermo_result, thermo)
  
  # Calculate hemafun values
  hemafun_freq <- table(data_list[[i]]$hemafun)[3]
  hemafun_prop <- round((prop.table(svytable(~ hemafun, design_list[[i]])) * 100)[3], 1)
  hemafun <- paste(hemafun_freq, "(", hemafun_prop, "%)", sep = "")
  hemafun_result <- c(hemafun_result, hemafun)
  
  # Calculate HIV RDT values
  hiv.rdt_freq <- table(data_list[[i]]$V462a)[2]
  hiv.rdt_prop <- round((prop.table(svytable(~ V462a, design_list[[i]])) * 100)[2], 1)
  hiv.rdt <- paste(hiv.rdt_freq, "(", hiv.rdt_prop, "%)", sep = "")
  hiv.rdt_result <- c(hiv.rdt_result, hiv.rdt)
  
  # Calculate syphilis RDT values
  syp.rdt_freq <- table(data_list[[i]]$V462e)[2]
  syp.rdt_prop <- round((prop.table(svytable(~ V462e, design_list[[i]])) * 100)[2], 1)
  syp.rdt <- paste(syp.rdt_freq, "(", syp.rdt_prop, "%)", sep = "")
  syp.rdt_result <- c(syp.rdt_result, syp.rdt)
  
  # Calculate delivery pack values
  V531xh_freq <- table(data_list[[i]]$V531xh)[2]
  V531xh_prop <- round((prop.table(svytable(~ V531xh, design_list[[i]])) * 100)[2], 1)
  V531xh <- paste(V531xh_freq, "(", V531xh_prop, "%)", sep = "")
  del.pack_result <- c(del.pack_result, V531xh)
  
  # Calculate cord clamp values
  V531xl_freq <- table(data_list[[i]]$V531xl)[2]
  V531xl_prop <- round((prop.table(svytable(~ V531xl, design_list[[i]])) * 100)[2], 1)
  V531xl <- paste(V531xl_freq, "(", V531xl_prop, "%)", sep = "")
  c.clamp_result <- c(c.clamp_result, V531xl)
  
  # Calculate manual vacuum extractor values
  V533g_freq <- table(data_list[[i]]$V533g)[2]
  V533g_prop <- round((prop.table(svytable(~ V533g, design_list[[i]])) * 100)[2], 1)
  V533g <- paste(V533g_freq, "(", V533g_prop, "%)", sep = "")
  v.extractor_result <- c(v.extractor_result, V533g)
  
  # Calculate D&C kit values
  V533h_freq <- table(data_list[[i]]$V533h)[2]
  V533h_prop <- round((prop.table(svytable(~ V533h, design_list[[i]])) * 100)[2], 1)
  V533h <- paste(V533h_freq, "(", V533h_prop, "%)", sep = "")
  dnc.kit_result <- c(dnc.kit_result, V533h)
  
  # Calculate forceps large values
  V531xj_freq <- table(data_list[[i]]$V531xj)[2]
  V531xj_prop <- round((prop.table(svytable(~ V531xj, design_list[[i]])) * 100)[2], 1)
  V531xj <- paste(V531xj_freq, "(", V531xj_prop, "%)", sep = "")
  l.forcep_result <- c(l.forcep_result, V531xj)
  
  # Calculate forceps medium values
  V531xk_freq <- table(data_list[[i]]$V531xk)[2]
  V531xk_prop <- round((prop.table(svytable(~ V531xk, design_list[[i]])) * 100)[2], 1)
  V531xk <- paste(V531xk_freq, "(", V531xk_prop, "%)", sep = "")
  m.forcep_result <- c(m.forcep_result, V531xk)
  
  # Calculate abx values
  V534g_freq <- table(data_list[[i]]$V534g)[2]
  V534g_prop <- round((prop.table(svytable(~ V534g, design_list[[i]])) * 100)[2], 1)
  V534g <- paste(V534g_freq, "(", V534g_prop, "%)", sep = "")
  abx_result <- c(abx_result, V534g)
  
  # Calculate diazepam values
  V534d_freq <- table(data_list[[i]]$V534d)[2]
  V534d_prop <- round((prop.table(svytable(~ V534d, design_list[[i]])) * 100)[2], 1)
  V534d <- paste(V534d_freq, "(", V534d_prop, "%)", sep = "")
  diazepam_result <- c(diazepam_result, V534d)
  
  # Calculate oxytocin values
  V534c_freq <- table(data_list[[i]]$V534c)[2]
  V534c_prop <- round((prop.table(svytable(~ V534c, design_list[[i]])) * 100)[2], 1)
  V534c <- paste(V534c_freq, "(", V534c_prop, "%)", sep = "")
  oxytocin_result <- c(oxytocin_result, V534c)

  # Calculate cs workers values
  V558new_freq <- table(data_list[[i]]$V558new)[2]
  V558new_prop <- round((prop.table(svytable(~ V558new, design_list[[i]])) * 100)[2], 1)
  V558new <- paste(V558new_freq, "(", V558new_prop, "%)", sep = "")
  cs.worker_result <- c(cs.worker_result, V558new)

  # Calculate anes workers values
  V559new_freq <- table(data_list[[i]]$V559new)[2]
  V559new_prop <- round((prop.table(svytable(~ V559new, design_list[[i]])) * 100)[2], 1)
  V559new <- paste(V559new_freq, "(", V559new_prop, "%)", sep = "")
  anes.worker_result <- c(anes.worker_result, V559new)

  # Calculate bemonc values
  V537e_freq <- table(data_list[[i]]$V537e)[2]
  V537e_prop <- round((prop.table(svytable(~ V537e, design_list[[i]])) * 100)[2], 1)
  V537e <- paste(V537e_freq, "(", V537e_prop, "%)", sep = "")
  bemonc_result <- c(bemonc_result, V537e)

  # Calculate cemonc workers values
  V537c_freq <- table(data_list[[i]]$V537c)[2]
  V537c_prop <- round((prop.table(svytable(~ V537c, design_list[[i]])) * 100)[2], 1)
  V537c <- paste(V537c_freq, "(", V537c_prop, "%)", sep = "")
  cemonc_result <- c(cemonc_result, V537c)
  
  # Calculate suction values
  V536e_freq <- table(data_list[[i]]$V536e)[2]
  V536e_prop <- round((prop.table(svytable(~ V536e, design_list[[i]])) * 100)[2], 1)
  V536e <- paste(V536e_freq, "(", V536e_prop, "%)", sep = "")
  suction_result <- c(suction_result, V536e)
  
  # Calculate stethoscope values
  V533d_freq <- table(data_list[[i]]$V533d)[2]
  V533d_prop <- round((prop.table(svytable(~ V533d, design_list[[i]])) * 100)[2], 1)
  V533d <- paste(V533d_freq, "(", V533d_prop, "%)", sep = "")
  stetho_result <- c(stetho_result, V533d)
  
  # Calculate bag and mask values
  V536a_freq <- table(data_list[[i]]$V536a)[2]
  V536a_prop <- round((prop.table(svytable(~ V536a, design_list[[i]])) * 100)[2], 1)
  V536a <- paste(V536a_freq, "(", V536a_prop, "%)", sep = "")
  bagnmask_result <- c(bagnmask_result, V536a)

  # Calculate incubator values
  V536b_freq <- table(data_list[[i]]$V536b)[2]
  V536b_prop <- round((prop.table(svytable(~ V536b, design_list[[i]])) * 100)[2], 1)
  V536b <- paste(V536b_freq, "(", V536b_prop, "%)", sep = "")
  incubator_result <- c(incubator_result, V536b)
  
  # Calculate external heat values
  V536c_freq <- table(data_list[[i]]$V536c)[2]
  V536c_prop <- round((prop.table(svytable(~ V536c, design_list[[i]])) * 100)[2], 1)
  V536c <- paste(V536c_freq, "(", V536c_prop, "%)", sep = "")
  heat_result <- c(heat_result, V536c)
  
  # Calculate pulse oximeter values
  V166N_freq <- table(data_list[[i]]$V166N)[2]
  V166N_prop <- round((prop.table(svytable(~ V166N, design_list[[i]])) * 100)[2], 1)
  V166N <- paste(V166N_freq, "(", V166N_prop, "%)", sep = "")
  oximeter_result <- c(oximeter_result, V166N)

  # Calculate O2 concentrator values
  V166O_freq <- table(data_list[[i]]$V166O)[2]
  V166O_prop <- round((prop.table(svytable(~ V166O, design_list[[i]])) * 100)[2], 1)
  V166O <- paste(V166O_freq, "(", V166O_prop, "%)", sep = "")
  concen_result <- c(concen_result, V166O)

  # Calculate cylinder values
  V166P_freq <- table(data_list[[i]]$V166P)[2]
  V166P_prop <- round((prop.table(svytable(~ V166P, design_list[[i]])) * 100)[2], 1)
  V166P <- paste(V166P_freq, "(", V166P_prop, "%)", sep = "")
  cylinder_result <- c(cylinder_result, V166P)

  # Calculate O2 distribution system values
  V166Q_freq <- table(data_list[[i]]$V166Q)[2]
  V166Q_prop <- round((prop.table(svytable(~ V166Q, design_list[[i]])) * 100)[2], 1)
  V166Q <- paste(V166Q_freq, "(", V166Q_prop, "%)", sep = "")
  dis.system_result <- c(dis.system_result, V166Q)

  # Calculate glucometer values
  glucofun_freq <- table(data_list[[i]]$glucofun)[3]
  glucofun_prop <- round((prop.table(svytable(~ glucofun, design_list[[i]])) * 100)[3], 1)
  glucofun <- paste(glucofun_freq, "(", glucofun_prop, "%)", sep = "")
  glucofun_result <- c(glucofun_result, glucofun)

  # Calculate glucometer strip values
  stripfun_freq <- table(data_list[[i]]$stripfun)[3]
  stripfun_prop <- round((prop.table(svytable(~ stripfun, design_list[[i]])) * 100)[3], 1)
  stripfun <- paste(stripfun_freq, "(", stripfun_prop, "%)", sep = "")
  stripfun_result <- c(stripfun_result, stripfun)

  # Calculate soap supply values
  V946_13new_freq <- table(data_list[[i]]$V946_13new)[2]
  V946_13new_prop <- round((prop.table(svytable(~ V946_13new, design_list[[i]])) * 100)[2], 1)
  V946_13new <- paste(V946_13new_freq, "(", V946_13new_prop, "%)", sep = "")
  soap.supply_result <- c(soap.supply_result, V946_13new)

  # Calculate gloves supply values
  V946_09new_freq <- table(data_list[[i]]$V946_09new)[2]
  V946_09new_prop <- round((prop.table(svytable(~ V946_09new, design_list[[i]])) * 100)[2], 1)
  V946_09new <- paste(V946_09new_freq, "(", V946_09new_prop, "%)", sep = "")
  glove.supply_result <- c(glove.supply_result, V946_09new)
  
  # Calculate guidelines values
  V537d_freq <- table(data_list[[i]]$V537d)[2]
  V537d_prop <- round((prop.table(svytable(~ V537d, design_list[[i]])) * 100)[2], 1)
  V537d <- paste(V537d_freq, "(", V537d_prop, "%)", sep = "")
  guideline_result <- c(guideline_result, V537d)
  }

# 4. Combine results using rbind
p1.tb4.result <-rbind(
                beta_result,         dexa_result,       steroid_result,      ultrafun_result,     thermo_result,    
                hemafun_result,      hiv.rdt_result,    syp.rdt_result,      del.pack_result,     c.clamp_result,    
                v.extractor_result,  dnc.kit_result,    l.forcep_result,     m.forcep_result,     abx_result,        
                diazepam_result,     oxytocin_result,   cs.worker_result,    anes.worker_result,  bemonc_result,     
                cemonc_result,       suction_result,    stetho_result,       bagnmask_result,     incubator_result,  
                heat_result,         oximeter_result,   concen_result,       cylinder_result,     dis.system_result, 
                glucofun_result,     stripfun_result,   soap.supply_result,  glove.supply_result, guideline_result)
colnames(p1.tb4.result)<-c("Overall","Level 1", "Level 2", "Level 3")
print(p1.tb4.result)

# 5. Export output table
# 5. 1 Specify the file path 
file_path <- "C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/paper 1 and 2/2. figures and tables/1. paper1-table 4/AFG18-p1.tb4.csv"
# 5. 2 Export the data frame to a CSV file at the specified location
write.csv(p1.tb4.result, file = file_path, row.names = TRUE)
################################################################################
#                             Generate P2 table 1                              # 
################################################################################
# Outcomes include provide acs ever
#                  provide abx ever 
#                  provide oxytocin ever
#                  provide diazepam ever
#                  perform assisted delivery ever 
#                  perfrom removal of placenta ever 
#                  perform removal of remaining products ever 
#                  perform neonatal resuscitation ever 
#                  perform CS 
#                  provide blood transfusion 
#                  routinely drying newborns   
#                  initiate BF within 1 hour 
#                  routine KMC

# 1. Create a list of outcomes to simplify the loop
list <- c("acs", "abx", "oxy", "dia", "assis.del", "remove.pla", "remove.pro", "neo", "cs", "transfu")

# 2. create outcome character list 
# 2. 1 for loop for outcome i in the list 
for(i in list) {
    assign(paste0(i, ".in3_result"), character())
    assign(paste0(i, ".out3_result"), character())
    assign(paste0(i, ".dk_result"), character())
    assign(paste0(i, ".ever_result"), character())}

# 2. 2 create result character list for the outcomes of drying newborns, BF, and KMC
drying_result<-character()
bf_result<-character()
kmc_result<-character()

# 3. create for loop to calculate outcome values for each outcome 
for (i in seq_along(data_list)) {
  # calculate acs.ever values
  acs.in3_freq<- table(data_list[[i]]$V554h)[2]
  acs.in3_prop <- round((prop.table(svytable(~ V554h, design_list[[i]])) * 100)[2], 1)
  
  acs.out3_freq<- table(data_list[[i]]$V554h)[3]
  acs.out3_prop <- round((prop.table(svytable(~ V554h, design_list[[i]])) * 100)[3], 1)
  
  acs.dk_freq<- table(data_list[[i]]$V554h)[4]
  acs.dk_prop <- round((prop.table(svytable(~ V554h, design_list[[i]])) * 100)[4], 1)
  
  acs.ever_freq<-acs.in3_freq+acs.out3_freq+acs.dk_freq
  acs.ever_prop<-acs.in3_prop+acs.out3_prop+acs.dk_prop
  acs.ever <- paste(acs.ever_freq, "(", acs.ever_prop, "%)", sep = "")
  acs.ever_result <- c(acs.ever_result, acs.ever)
  
  # calculate abx.ever values
  abx.in3_freq <- table(data_list[[i]]$V554d)[2]
  abx.in3_prop <- round((prop.table(svytable(~ V554d, design_list[[i]])) * 100)[2], 1)
  
  abx.out3_freq <- table(data_list[[i]]$V554d)[3]
  abx.out3_prop <- round((prop.table(svytable(~ V554d, design_list[[i]])) * 100)[3], 1)
  
  abx.dk_freq <- table(data_list[[i]]$V554d)[4]
  abx.dk_prop <- round((prop.table(svytable(~ V554d, design_list[[i]])) * 100)[4], 1)
  
  abx.ever_freq<-abx.in3_freq+abx.out3_freq+abx.dk_freq
  abx.ever_prop<-abx.in3_prop+abx.out3_prop+abx.dk_prop
  abx.ever <- paste(abx.ever_freq, "(", abx.ever_prop, "%)", sep = "")
  abx.ever_result <- c(abx.ever_result, abx.ever)
  
  # calculate oxytoxin.ever values
  oxy.in3_freq <- table(data_list[[i]]$V554a)[2]
  oxy.in3_prop <- round((prop.table(svytable(~ V554a, design_list[[i]])) * 100)[2], 1)
  
  oxy.out3_freq <- table(data_list[[i]]$V554a)[3]
  oxy.out3_prop <- round((prop.table(svytable(~ V554a, design_list[[i]])) * 100)[3], 1)
  
  oxy.dk_freq <- table(data_list[[i]]$V554a)[4]
  oxy.dk_prop <- round((prop.table(svytable(~ V554a, design_list[[i]])) * 100)[4], 1)
  
  oxy.ever_freq<-oxy.in3_freq+oxy.out3_freq+oxy.dk_freq
  oxy.ever_prop<-oxy.in3_prop+oxy.out3_prop+oxy.dk_prop
  oxy.ever <- paste(oxy.ever_freq, "(", oxy.ever_prop, "%)", sep = "")
  oxy.ever_result <- c(oxy.ever_result, oxy.ever)
  
  # calculate diazepam.ever values
  dia.in3_freq <- table(data_list[[i]]$V554b)[2]
  dia.in3_prop <- round((prop.table(svytable(~ V554b, design_list[[i]])) * 100)[2], 1)
  
  dia.out3_freq <- table(data_list[[i]]$V554b)[3]
  dia.out3_prop <- round((prop.table(svytable(~ V554b, design_list[[i]])) * 100)[3], 1)
  
  dia.dk_freq <- table(data_list[[i]]$V554b)[4]
  dia.dk_prop <- round((prop.table(svytable(~ V554b, design_list[[i]])) * 100)[4], 1)
  
  dia.ever_freq<-dia.in3_freq+dia.out3_freq+dia.dk_freq
  dia.ever_prop<-dia.in3_prop+dia.out3_prop+dia.dk_prop
  dia.ever <- paste(dia.ever_freq, "(", dia.ever_prop, "%)", sep = "")
  dia.ever_result <- c(dia.ever_result, dia.ever)
  
  # calculate assisted delivery ever values
  assis.del.in3_freq <- table(data_list[[i]]$V554e)[2]
  assis.del.in3_prop <- round((prop.table(svytable(~ V554e, design_list[[i]])) * 100)[2], 1)
  
  assis.del.out3_freq <- table(data_list[[i]]$V554e)[3]
  assis.del.out3_prop <- round((prop.table(svytable(~ V554e, design_list[[i]])) * 100)[3], 1)
  
  assis.del.dk_freq <- table(data_list[[i]]$V554e)[4]
  assis.del.dk_prop <- round((prop.table(svytable(~ V554e, design_list[[i]])) * 100)[4], 1)
  
  assis.del.ever_freq<-assis.del.in3_freq+assis.del.out3_freq+assis.del.dk_freq
  assis.del.ever_prop<-assis.del.in3_prop+assis.del.out3_prop+assis.del.dk_prop
  assis.del.ever <- paste(assis.del.ever_freq, "(", assis.del.ever_prop, "%)", sep = "")
  assis.del.ever_result <- c(assis.del.ever_result, assis.del.ever)
  
  # calculate placenta removel ever values
  remove.pla.in3_freq <- table(data_list[[i]]$V554c)[2]
  remove.pla.in3_prop <- round((prop.table(svytable(~ V554c, design_list[[i]])) * 100)[2], 1)
  
  remove.pla.out3_freq <- table(data_list[[i]]$V554c)[3]
  remove.pla.out3_prop <- round((prop.table(svytable(~ V554c, design_list[[i]])) * 100)[3], 1)
  
  remove.pla.dk_freq <- table(data_list[[i]]$V554c)[4]
  remove.pla.dk_prop <- round((prop.table(svytable(~ V554c, design_list[[i]])) * 100)[4], 1)
  
  remove.pla.ever_freq<-remove.pla.in3_freq+remove.pla.out3_freq+remove.pla.dk_freq
  remove.pla.ever_prop<-remove.pla.in3_prop+remove.pla.out3_prop+remove.pla.dk_prop
  remove.pla.ever <- paste(remove.pla.ever_freq, "(", remove.pla.ever_prop, "%)", sep = "")
  remove.pla.ever_result <- c(remove.pla.ever_result, remove.pla.ever)
  
  # calculate remaining product removal ever values
  remove.pro.in3_freq <- table(data_list[[i]]$V554f)[2]
  remove.pro.in3_prop <- round((prop.table(svytable(~ V554f, design_list[[i]])) * 100)[2], 1)
  
  remove.pro.out3_freq <- table(data_list[[i]]$V554f)[3]
  remove.pro.out3_prop <- round((prop.table(svytable(~ V554f, design_list[[i]])) * 100)[3], 1)
  
  remove.pro.dk_freq <- table(data_list[[i]]$V554f)[4]
  remove.pro.dk_prop <- round((prop.table(svytable(~ V554f, design_list[[i]])) * 100)[4], 1)
  
  remove.pro.ever_freq<-remove.pro.in3_freq+remove.pro.out3_freq+remove.pro.dk_freq
  remove.pro.ever_prop<-remove.pro.in3_prop+remove.pro.out3_prop+remove.pro.dk_prop
  remove.pro.ever <- paste(remove.pro.ever_freq, "(", remove.pro.ever_prop, "%)", sep = "")
  remove.pro.ever_result <- c(remove.pro.ever_result, remove.pro.ever)
  
  # calculate remaining product removal ever values
  neo.in3_freq <- table(data_list[[i]]$V554g)[2]
  neo.in3_prop <- round((prop.table(svytable(~ V554g, design_list[[i]])) * 100)[2], 1)
  
  neo.out3_freq <- table(data_list[[i]]$V554g)[3]
  neo.out3_prop <- round((prop.table(svytable(~ V554g, design_list[[i]])) * 100)[3], 1)
  
  neo.dk_freq <- table(data_list[[i]]$V554g)[4]
  neo.dk_prop <- round((prop.table(svytable(~ V554g, design_list[[i]])) * 100)[4], 1)
  
  neo.ever_freq<-neo.in3_freq+neo.out3_freq+neo.dk_freq
  neo.ever_prop<-neo.in3_prop+neo.out3_prop+neo.dk_prop
  neo.ever <- paste(neo.ever_freq, "(", neo.ever_prop, "%)", sep = "")
  neo.ever_result <- c(neo.ever_result, neo.ever)
  
  # calculate C/S ever values
  cs.in3_freq <- table(data_list[[i]]$V554j)[2]
  cs.in3_prop <- round((prop.table(svytable(~ V554j, design_list[[i]])) * 100)[2], 1)
  
  cs.out3_freq <- table(data_list[[i]]$V554j)[3]
  cs.out3_prop <- round((prop.table(svytable(~ V554j, design_list[[i]])) * 100)[3], 1)
  
  cs.dk_freq <- table(data_list[[i]]$V554j)[4]
  cs.dk_prop <- round((prop.table(svytable(~ V554j, design_list[[i]])) * 100)[4], 1)
   
  cs.ever_freq<-cs.in3_freq+cs.out3_freq+cs.dk_freq
  cs.ever_prop<-cs.in3_prop+cs.out3_prop+cs.dk_prop
  cs.ever <- paste(cs.ever_freq, "(", cs.ever_prop, "%)", sep = "")
  cs.ever_result <- c(cs.ever_result, cs.ever)
  
  # calculate blood transfusion ever values
  transfu.in3_freq <- table(data_list[[i]]$V554i)[2]
  transfu.in3_prop <- round((prop.table(svytable(~ V554i, design_list[[i]])) * 100)[2], 1)
  
  transfu.out3_freq <- table(data_list[[i]]$V554i)[3]
  transfu.out3_prop <- round((prop.table(svytable(~ V554i, design_list[[i]])) * 100)[3], 1)
  
  transfu.dk_freq <- table(data_list[[i]]$V554i)[4]
  transfu.dk_prop <- round((prop.table(svytable(~ V554i, design_list[[i]])) * 100)[4], 1)
  
  transfu.ever_freq<-transfu.in3_freq+transfu.out3_freq+transfu.dk_freq
  transfu.ever_prop<-transfu.in3_prop+transfu.out3_prop+transfu.dk_prop
  transfu.ever <- paste(transfu.ever_freq, "(", transfu.ever_prop, "%)", sep = "")
  transfu.ever_result <- c(transfu.ever_result, transfu.ever)
  
  # calculate drying newborns ever values
  drying_freq <- table(data_list[[i]]$V507i)[2]
  drying_prop <- round((prop.table(svytable(~ V507i, design_list[[i]])) * 100)[2], 1)
  drying <- paste(drying_freq, "(", drying_prop, "%)", sep = "")
  drying_result <- c(drying_result, drying)
  
  # calculate breast feeding ever values
  bf_freq <- table(data_list[[i]]$V507j)[2]
  bf_prop <- round((prop.table(svytable(~ V507j, design_list[[i]])) * 100)[2], 1)
  bf <- paste(bf_freq, "(", bf_prop, "%)", sep = "")
  bf_result <- c(bf_result, bf)
  
  # calculate KMC values
  kmc_freq <- table(data_list[[i]]$V506c)[2]
  kmc_prop <- round((prop.table(svytable(~ V506c, design_list[[i]])) * 100)[2], 1)
  kmc <- paste(kmc_freq, "(", kmc_prop, "%)", sep = "")
  kmc_result <- c(kmc_result, kmc)
  }

# 4. Combine results using rbind
p2.tb1.result<- rbind(
                acs.ever_result,       abx.ever_result,        oxy.ever_result,         dia.ever_result,
                assis.del.ever_result, remove.pla.ever_result, remove.pro.ever_result,  neo.ever_result,
                cs.ever_result,        transfu.ever_result,    drying_result,           bf_result,
                kmc_result)
colnames(p2.tb1.result)<-c("Overall","Level 1", "Level 2", "Level 3")
print(p2.tb1.result)

# 5. Export output table
# 5. 1 Specify the file path 
file_path <- "C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/paper 1 and 2/2. figures and tables/2. paper2-table 1/AFG18-p2.tb1.csv"
# 5. 2 Export the data frame to a CSV file at the specified location
write.csv(p2.tb1.result, file = file_path, row.names = TRUE)

################################################################################
#########   < 2 >  AFG 18 health worker interview recode dataset      ##########
################################################################################
setwd("C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/data/AFG 2018-19/Provider recode/AFPV7ASDSR")
pro.data<-read_sas("AFPV7AFLSR.SAS7BDAT")      # 1510 providers   
length(unique(pro.data$INV_ID))                # 1510 providers came from 142 facilities

################################################################################
#                  variables to identify facility and provider                 #
################################################################################
# Questionnaire	  New Recode	 Label
# FTYPE         	W003         Urban/rural                                      
# FACIL          	W004         Facility identification     
# PROVNO         	W004P	       Provider serial number      
# FACTYPE       	W007         Facility type (Country-specific)                 
# PROVWT	        W005	       Provider weight (Divide by 1000000)

################################################################################
#                               Data management                                #
################################################################################
# create pro.wt
pro.data$pro.wt<-pro.data$W005/1000000

# merge provider data with provider data 
merge.data <- merge(data, pro.data, by = "INV_ID")
length(unique(merge.data$INV_ID))               # confirm 1510 providers are from 142 facilities 

# remove observations with 0 weights and create provider dataset pro.d
pro.dv0<-subset(merge.data, pro.wt>0)           # 1038 providers with not zero weights 

# create dataset that removed facilities without level
pro.d<-subset(pro.dv0, level=="level 1"|
                       level=="level 2"|
                       level=="level 3")        # 874 providers coming from facilities with a level 
# the number of providers for each facility 
id <- unique(pro.d$W004)                        # retrieve unique ID for facilities of 874 providers
length(id)                                      # 108 facilities 

# create subset with one unique facility to retrieve facility level and weight 
dd <- pro.d %>% 
      group_by(W004) %>%    # Grouping by the facility ID column
      slice(1)              # Selecting the first observation within each group

# for loop to cal n of providers for each facility 
n.provider <- list ()                           # create a list to store the n of provider for each facility
for (i in 1:length(id)) {
     sub.d <- subset(pro.d, W004 == id[i])
     n.provider[[i]] <- nrow(sub.d)}

fac.name<-list()
for (i in 1:length(id)){ 
     fac.name[[i]]<-paste0("fac", id[i])}       # create a list to store the ID of facility 

# n.pro.df: a table of n of providers for each of the 108 facilities  
n.pro.df<-as.data.frame(n.provider)             # change list to data frame
colnames(n.pro.df)<-fac.name                    # assign col name
rownames(n.pro.df)<-c("n_providers")            # assign row name

# # Check the facility IDs in facility recode dataset and provider recode dataset 
# # 1. Find common elements
# common.fac <- intersect(fac.id, id)
# # 2. Find unique elements in each list (fac.id, id)
# uni.list1 <- setdiff(fac.id, id)
# uni.list2 <- setdiff(id, fac.id)
# # 3. Results
# cat("common.fac: ", common.fac, "\n")
# cat("Unique elements in fac.id: ", uni.list1, "\n")
# cat("Unique elements in id: ", uni.list2, "\n") # The facilities IDs in provider dataset are all in the facilit dataset

################################################################################
#        P1 Data analysis : Table 4-4, adequate preterm newborn care           #
################################################################################
# 1. create new var for skip patterns
# P509     W147	  Ever training related to delivery care (1, 0-> Yes, No)
# P510(1)	 W147I  Delivery in-service:Integrated management of pregnancy and childbirth (IMPAC) 	
# P510(2)  W147J	Delivery in-service:Comprehensive emergency obstetrics (CEmOC) 	
# P510(3)  W147A	Delivery in-service:Care during labor/delivery   	
# For these three variables (0, 1, 2 -> None, Yes/training in past 24 months, Yes/training over 24 months ago)
# create new var W147i W147j W147a for skip patterns 
pro.d <- pro.d %>% mutate(W147i= case_when(
         W147I==0 ~0,
         W147I==1 ~1,
         W147I==2 ~2,
         W147==0|is.na(W147)  ~0))

pro.d <- pro.d %>% mutate(W147j= case_when(
         W147J==0 ~0,
         W147J==1 ~1,
         W147J==2 ~2,
         W147==0|is.na(W147)  ~0))

pro.d <- pro.d %>% mutate(W147a= case_when(
         W147A==0 ~0,
         W147A==1 ~1,
         W147A==2 ~2,
         W147==0|is.na(W147)  ~0))

# 2. Cal the proportion of impac trained providers for each facility
no.impac<-list()  # a list to store the number of providers with no impac training 
in.impac<-list()  # a list to store the number of providers with impac trained in past 24 months 
out.impac<-list() # a list to store the number of providers with impac trained over past 24 months 
impac.pro<-list() # a list to store the proportion of providers with impac trained within 2 years 

for (i in 1:length(id)) {
     impac.d <- subset(pro.d, W004 == id[i])
     no.impac[[i]]   <- nrow(subset(impac.d, W147i==0))
     in.impac[[i]]   <- nrow(subset(impac.d, W147i==1))
     out.impac[[i]]  <- nrow(subset(impac.d, W147i==2))
     impac.pro[[i]]  <-(in.impac[[i]] /(no.impac[[i]]+in.impac[[i]]+out.impac[[i]]))}

# convert to data.frame and add column name  
impac.pro.df<-as.data.frame(impac.pro)  
colnames(impac.pro.df)<-fac.name        

# 3. Cal the proportion of cemon trained providers for each facility
no.cemoc<-list()  # a list to store the number of providers with no cemoc training 
in.cemoc<-list()  # a list to store the number of providers with cemoc trained in past 24 months 
out.cemoc<-list() # a list to store the number of providers with cemoc trained over past 24 months 
cemoc.pro<-list() # a list to store the proportion of providers with cemoc trained within 2 years 

for (i in 1:length(id)) {
  cemoc.d <- subset(pro.d, W004 == id[i])
  no.cemoc[[i]]   <- nrow(subset(cemoc.d, W147j==0))
  in.cemoc[[i]]   <- nrow(subset(cemoc.d, W147j==1))
  out.cemoc[[i]]  <- nrow(subset(cemoc.d, W147j==2))
  cemoc.pro[[i]]  <-(in.cemoc[[i]] /(no.cemoc[[i]]+in.cemoc[[i]]+out.cemoc[[i]]))}

# convert to data.frame and add column name  
cemoc.pro.df<-as.data.frame(cemoc.pro)  
colnames(cemoc.pro.df)<-fac.name        

# 4. Cal the proportion of delivery care trained providers for each facility
no.del<-list()  # a list to store the number of providers with no del training 
in.del<-list()  # a list to store the number of providers with del trained in past 24 months 
out.del<-list() # a list to store the number of providers with del trained over past 24 months 
del.pro<-list() # a list to store the proportion of providers with del trained within 2 years 

for (i in 1:length(id)) {
  del.d <- subset(pro.d, W004 == id[i])
  no.del[[i]]   <- nrow(subset(del.d, W147a==0))
  in.del[[i]]   <- nrow(subset(del.d, W147a==1))
  out.del[[i]]  <- nrow(subset(del.d, W147a==2))
  del.pro[[i]]  <-(in.del[[i]] /(no.del[[i]]+in.del[[i]]+out.del[[i]]))}

# convert to data.frame and add column name  
del.pro.df<-as.data.frame(del.pro)  
colnames(del.pro.df)<-fac.name        

# 5. overall new dataset 
newd<-as.data.frame(t(rbind(impac.pro.df, cemoc.pro.df, del.pro.df)))  # combine datasets and transpose dataset  
colnames(newd)<-c("prop.impac_trained", "prop.cemoc_trained", "prop.delivery_trained")
# 5.1 create impac1, cemoc1, and del1 to indicate at least one provider who received impac, cemoc, delivery care training in the past 24 months
newd$impac1<-ifelse(newd$prop.impac_trained > 0, 1, 0) 
newd$cemoc1<-ifelse(newd$prop.cemoc_trained > 0, 1, 0) 
newd$del1  <-ifelse(newd$prop.delivery_trained > 0, 1, 0) 
# 5.2 adding facility level and facility weight for each unique facility ID   
newd<-cbind(dd$INV_ID, dd$level, dd$weight, newd)
colnames(newd)<-c("INV_ID", "level", "weight", "prop_impac.trained", "prop_cemoc.trained", "prop_del.trained",
                  "impac1", "cemoc1",  "del1")

# 6. The proportion of facilities with ar least one provider who received training about impac, cemon, routine delivery care
# 6.1 create subsets for level 1, 2, 3 facilities
newd.1<-subset (newd, level=="level 1")  # 7  facilities 
newd.2<-subset (newd, level=="level 2")  # 10 facilities 
newd.3<-subset (newd, level=="level 3")  # 91 facilities 

# 6.2 create survey design objects 
des.pro  <- svydesign (id = ~ 1, weights = ~ weight, data = newd)             
des.pro1 <- svydesign (id = ~ 1, weights = ~ weight, data = newd.1) 
des.pro2 <- svydesign (id = ~ 1, weights = ~ weight, data = newd.2) 
des.pro3 <- svydesign (id = ~ 1, weights = ~ weight, data = newd.3)

# 6.3 The % of facilities that have at least provider trained with impac 
# overall 
table(newd$impac1)                             # freq   
prop.table(svytable(~ impac1, des.pro)) * 100  # weighted prop   
# level 1 
table(newd.1$impac1)                           # freq   
prop.table(svytable(~ impac1, des.pro1)) * 100 # weighted prop 
# level 2 
table(newd.2$impac1)                           # freq   
prop.table(svytable(~ impac1, des.pro2)) * 100 # weighted prop   
# level 3 
table(newd.3$impac1)                           # freq   
prop.table(svytable(~ impac1, des.pro3)) * 100 # weighted prop   

# 6.4 The % of facilities that have at least provider trained with cemon 
# overall 
table(newd$cemoc1)                             # freq   
prop.table(svytable(~ cemoc1, des.pro)) * 100  # weighted prop   
#  level 1 
table(newd.1$cemoc1)                           # freq   
prop.table(svytable(~ cemoc1, des.pro1)) * 100 # weighted prop 
#  level 2 
table(newd.2$cemoc1)                           # freq   
prop.table(svytable(~ cemoc1, des.pro2)) * 100 # weighted prop   
#  level 3 
table(newd.3$cemoc1)                           # freq   
prop.table(svytable(~ cemoc1, des.pro3)) * 100 # weighted prop   

#  6.5 The % of facilities that have at least provider trained with delivery care 
#  overall 
table(newd$del1)                               # freq   
prop.table(svytable(~ del1, des.pro)) * 100    # weighted prop   
#  level 1 
table(newd.1$del1)                             # freq   
prop.table(svytable(~ del1, des.pro1)) * 100   # weighted prop 
#  level 2 
table(newd.2$del1)                             # freq   
prop.table(svytable(~ del1, des.pro2)) * 100   # weighted prop   
#  level 3 
table(newd.3$del1)                             # freq   
prop.table(svytable(~ del1, des.pro3)) * 100   # weighted prop   
################################################################################
#                     Generate P1 Table 4 provider training  
################################################################################
# 1. Define the list of datasets and survey design objects
data_list2 <- list(newd, newd.1, newd.2, newd.3)
design_list2 <- list(des.pro, des.pro1, des.pro2, des.pro3)

# 2. Initialize empty vectors to store results
impac1_result<-character()
cemoc1_result<-character()
del1_result<-character()

for (i in seq_along(data_list2)) {
  # Calculate impac training values
  impac1_freq<- table(data_list2[[i]]$impac1)[2]
  impac1_prop <- round((prop.table(svytable(~ impac1, design_list2[[i]])) * 100)[2], 1)
  impac1 <- paste(impac1_freq, "(", impac1_prop, "%)", sep = "")
  impac1_result <- c(impac1_result, impac1)
  
  # Calculate cemoc training values
  cemoc1_freq<- table(data_list2[[i]]$cemoc1)[2]
  cemoc1_prop <- round((prop.table(svytable(~ cemoc1, design_list2[[i]])) * 100)[2], 1)
  cemoc1 <- paste(cemoc1_freq, "(", cemoc1_prop, "%)", sep = "")
  cemoc1_result <- c(cemoc1_result, cemoc1)
  
  # Calculate delivery care training values
  del1_freq<- table(data_list2[[i]]$del1)[2]
  del1_prop <- round((prop.table(svytable(~ del1, design_list2[[i]])) * 100)[2], 1)
  del1 <- paste(del1_freq, "(", del1_prop, "%)", sep = "")
  del1_result <- c(del1_result, del1)}
  
  # 4. Combine results using rbind
  p1.tb4.pro.result <- rbind(impac1_result,       cemoc1_result,     del1_result)
  colnames(p1.tb4.pro.result)<-c("Overall","Level 1", "Level 2", "Level 3")
  print(p1.tb4.pro.result)
  
  # 5. Export output table
  # 5. 1 Specify the file path 
  file_path <- "C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/paper 1 and 2/2. figures and tables/1. paper1-table 4/AFG18-p1.tb4-pro.csv"
  # 5. 2 Export the data frame to a CSV file at the specified location
  write.csv(p1.tb4.pro.result, file = file_path, row.names = TRUE)

################################################################################
########    < 3 >   AFG 18 ANC observation recode dataset           ############
################################################################################
setwd("C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/data/AFG 2018-19/ANC recode/AFAN7ASDSR")
anc.data.v0<-read_sas("AFAN7AFLSR.SAS7BDAT")    # 494 clients 

################################################################################
#                 variables to identify facility and client                    #
################################################################################
# FTYPE     C003     Urban/rural                                      
# FACIL     C004     Facility identification  
# PROVNO    C004P	   Provider serial number   
# CLNO      C004CN	 Client number            
# CLIENTWT  C005	   Client weight                                    

################################################################################
#                               Data management                                #　　　　　　　　　　　
################################################################################
# merge anc data with facility data 
anc.data.v1 <- merge(data, anc.data.v0, by = "INV_ID")
length(unique(anc.data.v1$INV_ID))               # 494 anc observations are from 74 facilities 

# remove anc observations with 0 weights and create anc.data
anc.data <-subset(anc.data.v1, C005>0)           # 491 anc observations with not zero weights 
anc.fac.id <-unique(anc.data$INV_ID)             # retrieve the facility ID of 491 anc observations
length(anc.fac.id)                               # 74 facilities 
 
# create subset with one unique facility to retrieve facility level and weight 
ddd <- anc.data %>%  
       group_by(INV_ID) %>%                      # Grouping by the facility ID column
       slice(1)                                  # Selecting the first observation within each group

# for loop to cal n of clients for each facility 
n.anc <- list ()                                 # create a list to store the n of provider for each facility
for (i in 1:length(anc.fac.id)) {
     sub.data <- subset(anc.data, INV_ID == anc.fac.id[i])
     n.anc[[i]] <- nrow(sub.data )}

facility.name<-list()
for (i in 1:length(anc.fac.id)){ 
      facility.name[[i]]<-paste0("fac", anc.fac.id[i])}        # create a list to store the ID of facility 

# n.anc.df: a table of n of anc observation for each of the 74 facilities  
n.anc.df<-as.data.frame(n.anc)                   # change list to data frame
colnames(n.anc.df)<-facility.name                # assign col name
rownames(n.anc.df)<-c("n_anc")                   # assign row name

##########################################################################################
# P2 tb1 analysis: Table 1-2,3 provider competence-assess GA, identifying PL, infection  #
##########################################################################################
# Questionnaire  New recode   label 
# OA106A  A 	   C105A        Provider/client asked about danger sign:Bleeding  
# OA106B  A 	   C105A1       Provider counseled danger sign:Bleeding   
# create new var C105a to indicate either provider/client asked about bleeding or provider counseled bleeding 
anc.data <- anc.data %>%mutate (C105a=case_when (
            C105A==1 | C105A1==1 ~ 1,
            C105A==0 & C105A1==0 ~ 0))

# OA106A  C	     C105C        Provider/client asked about danger sign:Headache or blurred vision 
# OA106B  C	     C105C1       Provider counseled danger sign:Headache or blurred vision
# create new var C105c to indicate either provider/client asked about headache(or blurred vision) or provider counseled headache(or blurred vision)
anc.data <- anc.data %>%mutate (C105c=case_when (
            C105C==1 | C105C1==1 ~ 1,
            C105C==0 & C105C1==0 ~ 0))

# OA106A  D 	   C105D        Provider/client asked about danger sign:Swollen face/hands/extremities
# OA106B  D 	   C105D1       Provider counseled danger sign:Swollen face/hands/extremities
# create new var C105d to indicate either provider/client asked about swelling or provider counseled swelling
anc.data <- anc.data %>%mutate (C105d=case_when (
            C105D==1 | C105D1==1 ~ 1,
            C105D==0 & C105D1==0 ~ 0))

# OA106A  B  	   C105B        Provider/client asked about danger sign:Fever 
# OA106B  B      C105B1       Provider counseled danger sign:Fever     
# create new var C105b to indicate either provider/client asked about fever or provider counseled fever
anc.data <- anc.data %>%mutate (C105b=case_when (
            C105B==1 | C105B1==1 ~ 1,
            C105B==0 & C105B1==0 ~ 0)) 

# OA107  A       C106A 			  Procedures performed:Blood pressure              
# OA107  D 	     C106M	  	  Procedures performed:Examine legs/feet/hands for edema 
# OA107  I       C106Q				Procedures performed:Conduct/refer ultrasound or review ultrasound report  

# 1. Cal the proportion of anc visits in which danger signs were checked and examinations performed
# 1. 1 Cal the proportion of anc visits in which bleedings were checked for each facility
yes.ble <-list()     # a list to store the number of anc visits in which bleeding were checked for each facility  
no.ble <-list()      # a list to store the number of anc visits in which bleeding were not checked for each facility  
prop.ble  <-list()   # a list to store the proportion of anc visits in which bleeding were checked for each facility   

for (i in 1:length(anc.fac.id)) {
    sub.dd <- subset(anc.data, INV_ID == anc.fac.id[i])
    yes.ble[[i]]   <- nrow(subset(sub.dd, C105a==1))
    no.ble[[i]]    <- nrow(subset(sub.dd, C105a==0))
    prop.ble[[i]]  <-(yes.ble[[i]]/(yes.ble[[i]]+no.ble[[i]]))}
prop.ble.d<-as.data.frame(t(as.data.frame(prop.ble)))

# 1. 2 Cal the proportion of anc visits in which headache or blurred vision were checked for each facility
yes.head <-list()     # a list to store the number of anc visits in which headache/blurred vision were checked for each facility  
no.head <-list()      # a list to store the number of anc visits in which headache/blurred vision were not checked for each facility  
prop.head  <-list()   # a list to store the proportion of anc visits in which headache/blurred vision  were checked for each facility   

for (i in 1:length(anc.fac.id)) {
  sub.dd <- subset(anc.data, INV_ID == anc.fac.id[i])
  yes.head[[i]]   <- nrow(subset(sub.dd, C105c==1))
  no.head[[i]]    <- nrow(subset(sub.dd, C105c==0))
  prop.head[[i]]  <-(yes.head[[i]]/(yes.head[[i]]+no.head[[i]]))}
prop.head.d<-as.data.frame(t(as.data.frame(prop.head)))

# 1. 3 Cal the proportion of anc visits in which swelling was checked for each facility
yes.swell <-list()     # a list to store the number of anc visits in which swelling was checked for each facility  
no.swell <-list()      # a list to store the number of anc visits in which swelling was not checked for each facility  
prop.swell  <-list()   # a list to store the proportion of anc visits in which swelling was checked for each facility   

for (i in 1:length(anc.fac.id)) {
  sub.dd <- subset(anc.data, INV_ID == anc.fac.id[i])
  yes.swell[[i]]   <- nrow(subset(sub.dd, C105d==1))
  no.swell[[i]]    <- nrow(subset(sub.dd, C105d==0))
  prop.swell[[i]]  <-(yes.swell[[i]]/(yes.swell[[i]]+no.swell[[i]]))}
prop.swell.d<-as.data.frame(t(as.data.frame(prop.swell)))

# 1. 4 Cal the proportion of anc visits in which fever was checked for each facility
yes.fever <-list()     # a list to store the number of anc visits in which fever was checked for each facility  
no.fever <-list()      # a list to store the number of anc visits in which fever was not checked for each facility  
prop.fever  <-list()   # a list to store the proportion of anc visits in which fever was checked for each facility   

for (i in 1:length(anc.fac.id)) {
  sub.dd <- subset(anc.data, INV_ID == anc.fac.id[i])
  yes.fever[[i]]   <- nrow(subset(sub.dd, C105b==1))
  no.fever[[i]]    <- nrow(subset(sub.dd, C105b==0))
  prop.fever[[i]]  <-(yes.fever[[i]]/(yes.fever[[i]]+no.fever[[i]]))}
prop.fever.d<-as.data.frame(t(as.data.frame(prop.fever)))

# 1. 5 Cal the proportion of anc visits in which blood pressure was checked for each facility
yes.bp <-list()     # a list to store the number of anc visits in which bp was checked for each facility  
no.bp <-list()      # a list to store the number of anc visits in which bp was not checked for each facility  
prop.bp  <-list()   # a list to store the proportion of anc visits in which bp was checked for each facility   

for (i in 1:length(anc.fac.id)) {
  sub.dd <- subset(anc.data, INV_ID == anc.fac.id[i])
  yes.bp[[i]]   <- nrow(subset(sub.dd, C106A==1))
  no.bp[[i]]    <- nrow(subset(sub.dd, C106A==0))
  prop.bp[[i]]  <-(yes.bp[[i]]/(yes.bp[[i]]+no.bp[[i]]))}
prop.bp.d<-as.data.frame(t(as.data.frame(prop.bp)))

# 1. 6 Cal the proportion of anc visits in which edema was checked for each facility
yes.edema <-list()     # a list to store the number of anc visits in which edema was checked for each facility  
no.edema <-list()      # a list to store the number of anc visits in which edema was not checked for each facility  
prop.edema  <-list()   # a list to store the proportion of anc visits in which edema was checked for each facility   

for (i in 1:length(anc.fac.id)) {
  sub.dd <- subset(anc.data, INV_ID == anc.fac.id[i])
  yes.edema[[i]]   <- nrow(subset(sub.dd, C106M==1))
  no.edema[[i]]    <- nrow(subset(sub.dd, C106M==0))
  prop.edema[[i]]  <-(yes.edema[[i]]/(yes.edema[[i]]+no.edema[[i]]))}
prop.edema.d<-as.data.frame(t(as.data.frame(prop.edema)))

# 1. 7 Cal the proportion of anc visits in which provider conducted/referred a ultrasound or read ultrasound report for each facility
yes.echo <-list()     # a list to store the number of anc visits in which echo was conducted/referred/read for each facility  
no.echo <-list()      # a list to store the number of anc visits in which echo was not conducted/referred/read for each facility  
prop.echo  <-list()   # a list to store the proportion of anc visits in which echo was conducted/referred/read for each facility   

for (i in 1:length(anc.fac.id)) {
  sub.dd <- subset(anc.data, INV_ID == anc.fac.id[i])
  yes.echo[[i]]   <- nrow(subset(sub.dd, C106Q==1))
  no.echo[[i]]    <- nrow(subset(sub.dd, C106Q==0))
  prop.echo[[i]]  <-(yes.echo[[i]]/(yes.echo[[i]]+no.echo[[i]]))}
prop.echo.d<-as.data.frame(t(as.data.frame(prop.echo)))

# 2. create new anc.newd table to indicate in at least one ANC visits, danger signs were checked for each facility 
anc.newd<-cbind(ddd$level,  ddd$C005, 
                prop.ble.d, prop.head.d, prop.swell.d, prop.fever.d,  prop.bp.d,  prop.edema.d, prop.echo.d)
rownames(anc.newd)<-facility.name
colnames(anc.newd)<-c("level",           "fac_weight",      "prop_checkbleed", "prop_checkhead", 
                      "prop_checkswell", "prop_checkfever", "prop_checkbp",    "prop_checkedema",
                      "prop_checkecho")
anc.newd$check_bleed<-ifelse(anc.newd$prop_checkbleed >0, 1, 0)  # create new var check_bleed to indicate in at least one ANC, bleeding was checked  
anc.newd$check_head <-ifelse(anc.newd$prop_checkhead  >0, 1, 0)  # create new var check_head to indicate in at least one ANC, headache was checked  
anc.newd$check_swell<-ifelse(anc.newd$prop_checkswell >0, 1, 0)  # create new var check_swell to indicate in at least one ANC, swelling was checked  
anc.newd$check_fever<-ifelse(anc.newd$prop_checkfever >0, 1, 0)  # create new var check_fever to indicate in at least one ANC, fever was checked  
anc.newd$check_bp   <-ifelse(anc.newd$prop_checkbp    >0, 1, 0)  # create new var check_bp to indicate in at least one ANC, BP measurement was performed  
anc.newd$check_edema<-ifelse(anc.newd$prop_checkedema >0, 1, 0)  # create new var check_edema to indicate in at least one ANC, edema was checked
anc.newd$check_echo <-ifelse(anc.newd$prop_checkecho  >0, 1, 0)  # create new var check_echo to indicate in at least one ANC, echo was conducted/refered or an echo report was read

# 3. create subsets for level 1, 2, 3 facilities and survey design objects 
# 3. 1 
anc.newd1<-subset (anc.newd, level=="level 1")  # 4  facilities 
anc.newd2<-subset (anc.newd, level=="level 2")  # 5  facilities 
anc.newd3<-subset (anc.newd, level=="level 3")  # 65 facilities 

# 3. 2 create survey design objects 
des.anc  <- svydesign (id = ~ 1, weights = ~ fac_weight, data = anc.newd)             
des.anc1 <- svydesign (id = ~ 1, weights = ~ fac_weight, data = anc.newd1) 
des.anc2 <- svydesign (id = ~ 1, weights = ~ fac_weight, data = anc.newd2) 
des.anc3 <- svydesign (id = ~ 1, weights = ~ fac_weight, data = anc.newd3)

# 4. Cal the frequency and proportion of facilities in which at least one provider asked about (or client mentioned) danger signs in ANC visits
# 4. 1 cal the frequency and proportion of facilities in which at least one provider asked about (or client mentioned) haeding in ANC visits
# overall 
table(anc.newd$check_bleed)                         # freq   
prop.table(svytable(~ check_bleed, des.anc)) * 100  # weighted prop   
#  level 1 
table(anc.newd1$check_bleed)                        # freq   
prop.table(svytable(~ check_bleed, des.anc1)) * 100 # weighted prop 
#  level 2 
table(anc.newd2$check_bleed)                        # freq   
prop.table(svytable(~ check_bleed, des.anc2)) * 100 # weighted prop   
#  level 3 
table(anc.newd3$check_bleed)                        # freq   
prop.table(svytable(~ check_bleed, des.anc3)) * 100 # weighted prop  

# 4. 2 cal the frequency and proportion of facilities in which at least one provider asked about (or client mentioned) headache in ANC visits
# overall 
table(anc.newd$check_head)                          # freq   
prop.table(svytable(~ check_head, des.anc)) * 100   # weighted prop   
#  level 1 
table(anc.newd1$check_head)                         # freq   
prop.table(svytable(~ check_head, des.anc1)) * 100  # weighted prop 
#  level 2 
table(anc.newd2$check_head)                         # freq   
prop.table(svytable(~ check_head, des.anc2)) * 100  # weighted prop   
#  level 3 
table(anc.newd3$check_head)                         # freq   
prop.table(svytable(~ check_head, des.anc3)) * 100  # weighted prop 

# 4. 3 cal the frequency and proportion of facilities in which at least one provider asked about (or client mentioned) swelling in ANC visits
# overall 
table(anc.newd$check_swell)                          # freq   
prop.table(svytable(~ check_swell, des.anc)) * 100   # weighted prop   
#  level 1 
table(anc.newd1$check_swell)                         # freq   
prop.table(svytable(~ check_swell, des.anc1)) * 100  # weighted prop 
#  level 2 
table(anc.newd2$check_swell)                         # freq   
prop.table(svytable(~ check_swell, des.anc2)) * 100  # weighted prop   
#  level 3 
table(anc.newd3$check_swell)                         # freq   
prop.table(svytable(~ check_swell, des.anc3)) * 100  # weighted prop

# 4. 4 cal the frequency and proportion of facilities in which at least one provider asked about (or client mentioned) fever in ANC visits
# overall 
table(anc.newd$check_fever)                          # freq   
prop.table(svytable(~ check_fever, des.anc)) * 100   # weighted prop   
#  level 1 
table(anc.newd1$check_fever)                         # freq   
prop.table(svytable(~ check_fever, des.anc1)) * 100  # weighted prop 
#  level 2 
table(anc.newd2$check_fever)                         # freq   
prop.table(svytable(~ check_fever, des.anc2)) * 100  # weighted prop   
#  level 3 
table(anc.newd3$check_fever)                         # freq   
prop.table(svytable(~ check_fever, des.anc3)) * 100  # weighted prop

# 4. 5 cal the frequency and proportion of facilities in which at least one provider checked blood pressure in ANC visits
# overall 
table(anc.newd$check_bp)                          # freq   
prop.table(svytable(~ check_bp, des.anc)) * 100   # weighted prop   
#  level 1 
table(anc.newd1$check_bp)                         # freq   
prop.table(svytable(~ check_bp, des.anc1)) * 100  # weighted prop 
#  level 2 
table(anc.newd2$check_bp)                         # freq   
prop.table(svytable(~ check_bp, des.anc2)) * 100  # weighted prop   
#  level 3 
table(anc.newd3$check_bp)                         # freq   
prop.table(svytable(~ check_bp, des.anc3)) * 100  # weighted prop

# 4. 6 cal the frequency and proportion of facilities in which at least one provider checked edema in ANC visits
# overall 
table(anc.newd$check_edema)                          # freq   
prop.table(svytable(~ check_edema, des.anc)) * 100   # weighted prop   
#  level 1 
table(anc.newd1$check_edema)                         # freq   
prop.table(svytable(~ check_edema, des.anc1)) * 100  # weighted prop 
#  level 2 
table(anc.newd2$check_edema)                         # freq   
prop.table(svytable(~ check_edema, des.anc2)) * 100  # weighted prop   
#  level 3 
table(anc.newd3$check_edema)                         # freq   
prop.table(svytable(~ check_edema, des.anc3)) * 100  # weighted prop

# 4. 7 cal the frequency and proportion of facilities in which at least one provider checked echo in ANC visits
# overall 
table(anc.newd$check_echo)                          # freq   
prop.table(svytable(~ check_echo, des.anc)) * 100   # weighted prop   
#  level 1 
table(anc.newd1$check_echo)                         # freq   
prop.table(svytable(~ check_echo, des.anc1)) * 100  # weighted prop 
#  level 2 
table(anc.newd2$check_echo)                         # freq   
prop.table(svytable(~ check_echo, des.anc2)) * 100  # weighted prop   
#  level 3 
table(anc.newd3$check_echo)                         # freq   
prop.table(svytable(~ check_echo, des.anc3)) * 100  # weighted prop

################################################################################
#                       Generate P2 Table 1 ANC observation    
################################################################################
# 1. Define the list of datasets and survey design objects
data_list3 <- list(anc.newd, anc.newd1, anc.newd2, anc.newd3)
design_list3 <- list(des.anc, des.anc1, des.anc2, des.anc3)

# 2. Initialize empty vectors to store results
check_bleed_result<-character()
check_head_result<-character()
check_swell_result<-character()
check_fever_result<-character()
check_bp_result<-character()
check_edema_result<-character()
check_echo_result<-character()

for (i in seq_along(data_list3)) {
  # Calculate bleed values
  check_bleed_freq<- table(data_list3[[i]]$check_bleed)[2]
  check_bleed_prop <- round((prop.table(svytable(~ check_bleed, design_list3[[i]])) * 100)[2], 1)
  check_bleed <- paste(check_bleed_freq, "(", check_bleed_prop, "%)", sep = "")
  check_bleed_result <- c(check_bleed_result, check_bleed)
  
  # Calculate headache values
  check_head_freq<- table(data_list3[[i]]$check_head)[2]
  check_head_prop <- round((prop.table(svytable(~ check_head, design_list3[[i]])) * 100)[2], 1)
  check_head <- paste(check_head_freq, "(", check_head_prop, "%)", sep = "")
  check_head_result <- c(check_head_result, check_head)
  
  # Calculate swell values
  check_swell_freq<- table(data_list3[[i]]$check_swell)[2]
  check_swell_prop <- round((prop.table(svytable(~ check_swell, design_list3[[i]])) * 100)[2], 1)
  check_swell <- paste(check_swell_freq, "(", check_swell_prop, "%)", sep = "")
  check_swell_result <- c(check_swell_result, check_swell)
  
  # Calculate fever values
  check_fever_freq<- table(data_list3[[i]]$check_fever)[2]
  check_fever_prop <- round((prop.table(svytable(~ check_fever, design_list3[[i]])) * 100)[2], 1)
  check_fever <- paste(check_fever_freq, "(", check_fever_prop, "%)", sep = "")
  check_fever_result <- c(check_fever_result, check_fever)
  
  # Calculate bp values
  check_bp_freq<- table(data_list3[[i]]$check_bp)[2]
  check_bp_prop <- round((prop.table(svytable(~ check_bp, design_list3[[i]])) * 100)[2], 1)
  check_bp <- paste(check_bp_freq, "(", check_bp_prop, "%)", sep = "")
  check_bp_result <- c(check_bp_result, check_bp)
  
  # Calculate edema values
  check_edema_freq<- table(data_list3[[i]]$check_edema)[2]
  check_edema_prop <- round((prop.table(svytable(~ check_edema, design_list3[[i]])) * 100)[2], 1)
  check_edema <- paste(check_edema_freq, "(", check_edema_prop, "%)", sep = "")
  check_edema_result <- c(check_edema_result, check_edema)
  
  # Calculate echo values
  check_echo_freq<- table(data_list3[[i]]$check_echo)[2]
  check_echo_prop <- round((prop.table(svytable(~ check_echo, design_list3[[i]])) * 100)[2], 1)
  check_echo <- paste(check_echo_freq, "(", check_echo_prop, "%)", sep = "")
  check_echo_result <- c(check_echo_result, check_echo)}

# 4. Combine results using rbind
p2.tb1.anc.result <- rbind (check_echo_result,  check_bleed_result, check_head_result, check_swell_result, 
                            check_bp_result,    check_edema_result, check_fever_result) 

colnames(p2.tb1.anc.result)<-c("Overall","Level 1", "Level 2", "Level 3")
print(p2.tb1.anc.result)

# 5. Export output table
# 5. 1 Specify the file path 
file_path <- "C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/paper 1 and 2/2. figures and tables/2. paper2-table 1/AFG18-p2.tb1-anc.csv"
# 5. 2 Export the data frame to a CSV file at the specified location
write.csv(p2.tb1.anc.result, file = file_path, row.names = TRUE)

################################################################################
#                   Collapsing readiness items into categories
################################################################################
# create dummy variables for all 35 items from inventory
d<-merge(d, newd, by = "INV_ID", all.x = TRUE)

d$ULTRAFUN<-ifelse(d$ultrafun=="Equipment used and observed, and working", 1, 0)
d$THERMOFUN<-ifelse(d$V533i=="Observed, functioning", 1, 0)
d$HEMAFUN<-ifelse(d$hemafun=="Equipment used and observed, and working", 1, 0)
d$HIV.RDT<-ifelse(d$V462a=="Yes, observed, at least 1 valid", 1, 0)
d$SYP.RDT<-ifelse(d$V462e=="Yes, observed, at least 1 valid", 1, 0)
d$DEL.PACK<-ifelse(d$V531xh=="Yes, observed", 1, 0)
d$C.CLAMP<-ifelse(d$V531xl=="Yes, observed", 1, 0)
d$V.EXTRACTOR<-ifelse(d$V533g=="Observed, functioning", 1, 0)
d$DNC.KIT<-ifelse(d$V533h=="Observed, functioning", 1, 0)
d$L.FORCEP<-ifelse(d$V531xj=="Yes, observed", 1, 0)
d$M.FORCEP<-ifelse(d$V531xk=="Yes, observed", 1, 0)
d$ABX<-ifelse(d$V534g=="Yes, observed, at least 1 valid", 1, 0)
d$DIAZEPAM<-ifelse(d$V534d=="Yes, observed, at least 1 valid", 1, 0)
d$OXYTOCIN<-ifelse(d$V534c=="Yes, observed, at least 1 valid", 1, 0)
d$CS.WORKER<-ifelse(d$V558new=="Yes, observed schedule", 1, 0)
d$ANES.WORKER<-ifelse(d$V559new=="Yes, observed schedule", 1, 0)
d$BEMONC<-ifelse(d$V537c=="Yes, observed", 1, 0)
d$CEMONC<-ifelse(d$V537e=="Yes, observed", 1, 0)
d$SUCTION<-ifelse(d$V536e=="Observed, functioning", 1, 0)
d$STETHO<-ifelse(d$V533d=="Observed, functioning", 1, 0)
d$BAGNMASK<-ifelse(d$V536a=="Observed, functioning", 1, 0)
d$INCU<-ifelse(d$V536b=="Observed, functioning", 1, 0)
d$HEAT<-ifelse(d$V536c=="Observed, functioning", 1, 0)
d$OXI<-ifelse(d$V166N=="Observed, functioning", 1, 0)
d$CONCEN<-ifelse(d$V166O=="Observed, functioning", 1, 0)
d$CYLINDER<-ifelse(d$V166P=="Observed, functioning", 1, 0)
d$DIS<-ifelse(d$V166Q=="Observed, functioning", 1, 0)
d$GLUCOFUN<-ifelse(d$glucofun=="Equipment used and observed, and working", 1, 0)
d$STRIPFUN<-ifelse(d$stripfun=="Equipment used and observed, and working", 1, 0)
d$SOAP<-ifelse(d$V946_13new=="Observed", 1, 0)
d$GLOVE<-ifelse(d$V946_09new=="Observed", 1, 0)
d$GUIDE<-ifelse(d$V537d=="Yes, observed", 1, 0)

# create new var INDEX to add up 35 dummy variables
d["INDEX"] <- 
  d["ULTRAFUN"]   + d["THERMOFUN"]   + d["HEMAFUN"]  + d["HIV.RDT"]     + d["SYP.RDT"]     + d["DEL.PACK"] + 
  d["C.CLAMP"]    + d["V.EXTRACTOR"] + d["DNC.KIT"]  + d["L.FORCEP"]    + d["M.FORCEP"]    + d["ABX"] + 
  d["DIAZEPAM"]   + d["OXYTOCIN"]    + d["CS.WORKER"]+ d["ANES.WORKER"] + d["BEMONC"]      + d["CEMONC"] + 
  d["SUCTION"]    + d["STETHO"]      + d["BAGNMASK"] + d["INCU"]        + d["HEAT"]        + d["OXI"] + 
  d["CONCEN"]     + d["CYLINDER"]    + d["DIS"]      + d["GLUCOFUN"]    + d["STRIPFUN"]    + d["SOAP"] + 
  d["GLOVE"]      + d["impac1"]      + d["cemoc1"]   + d["del1"]        + d["GUIDE"]

# create new var INDEX-GAnPL to indicate the ULTRAFUN
d["INDEX.GAnPL"]<-d["ULTRAFUN"]

# create new var INDEX-Mat.inf to indicate the equipment and diagnostics for maternal infection
d["INDEX.Mat.inf"]<-d["THERMOFUN"]+ d["HEMAFUN"]+ d["HIV.RDT"]+ d["SYP.RDT"]

# create new var INDEX-Birth.care to indicate items for adequate child birth care
d["INDEX.Birth.care"] <- 
  d["DEL.PACK"]   + d["C.CLAMP"] + d["V.EXTRACTOR"] + d["DNC.KIT"]  + d["L.FORCEP"]  +
  d["M.FORCEP"]   + d["ABX"]     + d["DIAZEPAM"]    + d["OXYTOCIN"] + d["CS.WORKER"] +
  d["ANES.WORKER"]+ d["BEMONC"]  + d["CEMONC"]

# create new var INDEX-Preterm.care to indicate items for adequate preterm newborn care
d["INDEX.Preterm.care"] <- 
  d["SUCTION"]  + d["STETHO"]  + d["BAGNMASK"] + d["INCU"]  + d["HEAT"]     +
  d["OXI"]      + d["CONCEN"]  + d["CYLINDER"] + d["DIS"]   + d["GLUCOFUN"] +
  d["STRIPFUN"] + d["SOAP"]    + d["GLOVE"]    + d["impac1"]+ d["cemoc1"]   +
  d["del1"]     + d["GUIDE"] 

d$index.ganpl<-d$INDEX.GAnPL/1
d$index.mat.inf<-d$INDEX.Mat.inf/4
d$index.birth.care<-(d$INDEX.Birth.care)/13
d$index.preterm.care<-(d$INDEX.Preterm.care)/17
d$index.mean<-rowSums(d["index.ganpl"]+d["index.mat.inf"]+d["index.birth.care"]+d["index.preterm.care"])/4 

# Create survey design object
wt<-d$V005/100000
wt1<-d1$V005/100000
wt2<-d2$V005/100000
wt3<-d3$V005/100000

# create subset datasets for three levels of facilities 
d1<-subset (d, level.x=="level 1")
d2<-subset (d, level.x=="level 2")
d3<-subset (d, level.x=="level 3")

des <- svydesign (id = ~ 1, weights = ~ wt, data = d)
des1 <- svydesign (id = ~ 1, weights = ~ wt1, data = d1)
des2 <- svydesign (id = ~ 1, weights = ~ wt2, data = d2)
des3 <- svydesign (id = ~ 1, weights = ~ wt3, data = d3)

# calculate weighted mean for overall 
m.index.ganpl<-svymean(~index.ganpl, design = des, na.rm=T)[[1]]
m.index.mat.inf<-svymean(~index.mat.inf, design = des, na.rm=T)[[1]]
m.index.birth.care<-svymean(~index.birth.care, design = des, na.rm=T)[[1]]
m.index.preterm.care<-svymean(~index.preterm.care, design = des, na.rm=T)[[1]]
m.index.mean<-(m.index.ganpl+m.index.mat.inf+m.index.birth.care+m.index.preterm.care)/4

IND<-c(m.index.ganpl, m.index.mat.inf, m.index.birth.care, m.index.preterm.care, m.index.mean)
overall.IND<-as.data.frame(t(IND))

# calculate weighted mean for level 1 
m.index.ganpl.lev1<-svymean(~index.ganpl, design = des1, na.rm=T)[[1]]
m.index.mat.inf.lev1<-svymean(~index.mat.inf, design = des1, na.rm=T)[[1]]
m.index.birth.care.lev1<-svymean(~index.birth.care, design = des1, na.rm=T)[[1]]
m.index.preterm.care.lev1<-svymean(~index.preterm.care, design = des1, na.rm=T)[[1]]
m.index.mean.lev1<-(m.index.ganpl.lev1+m.index.mat.inf.lev1+m.index.birth.care.lev1+m.index.preterm.care.lev1)/4

IND.lev1<-c(m.index.ganpl.lev1, m.index.mat.inf.lev1, m.index.birth.care.lev1, m.index.preterm.care.lev1, m.index.mean.lev1)
level1.IND<-as.data.frame(t(IND.lev1))

# calculate weighted mean for level 2
m.index.ganpl.lev2<-svymean(~index.ganpl, design = des2, na.rm=T)[[1]]
m.index.mat.inf.lev2<-svymean(~index.mat.inf, design = des2, na.rm=T)[[1]]
m.index.birth.care.lev2<-svymean(~index.birth.care, design = des2, na.rm=T)[[1]]
m.index.preterm.care.lev2<-svymean(~index.preterm.care, design = des2, na.rm=T)[[1]]
m.index.mean.lev2<-(m.index.ganpl.lev2+m.index.mat.inf.lev2+m.index.birth.care.lev2+m.index.preterm.care.lev2)/4

IND.lev2<-c(m.index.ganpl.lev2, m.index.mat.inf.lev2, m.index.birth.care.lev2, m.index.preterm.care.lev2, m.index.mean.lev2)
level2.IND<-as.data.frame(t(IND.lev2))

# calculate weighted mean for level 3
m.index.ganpl.lev3<-svymean(~index.ganpl, design = des3, na.rm=T)[[1]]
m.index.mat.inf.lev3<-svymean(~index.mat.inf, design = des3, na.rm=T)[[1]]
m.index.birth.care.lev3<-svymean(~index.birth.care, design = des3, na.rm=T)[[1]]
m.index.preterm.care.lev3<-svymean(~index.preterm.care, design = des3, na.rm=T)[[1]]
m.index.mean.lev3<-(m.index.ganpl.lev3+m.index.mat.inf.lev3+m.index.birth.care.lev3+m.index.preterm.care.lev3)/4

IND.lev3<-c(m.index.ganpl.lev3, m.index.mat.inf.lev3, m.index.birth.care.lev3, m.index.preterm.care.lev3, m.index.mean.lev3)
level3.IND<-as.data.frame(t(IND.lev3))

# create table to combine overall, level 1, level 2, and level 3 data
tb<-rbind(overall.IND, level1.IND, level2.IND, level3.IND)
rownames(tb)<-c("Overall", "Level 1", "Level 2", "Level 3")
colnames(tb)<-c("assessGA.index", "infection.index", "childbirth.index", "pretermcare.index", "mean.index")

# Export AFG.tb
# Specify the file path 
file_path <- "C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/paper 1 and 2/2. figures and tables/5. paper1 readiness indexes by categories data/AFG.tb.csv."
write.csv(tb, file = file_path, row.names = TRUE)

################################################################################
#                                 Box plot data 
################################################################################
# create new var ACSEVER to indicate ever/never provided ACS
d <-d %>% mutate(ACSEVER=case_when(
  V554h=="Never provided"                              ~ "Never provided ACS",
  V554h=="Provided in past 3 months"                   ~ "Ever provided ACS",
  V554h=="Ever provided, not in past 3 months"         ~ "Ever provided ACS",
  V554h=="Ever provided, DK/missing if past 3 months"  ~ "Ever provided ACS"))

AFG.plot.d<-select(d, level.x, V001, V003, V007, V008, weight.x, ACSEVER, steroid, index.mean)
AFG.plot.d$country<-c("AFG18")
AFG.plot.d$year<-c("2018")

# create plot data 
AFG.plot.d<-AFG.plot.d[, c(10:11, 1:9)]
colnames(AFG.plot.d)<-c("country",   "year",          "level",   "region",       "urban", 
                        "fac.type",  "fac.authoriry", "weight",  "ACSEVER", "availability", "index.mean")

# Export AFG.plot.d  
    # Specify the file path 
    file_path <- "C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/paper 1 and 2/2. figures and tables/4. paper1 plot/AFG.plot.d.csv."
    write.csv(AFG.plot.d, file = file_path, row.names = TRUE)

################################################################################
#                               Scatter plot data 
################################################################################
# 1. extract data for acs.everuse 
setwd("C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/paper 1 and 2/2. figures and tables/2. paper2-table 1")
col1<-t(read.csv("AFG18-p2.tb1.csv")[1,])[2:5,] 
    
# 2. extract data for acs.availability
setwd("C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/paper 1 and 2/2. figures and tables/1. paper1-table 4")
col2<-t(read.csv("AFG18-p1.tb4.csv")[3,])[2:5,]

# 3. extract data for mean readiness index 
setwd("C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/paper 1 and 2/2. figures and tables/5. paper1 readiness indexes by categories data")
col3<-read.csv("AFG.tb.csv")[,5]
    
# combine col1 col2 col3 
AFG.dt<-as.data.frame(cbind(col1, col2, col3)[2:4,])
AFG.dt$level<-c("level 1", "level 2", "level 3")
AFG.dt<-AFG.dt[,c(4,1:3)]

rownames(AFG.dt)<-c("AFG18.level1", "AFG18.level2", "AFG18.level3")
colnames(AFG.dt)<-c("level","ACS.everuse", "ACS.availability", "Mean.readiness.index")

AFG.dt$ACS.EVERUSE <- as.numeric(gsub(".*\\((.*?)%\\)", "\\1", AFG.dt[["ACS.everuse"]]))
AFG.dt$ACS.AVAI <- as.numeric(gsub(".*\\((.*?)%\\)", "\\1", AFG.dt[["ACS.availability"]]))
AFG.dt$MEAN.READINESS.INDEX<-round(as.numeric(AFG.dt$Mean.readiness.index) * 100, 1)

# Export AFG.dt
    # Specify the file path 
    file_path <- "C:/Users/wench/Desktop/21 GW/dissertation preparation/6. dissertation/paper 1 and 2/2. figures and tables/4. paper1 plot/scatterplot/AFG.scatterplot.csv."
    write.csv(AFG.dt, file = file_path, row.names = TRUE)