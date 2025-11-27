#!/usr/bin/Rscript

## MEMORY NYASHA MHEMBERE
## University of Edinburgh - School of Informatics
## Msc Data Science - Dissertation (DSTI Student)
## s1772433
## 2020


library(dplyr)

set.seed(123)

#*******************************IMPORT DATA**********************************
#****************************************************************************

##data <- read.csv('D:\\Madhiva\\Docs\\Analysis\\HAALSI_baseline_dataverse_14Apr2017.csv')
##mydata <- data

args <- commandArgs(trailingOnly = TRUE)
input_csv <- args[1]
output_csv <- args[2]

#dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

data <- read.csv(input_csv)
mydata <- data



###########################*****DATA CLEANING****#####################################

#*******************************DATA CLEANING**********************************
#****************************************************************************
#*
#Smoking-coding
mydata$cm059 <- ifelse(mydata$cm059 == '2 (NO) No','No',ifelse(mydata$cm059 == '1 (YES) Yes','Yes',''))
mydata$cm061 <- ifelse(mydata$cm061 == '2 (NO) No','No',ifelse(mydata$cm061 == '1 (YES) Yes','Yes',''))

# add variable for smoker                       
mydata$smoking <- ifelse(mydata$cm059 == 'No','Never',
                         ifelse((mydata$cm059 == 'Yes' & mydata$cm061 == 'Yes'),'Current',
                                ifelse((mydata$cm059 == 'Yes' & mydata$cm061 == 'No'),'Former','')))
#add variable age category
mydata$agecat <- ifelse(mydata$rage < 50,'40-49',
                        ifelse(mydata$rage>49 & mydata$rage <60,'50-59',
                               ifelse(mydata$rage>59 & mydata$rage <70,'60-69',
                                      ifelse(mydata$rage>69 & mydata$rage <80,'70-79','80+'))))

# add variable for education                       
mydata$educ4 <- ifelse(mydata$c_bd_educ4 == 'No formal education','no formal education',
                       ifelse(mydata$c_bd_educ4 == 'Secondary or more (12+ years)','secondary or more',
                              ifelse(mydata$c_bd_educ4 == 'Some primary (1-7 years)','some primary',
                                     ifelse(mydata$c_bd_educ4 == 'Some secondary (8-11 years)','some secondary',''))))

# add variable for HH wealth                       
mydata$wealthindex <- ifelse(mydata$c_wealthindex == '1','Q1',
                             ifelse(mydata$c_wealthindex == '2','Q2',
                                    ifelse(mydata$c_wealthindex == '3','Q3',
                                           ifelse(mydata$c_wealthindex == '4','Q4',
                                                  ifelse(mydata$c_wealthindex == '5','Q5','')))))



#Alcohol YES/NO 
mydata$cm068 <- ifelse(mydata$cm068 == '2 (NO) No','No',ifelse(mydata$cm068 == '1 (YES) Yes','Yes',''))
mydata$cm069 <- ifelse(mydata$cm069 == '2 (NO) No','No',ifelse(mydata$cm069 == '1 (YES) Yes','Yes',''))


# add variable for smoker                     
mydata$alcohol <- ifelse(mydata$cm068 == 'No','Never',
                         ifelse((mydata$cm068 == 'Yes' & mydata$cm069 == 'Yes'),'Current',
                                ifelse((mydata$cm068 == 'Yes' & mydata$cm069 == 'No'),'Former','')))

#alcohol frequency
mydata$alcoholFreq <- ifelse(substr(mydata$cm070,1,1) %in% c(1,2),'>= 5 days per wk',
                             ifelse(substr(mydata$cm070,1,1) %in% c(3,4,5),'< 5 days per week',NA))

#Fruit and vegie serving
mydata$fruits.week <- (mydata$cm078 * mydata$cm079)
mydata$fruits.week[is.na(mydata$fruits.week)] <- 0   # set na to 0

mydata$vegie.week <- (mydata$cm080 * mydata$cm081)
mydata$vegie.week[is.na(mydata$vegie.week)] <- 0 # set na to 0

mydata$FruitVegie <- (mydata$fruits.week + mydata$vegie.week)
mydata$FruitVegie[is.na(mydata$FruitVegie)] <- 0 # set na to 0

mydata$Fruit.Vegie.serving <- ifelse(mydata$FruitVegie >=5,'>=5 serving per week', '<5 serving per week' )

#get a subset of cvd risk factor data
cvd_risk<-mydata %>%
  dplyr::select(prim_key, rage, rsex, c_bs_bmi, c_bs_bmicat, c_bs_chol, c_bs_ldl, c_bs_trig, 
                c_bs_hdl,c_bs_glucose,c_bs_mean_dia, c_bs_mean_sys, c_bs_weight, 
                c_bs_height, c_pt_waist,c_pt_hip,c_pt_whratio, cm059, cm061,cm068,
                cm069, cm078, cm079,cm080, cm081,
                pa006, pa007_hours, pa007_minutes, pa009,pa010_hours, pa010_minutes,
                pa013, pa014_hours, pa014_minutes, pa017,pa018_hours, pa018_minutes,
                pa020, pa021_hours, pa021_minutes, wealthindex, educ4,
                agecat, smoking, alcohol, alcoholFreq,FruitVegie,Fruit.Vegie.serving ) 


temp_cvd_risk<-data %>%
  dplyr::select(prim_key, rage, rsex, c_bs_bmi, c_bs_bmicat, c_bs_chol, c_bs_ldl, c_bs_trig, 
                c_bs_hdl,c_bs_glucose,c_bs_mean_dia, c_bs_mean_sys, c_bs_weight, 
                c_bs_height, c_pt_waist,c_pt_hip,c_pt_whratio) 

cvd_risk<- 
  dplyr::rename(cvd_risk,id=prim_key
                ,age=rage
                ,sex=rsex
                ,bmi=c_bs_bmi
                ,bmicat=c_bs_bmicat
                ,chol=c_bs_chol
                
                ,ldl=c_bs_ldl
                ,trig=c_bs_trig
                ,hdl=c_bs_hdl
                
                ,glucose=c_bs_glucose
                ,dia=c_bs_mean_dia
                ,sys=c_bs_mean_sys
                ,weight=c_bs_weight
                ,height=c_bs_height
                ,waist=c_pt_waist
                
                ,hip=c_pt_hip
                ,WHR=c_pt_whratio
                ,ever.smoke=cm059
                ,current.smoke=cm061
                ,ever.alcohol=cm068
                ,current.alcohol=cm069
                ,fruit.days=cm078
                ,fruit.servings=cm079
                ,veg.days=cm080
                ,veg.servings=cm081
                ,wealth.index=wealthindex
                ,edu=educ4
                ,walkingDays = pa013
                ,walkingHours = pa014_hours
                ,walkingMins = pa014_minutes
                ,vigorousDays = pa017
                ,vigorousHours = pa018_hours
                ,vigorousMins = pa018_minutes
                ,moderateDays = pa020
                ,moderateHours = pa021_hours
                ,moderateMins = pa021_minutes
  )

write.csv(cvd_risk,output_csv,row.names = FALSE)
