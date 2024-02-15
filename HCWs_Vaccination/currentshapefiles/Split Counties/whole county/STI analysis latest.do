*=====================================================================
*STI in Zimbabwe, risk factors analysis
*======================================================================
import excel "C:\Users\EOC PC1\Desktop\STATA DATA SET\STI basicdata (1).xlsx", sheet("basicdata") firstrow
*======================================================================
*Data management
*======================================================================
edit
encode Sex, gen(Sex1)
codebook Sex Sex1
tab Sex Sex1
tab Sex
tab Sex1
encode Sex, generate(Sex2)
destring A1Age, gen(Age1)
tostring A1Age, gen(Age1)
sum A1Age
sum A1Age, detail
encode A2Occupation, gen(Occupation)
encode A4LevelOfEducation, gen(LevelofEducation1)
encode A5MaritalStatus, gen(MaritalStatus1)
encode E8WhyhaveSTI, gen(WhyhaveSTI1)
encode N10givereceiveforsex, gen(giverecieveforsex1)
encode N11Usedcondom, gen(Usedcondom)
encode N13TakenAlcohol, gen(TakenAlcohol)
encode A3Church, gen(Church)
recode A1Age 16/25=1 26/35=2 36/45=3 46/55=4 56/65=5, gen(AgeGroup)
lab def AgeGroup 1"16-25" 2"26-35" 3"36-45" 4"46-55" 5"56-65"
lab val AgeGroup AgeGroup
recode CaseStatus 2=0 1=1, gen(Outcome)
lab def Outcome 1"Cases" 0"Controls"
lab val Outcome Outcome
*=========================================================================
*Descriptive analysis
*=========================================================================
tab Sex1 Outcome
sum A1Age Outcome
tab Sex1 Outcome, col chi
tab LevelofEducation1 Outcome
tab LevelofEducation1 Outcome, col row chi
*=============================================================================
*MultiVARIATE ANALYSIS
*============================================================================
tab Occupation Outcome, row col chi
logistic Outcome Usedcondom
