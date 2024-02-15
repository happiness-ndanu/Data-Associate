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
