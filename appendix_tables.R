cces08$whtbaprot <- cces08$bagain + cces08$white + cces08$protestant
cces08$whtbaprot <- Recode(cces08$whtbaprot, "3=1; else=0")
cces08$whtevan <- cces08$evangelical + cces08$white
cces08$whtevan <- Recode(cces08$whtevan, "2=1; else=0")


cces12$whtbaprot <- cces12$bagain + cces12$white + cces12$protestant
cces12$whtbaprot <- Recode(cces12$whtbaprot, "3=1; else=0")
cces12$whtevan <- cces12$evangelical + cces12$white
cces12$whtevan <- Recode(cces12$whtevan, "2=1; else=0")


cces16$whtbaprot <- cces16$bagain + cces16$white + cces16$protestant
cces16$whtbaprot <- Recode(cces16$whtbaprot, "3=1; else=0")
cces16$whtevan <- cces16$evangelical + cces16$white
cces16$whtevan <- Recode(cces16$whtevan, "2=1; else=0")



gss10$whtbaprot <- gss10$white  + gss10$protestant  + gss10$bagain 
gss10$whtbaprot <- Recode(gss10$whtbaprot, "3=1; else=0")
gss10$whtevan <- gss10$white  + gss10$evangelical 
gss10$whtevan <- Recode(gss10$whtevan, "2=1; else=0")

gss12$whtbaprot <- gss12$white  + gss12$protestant  + gss12$bagain 
gss12$whtbaprot <- Recode(gss12$whtbaprot, "3=1; else=0")
gss12$whtevan <- gss12$white  + gss12$evangelical 
gss12$whtevan <- Recode(gss12$whtevan, "2=1; else=0")

gss14$whtbaprot <- gss14$white  + gss14$protestant  + gss14$bagain 
gss14$whtbaprot <- Recode(gss14$whtbaprot, "3=1; else=0")
gss14$whtevan <- gss14$white  + gss14$evangelical 
gss14$whtevan <- Recode(gss14$whtevan, "2=1; else=0")

gss16$whtbaprot <- gss16$white  + gss16$protestant  + gss16$bagain 
gss16$whtbaprot <- Recode(gss16$whtbaprot, "3=1; else=0")
gss16$whtevan <- gss16$white  + gss16$evangelical 
gss16$whtevan <- Recode(gss16$whtevan, "2=1; else=0")

gss10 %>% filter(whtbaprot ==1) %>% summarise(age = mean(age, na.rm = TRUE))
gss10 %>% filter(whtevan ==1) %>% summarise(age = mean(age, na.rm = TRUE))

gss12 %>% filter(whtbaprot ==1) %>% summarise(age = mean(age, na.rm = TRUE))
gss12 %>% filter(whtevan ==1) %>% summarise(age = mean(age, na.rm = TRUE))

gss14 %>% filter(whtbaprot ==1) %>% summarise(age = mean(age, na.rm = TRUE))
gss14 %>% filter(whtevan ==1) %>% summarise(age = mean(age, na.rm = TRUE))

gss16 %>% filter(whtbaprot ==1) %>% summarise(age = mean(age, na.rm = TRUE))
gss16 %>% filter(whtevan ==1) %>% summarise(age = mean(age, na.rm = TRUE))


gss10 %>% filter(whtbaprot ==1) %>% summarise(male = mean(male, na.rm = TRUE))
gss10 %>% filter(whtevan ==1) %>% summarise(male = mean(male, na.rm = TRUE))

gss12 %>% filter(whtbaprot ==1) %>% summarise(male = mean(male, na.rm = TRUE))
gss12 %>% filter(whtevan ==1) %>% summarise(male = mean(male, na.rm = TRUE))

gss14 %>% filter(whtbaprot ==1) %>% summarise(male = mean(male, na.rm = TRUE))
gss14 %>% filter(whtevan ==1) %>% summarise(male = mean(male, na.rm = TRUE))

gss16 %>% filter(whtbaprot ==1) %>% summarise(male = mean(male, na.rm = TRUE))
gss16 %>% filter(whtevan ==1) %>% summarise(male = mean(male, na.rm = TRUE))


cces08 %>% filter(whtbaprot ==1) %>% summarise(male = mean(male, na.rm = TRUE))
cces08 %>% filter(whtevan ==1) %>% summarise(male = mean(male, na.rm = TRUE))

cces12 %>% filter(whtbaprot ==1) %>% summarise(male = mean(male, na.rm = TRUE))
cces12 %>% filter(whtevan ==1) %>% summarise(male = mean(male, na.rm = TRUE))

cces16 %>% filter(whtbaprot ==1) %>% summarise(male = mean(male, na.rm = TRUE))
cces16 %>% filter(whtevan ==1) %>% summarise(male = mean(male, na.rm = TRUE))


cces08 %>% filter(whtbaprot ==1) %>% summarise(educ = mean(V213, na.rm = TRUE))
cces08 %>% filter(whtevan ==1) %>% summarise(educ = mean(V213, na.rm = TRUE))

cces12 %>% filter(whtbaprot ==1) %>% summarise(educ = mean(educ, na.rm = TRUE))
cces12 %>% filter(whtevan ==1) %>% summarise(educ = mean(educ, na.rm = TRUE))

gss10 %>% filter(whtbaprot ==1) %>% summarise(educ = mean(educ2, na.rm = TRUE))
gss10 %>% filter(whtevan ==1) %>% summarise(educ = mean(educ2, na.rm = TRUE))

gss12 %>% filter(whtbaprot ==1) %>% summarise(educ = mean(educ2, na.rm = TRUE))
gss12 %>% filter(whtevan ==1) %>% summarise(educ = mean(educ2, na.rm = TRUE))

gss14 %>% filter(whtbaprot ==1) %>% summarise(educ = mean(educ2, na.rm = TRUE))
gss14 %>% filter(whtevan ==1) %>% summarise(educ = mean(educ2, na.rm = TRUE))

gss16 %>% filter(whtbaprot ==1) %>% summarise(educ = mean(educ2, na.rm = TRUE))
gss16 %>% filter(whtevan ==1) %>% summarise(educ = mean(educ2, na.rm = TRUE))

gss10 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)
gss12 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)
gss14 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)
gss16 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)

cces08 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)
cces12 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)
cces16 %>% crosstab(whtbaprot, whtevan) %>% adorn_crosstab(denom = "all", show_totals = TRUE)