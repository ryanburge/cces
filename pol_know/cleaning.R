library(haven)
library(labelled)
library(building)
library(car)

cces16 <- read_dta("D://cces/data/cces16.dta")
source("D://measuring_evangelicals/reltrad16.R")



cces16 <- cces16 %>% 
  mutate(right_fed_house = recode(CC16_321a, "1=1; 2:4= 0; else =NA")) %>% 
  mutate(right_fed_sen = recode(CC16_321b, "1=1; 2:4= 0; else =NA"))


cces16 <- cces16 %>% 
  mutate(state = to_factor(inputstate))


sen_rep <- cces16 %>% 
  filter(state == "Alabama" | state == "Alaska" | state == "Arizona" | state == "Arkansas" | state == "Colorado" | state == "Florida" | state == "Georgia" | state == "Idaho" | state == "Indiana" | state == "Kansas" | state == "Kentucky" | state == "Louisiana" | state == "Maine"| state == "Michigan" | state == "Mississippi" |  state == "Missouri" | state == "Montana" | state == "Nevada" |  state == "New Hampshire" | state == "New York" | state == "North Carolina" | state == "North Dakota" | state == "Ohio" | state == "Oklahoma" | state == "Pennsylvania" | state == "South Carolina" | state == "South Dakota" | state == "Tennessee" | state == "Texas" | state == "Utah" | state == "Virginia" | state == "Washington" | state == "West Virginia" | state == "Wisconsin" | state == "Wyoming")


sen_dem <- cces16 %>% 
  filter(state != "Nebraska" & state != "Alabama" & state != "Alaska" & state != "Arkansas" &state != "Arizona" & state != "Colorado" & state != "Florida" & state != "Georgia" & state != "Idaho" & state != "Indiana" & state != "Kansas" & state != "Kentucky" & state != "Louisiana" & state != "Maine"& state != "Michigan" & state != "Mississippi" &  state != "Missouri" & state != "Montana" & state != "Nevada" &  state != "New Hampshire" & state != "New York" & state != "North Carolina" & state != "North Dakota" & state != "Ohio" & state != "Oklahoma" & state != "Pennsylvania" & state != "South Carolina" & state != "South Dakota" & state != "Tennessee" & state != "Texas" & state != "Utah" & state != "Virginia" & state != "Washington" & state != "West Virginia" & state != "Wisconsin" & state != "Wyoming")


sen_rep <- sen_rep %>% 
  mutate(right_sen = recode(CC16_321c, "1=1; 2:4= 0; else = NA"))

sen_dem <- sen_dem %>% 
  mutate(right_sen = recode(CC16_321c, "1=0; 2=1; 3:4= 0; else = NA"))


house_rep <- cces16 %>% 
  filter(state == "Alabama" | state == "Alaska" | state == "Arizona" | state == "Arkansas" | state == "Florida" | state == "Georgia" | state == "Idaho" | state == "Indiana" | state == "Iowa" | state == "Kansas" | state == "Kentucky" | state == "Louisiana" | state == "Maine"| state == "Michigan" | state == "Minnesota" | state == "Mississippi" |  state == "Missouri" | state == "Montana" | state == "Nevada" |  state == "New Hampshire" |  state == "New Mexico" | state == "New York" | state == "North Carolina" | state == "North Dakota" | state == "Ohio" | state == "Oklahoma" | state == "Pennsylvania" | state == "South Carolina" | state == "South Dakota" | state == "Tennessee" | state == "Texas" | state == "Utah" | state == "Virginia" | state == "Washington" | state == "West Virginia" | state == "Wisconsin" | state == "Wyoming")

house_dem <- cces16 %>% 
  filter(state != "Nebraska" & state != "Alabama" & state != "Alaska" & state != "Arkansas" &state != "Arizona" &  state != "Florida" & state != "Georgia" & state != "Idaho" & state != "Indiana" & state != "Iowa" & state != "Kansas" & state != "Kentucky" & state != "Louisiana" & state != "Maine"& state != "Michigan" & state != "Minnesota" & state != "Mississippi" &  state != "Missouri" & state != "Montana" & state != "Nevada" &  state != "New Hampshire" &  state != "New Mexico" & state != "New York" & state != "North Carolina" & state != "North Dakota" & state != "Ohio" & state != "Oklahoma" & state != "Pennsylvania" & state != "South Carolina" & state != "South Dakota" & state != "Tennessee" & state != "Texas" & state != "Utah" & state != "Virginia" & state != "Washington" & state != "West Virginia" & state != "Wisconsin" & state != "Wyoming")


house_rep <- house_rep %>% 
  mutate(right_house = recode(CC16_321d, "1=1; 2:4= 0; else = NA"))

house_dem <- house_dem %>% 
  mutate(right_house = recode(CC16_321d, "1=0; 2=1; 3:4= 0; else = NA"))


gov_rep <- cces16 %>% 
  filter(state == "Alabama" | state == "Arizona" | state == "Arkansas" | state == "Florida" |state == "Georgia" | state == "Idaho" | state == "Illinois" |state == "Indiana" | state == "Iowa" | state == "Kansas" | state == "Kentucky" | state == "Louisiana" | state == "Maine"| state == "Maryland" | state == "Massachusetts" |state == "Michigan" |  state == "Mississippi" |  state == "Nevada" |  state == "Nebraska" | state == "New Mexico" |state == "New Jersey" | state == "North Carolina" | state == "North Dakota" | state == "Ohio" | state == "Oklahoma" | state == "Pennsylvania" | state == "South Carolina" | state == "South Dakota" | state == "Tennessee" | state == "Texas" | state == "Utah" | state == "Vermont" |  state == "Wisconsin" | state == "Wyoming")


gov_dem <- cces16 %>% 
  filter(state != "Alabama" & state != "Alaska" & state != "Arizona" & state != "Arkansas" & state != "Florida" &state != "Georgia" & state != "Idaho" & state != "Illinois" &state != "Indiana" & state != "Iowa" & state != "Kansas" & state != "Kentucky" & state != "Louisiana" & state != "Maine"& state != "Maryland" & state != "Massachusetts" &state != "Michigan" &  state != "Mississippi" &  state != "Nevada" &  state != "Nebraska" & state != "New Mexico" &state != "New Jersey" & state != "North Carolina" & state != "North Dakota" & state != "Ohio" & state != "Oklahoma" & state != "Pennsylvania" & state != "South Carolina" & state != "South Dakota" & state != "Tennessee" & state != "Texas" & state != "Utah" & state != "Vermont" &  state != "Wisconsin" & state != "Wyoming")


gov_rep <- gov_rep %>% 
  mutate(right_gov = recode(CC16_322a, "2=1; 1=0; 3:4= 0; else = NA"))

gov_dem <- gov_dem %>% 
  mutate(right_gov = recode(CC16_322a, "3=1; 1:2=0; 4:5= 0; else = NA"))


sen_rep <- sen_rep %>% select(V101, right_sen)
sen_dem <- sen_dem %>% select(V101, right_sen)

sen <- bind_rows(sen_rep, sen_dem)

house_rep <- house_rep %>% select(V101, right_house)
house_dem <- house_dem %>% select(V101, right_house)

house <- bind_rows(house_rep, house_dem)

gov_rep <- gov_rep %>% select(V101, right_gov)
gov_dem <- gov_dem %>% select(V101, right_gov)

gov <- bind_rows(gov_rep, gov_dem)


join <- left_join(sen, house) %>% 
  left_join(gov)

join <- join %>% mutate(V101 = as.numeric(V101))
cces16 <- cces16 %>% mutate(V101 = as.numeric(V101))


cces16 <- left_join(cces16, join)

cces16 <- cces16 %>% 
  mutate(know = right_fed_house + right_fed_sen + right_house + right_sen + right_gov)