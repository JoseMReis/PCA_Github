################################################################################

### Author: J. M. Reis

### Date: 
(dateOfAccess <- Sys.Date())

#14-10-2017
### Purpose: Extracting and cleaning general case data and procedural transparency data for all disputes at the PCA

################################################################################

#### Load the relevant packages--------------------------------------------------------------

require(stringr)
require(rvest)
require(tidyverse)
require(lubridate)
require(RCurl)
require(xlsx)
require(countrycode)
require(magrittr)

#### Info on the PCA repository----------------------------------------------------------------------------

### Does it cover all cases?

# The PCA Case Repository contains a full listing of cases commenced since 1996 in respect of which the parties have agreed to publication. In addition, the PCA continues to make information about further public cases since 1902 available on the PCA Case Repository. @https://www.pcacases.com/web/allcases/.

#########################################################################

                    # Pulling General Case Data #

#########################################################################
# Pulling General Case Data------------------------------------------------

#### Getting the case id's

### Extraction strategy. At the website which lists all the cases ("https://pca-cpa.org/en/cases/"), the HTML code reveals that the id numbers for each code are stored as href links. Parse the apge and use xpath to extract the id's.

id_raw <- RCurl::getURL("https://pca-cpa.org/en/cases/") %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attrs()
## find the non id's
not_id <- id_raw %>%
  str_detect(pattern = "\\d{1,3}")
## all the sublists > 18 and <139
id_raw <- id_raw[c(18:139)]
## remove the href and "/" and assign it to a vector
id_raw <- str_extract(id_raw, pattern="\\d{1,3}")

#### create a data frame and add id

PCA_genCaseData <- data.frame (id = id_raw)

#### titles: get, clean, and add to the data frame

### For the titles I will use another, PCA sponsored, website. Its case database: https://www.pcacases.com/web/allcases/. Why? less java injected, and thus easier to scrape.

title_raw <- RCurl::getURL("https://www.pcacases.com/web/allcases/") %>%
  read_html() %>%
  html_nodes(css = ".allCasesItems") %>%
  html_text()

### clean it
title <- title_raw %>%
  str_extract("\\:(.*)") %>%# extract substrings after:
  str_replace("\\:", "") %>%# remove the ":"
  str_trim()

#### Add it to the data frame as a new variable.

### "title". Variable which represents the title of the case as adopted by the PCA. New var name------ > "title"

PCA_genCaseData <- PCA_genCaseData %>%
  mutate(title = title)

#### "complain", "resp", "status","dateOfCommencement","dateOfConclusion", "typeOfCase", "adopted_procRules", "PCA_subj", and "treaty/Contract. Extract, clean and add to the dataset

### Information on the complainant, respondant etc. can be found at the case specific pages from the https://www.pcacases.com/web/view/. This is the static part of the URL. To access to the case specific pages we just have to add the case id and some other components "/view/149". So this website would be https://www.pcacases.com/web/view/149.

### The loop

## Set some things for the loop

# URL
staticURL <- "https://www.pcacases.com/web/view/"
dynURL <- PCA_genCaseData$id

# css selectors
cssComplain <- "tr:nth-child(3) .ViewCaseTableTdValue"
cssResp <- "tr:nth-child(4) .ViewCaseTableTdValue"
cssStatus <- "tr:nth-child(8) .ViewCaseTableTdValue"
cssDateCom <- "tr:nth-child(20) .ViewCaseTableTdValue"
cssDateConc <- "tr:nth-child(21) .ViewCaseTableTdValue"
cssType <- "tr:nth-child(9) .ViewCaseTableTdValue"
cssProcRules <- "tr:nth-child(11) .ViewCaseTableTdValue"
cssSubj <- "tr:nth-child(10) .ViewCaseTableTdValue"
cssTreaty <- "tr:nth-child(12) .ViewCaseTableTdValue"

# containers
complain_container <- c(rep(NA, 122))
resp_container <- c(rep(NA, 122))
status_container <- c(rep(NA, 122))
dateCom_container <- c(rep(NA, 122))
dateConc_container <- c(rep(NA,122))
type_container <- c(rep(NA, 122))
procRules_container <- c(rep(NA, 122))
subj_container <- c(rep(NA,122))
treaty_container <- c(rep(NA,122))
# for interm_DF and inspecting the quality of the data
id_container <- c(rep(NA, 122))
URL_container <- c(rep(NA, 122))

#regex for cleaning
uglyPat <- c("\\\n", "\\\t", "\\s+\\\t", "\\s+\\\n", "\\\n\\\t", "\\s+")

### start the loop

for (i in 1:nrow(PCA_genCaseData)) {
  
  ## *** store the interm containers for debugging ***
  id_container[i] <- dynURL[i]
  URL_container[i] <- paste0(staticURL, dynURL[i])
  
  ## *** complainant***
    x <- RCurl::getURL(paste0(staticURL, dynURL[i])) %>%
    read_html() %>%
    html_node(cssComplain) %>%
    html_text() %>%
    str_replace_all(uglyPat, "") %>% # remove ugly encoding
    str_trim()
  
  # deal with empty strings
  ifelse(nchar(x)<1 | length(x)==0,complain_container[i] <- NA, complain_container[i] <- x)
  
  print(paste0("For the case with id: ", dynURL[i], ". The complainant was ", complain_container[i]))
  
  ##*** respondant ***
    x <- RCurl::getURL(paste0(staticURL, dynURL[i])) %>%
    read_html() %>%
    html_node(css = cssResp) %>%
    html_text() %>%
    str_replace_all(uglyPat, "") %>%# idem
    str_trim()
  
  # deal with empty strings
  ifelse(nchar(x)<1 | length(x)==0,resp_container[i] <- NA, resp_container[i] <- x)
  
  print(paste0("For the case with id: ", dynURL[i], ". The respondant was ", resp_container[i]))
  
  ## *** status data ***
  
    x <- RCurl::getURL(paste0(staticURL, dynURL[i])) %>% 
    read_html() %>% 
    html_nodes(cssStatus) %>% 
    html_text() %>% 
    str_replace_all(uglyPat, replacement="")%>%
    str_trim()
  
  # conditional assignment for the empty nodes (no info on status)
  ifelse(nchar(x) < 1| length(x)==0, status_container[i] <- NA, status_container[i] <- x) # empty websites would not get added to the container otherwise
  
  print(paste0("The case id: ", dynURL[i], " gets status: ", status_container[i]))
  
  ## *** dateCom ***
    x <- RCurl::getURL(paste0(staticURL, dynURL[i])) %>% 
    read_html() %>% 
    html_nodes(cssDateCom) %>% 
    html_text()%>% 
    str_replace_all(uglyPat, replacement="")%>%
    str_trim()
    
  # test if character(0)
  ifelse(nchar(x) < 1 | length(x)==0, dateCom_container[i] <- NA, dateCom_container[i] <- x)
  
  print(paste0("The case id: ", dynURL[i], " gets dateCom: ", dateCom_container[i]))
  
  ## *** dateConc***
    x<- RCurl::getURL(paste0(staticURL, dynURL[i]))%>% 
    read_html() %>% 
    html_nodes(cssDateConc) %>% 
    html_text()%>% 
    str_replace_all(uglyPat, replacement="")%>%
    str_trim()
  
  # test if character(0)
  ifelse(nchar(x) < 1 | length(x)==0, dateConc_container[i] <- NA, dateConc_container[i] <- x)

  print(paste0("The case id: ", dynURL[i], " gets dateConc: ", dateConc_container[i]))
  
  ## *** type of dispute **
    x<- RCurl::getURL(paste0(staticURL, dynURL[i]))%>% 
    read_html() %>% 
    html_nodes(cssType) %>% 
    html_text()%>%
    str_replace_all(uglyPat, replacement = "")%>%
    str_trim()
  
  # test if character(0)
  ifelse(nchar(x) < 1 | length(x)==0, type_container[i] <- NA, type_container[i] <- x)
  
  print(paste0("The case id: ", dynURL[i], " gets type of disp.: ", type_container[i]))
  
  ## *** Adopted procedural rules ***
    x <- RCurl::getURL(paste0(staticURL, dynURL[i]))%>%
    read_html() %>% 
    html_nodes(cssProcRules) %>% 
    html_text()%>%
    str_replace_all(uglyPat, replacement = "")%>%
    str_trim()
  
  # test if character(0)
  ifelse(nchar(x) < 1 | length(x)==0, procRules_container[i]<- NA, procRules_container[i] <- x)
  
  print(paste0("The case id: ", dynURL[i], " gets proc. rule: ", procRules_container[i]))
  
  ## *** subj ***
    x <- RCurl::getURL(paste0(staticURL, dynURL[i]))%>%
    read_html() %>% 
    html_nodes(cssSubj) %>% 
    html_text()%>%
    str_replace_all(uglyPat, replacement = "")%>%
    str_trim()
  
  # test if character(0)
  ifelse(nchar(x) < 1 | length(x)==0, subj_container[i]<- NA, subj_container[i] <- x)
  
  print(paste0("The case id: ", dynURL[i], " gets subj.: ", subj_container[i]))
  
  ## *** treaty ***
    x <- RCurl::getURL(paste0(staticURL, dynURL[i]))%>%
    read_html() %>% 
    html_nodes(cssTreaty) %>% 
    html_text()%>%
    str_replace_all(uglyPat, replacement = "")%>%
      gsub(pattern = "\n", replacement="", fixed=TRUE) %>% # remove the messy encoding
      gsub(pattern = "\t", replacement="", fixed=TRUE) %>%
      gsub(pattern = "\\s+", replacement = " ") %>% # large space between category of treaty and treaty
      str_trim()
  
  # test if character(0)
  ifelse(nchar(x) < 1 | length(x)==0, treaty_container[i]<- NA, treaty_container[i] <- x)
  
  print(paste0("The case id: ", dynURL[i], " gets treaty: ", treaty_container[i]))
  
  Sys.sleep(runif(3,2,4)) # for the server
  
}

### Assign the results to different objects
complain <- complain_container
resp <- resp_container
status <- status_container
dateCom <- dateCom_container
dateConc <- dateConc_container
typeOfDisp <- type_container
procRules <- procRules_container
subj <- subj_container
invInstrument <- treaty_container

### add the vectors as new variables for the data frame
PCA_genCaseData <- PCA_genCaseData%>%
  mutate(complain = complain,
         resp = resp,
         status = status,
         dateCom = dateCom,
         dateConc = dateConc,
         typeOfDisp = typeOfDisp,
         procRules = procRules,
         subj = subj,
         invInstrument = invInstrument,
         dateOfAccess = dateOfAccess)


### export it 
# store the extraction results as a separate data frame
save(PCA_genCaseData,
     file="data/PCA_genCaseData_OriginalExt.RData")
write.csv(PCA_genCaseData,
          file="data/PCA_genCaseData_OriginalExt.RData")

#### Build an intermediate dataset for checking for extraction errors

interm_genCaseData <-tibble(id = PCA_genCaseData[,1],
                            complain = complain,
                            resp = resp,
                            status = status,
                            dateCom = dateCom,
                            dateConc = dateConc,
                            typeOfDisp = typeOfDisp,
                            procRules = procRules,
                            subj = subj,
                            invInstrument = invInstrument,
                            source = URL_container)

## export it
save(interm_genCaseData,
     file="data/interm_df/interm_genCaseData.RData")

# as a spreadsheet...for further manual checking of NAs.
write.xlsx(interm_genCaseData,
           file = "data/interm_df/interm_genCaseData.xlsx",
           "Sheet1",
           col.names = TRUE)

#########################################################################

# Handcoding missing observations, some formatting and adding new variables (general case data)#

#########################################################################
#### Handcoding missing observations, some formatting and adding new variables (general case data) -----------------------------------------------------

### load the data
load("data/PCA_genCaseData_OriginalExt.RData")

### Turning dates into date class 
PCA_genCaseData <- PCA_genCaseData%>% 
  mutate(dateCom = dmy(dateCom),
         dateConc = dmy(dateConc))

### Generating yearCom and yearConc variables
PCA_genCaseData <- PCA_genCaseData%>% 
  mutate(yearCom = year(dateCom),
         yearConc = year(dateConc))

### Handcoding the missing observations in the complain and resp variables in PCA_genCaseData
##handcode based on inter_df
PCA_genCaseData$complain[PCA_genCaseData$id==117] <- "Italy"
PCA_genCaseData$resp[PCA_genCaseData$id==117] <- "India"
PCA_genCaseData$complain[PCA_genCaseData$id==132] <- "The Democratic Republic of Timor-Leste"
PCA_genCaseData$resp[PCA_genCaseData$id==132] <- "The Commonwealth of Australia"
PCA_genCaseData$complain[PCA_genCaseData$id==148] <- "Not Available"# page is closed for editing
PCA_genCaseData$resp[PCA_genCaseData$id==148] <- "Not Available"
PCA_genCaseData$complain[PCA_genCaseData$id==54] <- "South American Silver Limited (Bermuda)"
PCA_genCaseData$resp[PCA_genCaseData$id==54] <- "The Plurinational State of Bolivia"
PCA_genCaseData$complain[PCA_genCaseData$id==33] <- "Not Available"
PCA_genCaseData$resp[PCA_genCaseData$id==33] <- "Not Available"
PCA_genCaseData$complain[PCA_genCaseData$id==112] <- "Peter A. Allard (Canada)" 
PCA_genCaseData$resp[PCA_genCaseData$id==112] <- "The Government of Barbados"
PCA_genCaseData$complain[PCA_genCaseData$id==3] <- "Republic of Croatia"
PCA_genCaseData$resp[PCA_genCaseData$id==3] <- "Republic of Slovenia"
PCA_genCaseData$complain[PCA_genCaseData$id==56] <- "Malaysia"
PCA_genCaseData$resp[PCA_genCaseData$id==56] <- "Republic of Singapore"
PCA_genCaseData$complain[PCA_genCaseData$id==92] <- "The Government of Sudan"
PCA_genCaseData$resp[PCA_genCaseData$id==92] <- "The Sudan People's Liberation Movement/Army"
PCA_genCaseData$complain[PCA_genCaseData$id==1] <- "The Kingdom of Belgium ( State )"
PCA_genCaseData$resp[PCA_genCaseData$id==1] <- "The Kingdom of the Netherlands ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==71] <- "State of Eritrea ( State )"
PCA_genCaseData$resp[PCA_genCaseData$id==71] <- "Federal Democratic Republic of Ethiopia ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==99] <- "State of Eritrea ( State )"
PCA_genCaseData$resp[PCA_genCaseData$id==99] <- "Federal Democratic Republic of Ethiopia ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==88] <- "Norway ( State )"
PCA_genCaseData$resp[PCA_genCaseData$id==88] <- "United States of America ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==87] <- "The Netherlands ( State )"
PCA_genCaseData$resp[PCA_genCaseData$id==87] <- "Portugal ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==102] <- "France ( State )"
PCA_genCaseData$resp[PCA_genCaseData$id==102] <- "Italy ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==68] <- "France ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==68] <- "Italy ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==79] <- "France ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==79] <- "Great Britain ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==89] <- "Russia ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==89] <- "Turkey ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==80] <- "Italy ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==80] <- "Peru ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==78] <- "United States of America ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==78] <- "Venezuela ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==74] <- "Great Britain ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==74] <- "United States of America ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==67] <- "France ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==67] <- "Germany ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==77] <- "Norway ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==77] <- "Sweden ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==93] <- "France ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==93] <- "Great Britain ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==69] <- "Germany ( State ), France ( State ), Great Britain ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==69] <- "Japan ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==75] <- "The United Mexican States ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==75] <- "The United States of America ( State )"
PCA_genCaseData$complain[PCA_genCaseData$id==31] <- "The Netherlands ( State )" 
PCA_genCaseData$resp[PCA_genCaseData$id==31] <- "France ( State )" 

# inspect it
PCA_genCaseData %>% 
  filter(is.na(complain)==T & is.na(resp)==T) # all good, no more NAs

### Generating variable identifying type of party 

## Add to the dataset as new variables
PCA_genCaseData <- PCA_genCaseData %>%
  mutate(typeOfComplain = rep(NA, nrow(PCA_genCaseData), 
                              typeOfResp = NA, nrow(PCA_genCaseData)))
## conditional assignment based on the state, private... part of the string complain and resp. We can use a nested ifelse.

PCA_genCaseData <- PCA_genCaseData%>% 
  mutate(typeOfComplain = 
           ifelse(str_detect(complain, "( State )")==T, "State", 
                  ifelse(str_detect(complain, "( Private entity )")==T, "Private Entity", 
                         ifelse(str_detect(complain, "( - Other - )")==T, "Other",
                                ifelse(str_detect(complain, "( International organization )")==T, "IO", NA)))),
         typeOfResp = 
           ifelse(str_detect(resp, "( State )")==T, "State", 
                  ifelse(str_detect(resp, "( Private entity )")==T, "Private Entity", 
                         ifelse(str_detect(resp, "( - Other - )")==T, "Other",
                                ifelse(str_detect(resp, "( International organization )")==T, "IO", NA)))))

### Remove the brackets and content within from the complain and resp observations
PCA_genCaseData <- PCA_genCaseData%>% 
  mutate(complain = gsub(complain, pattern = "\\((.*?)\\)", replacement = ""), 
         resp = gsub(resp, pattern = "\\((.*?)\\)", replacement = ""))

### convert the states to Correlates of war country code
complain_cowc <- countrycode(PCA_genCaseData$complain,"country.name", "cowc", warn = TRUE)
resp_cowc <- countrycode(PCA_genCaseData$resp,"country.name", "cowc", warn = TRUE) 
## add to the dataset complain_cowc
PCA_genCaseData  <- PCA_genCaseData %>% 
  mutate(complain_cowc = complain_cowc) %>%
  mutate(complain_cowc = replace(complain_cowc, which(typeOfComplain != "State"), NA))%>%# only states receive a country code
  mutate(resp_cowc = resp_cowc)%>%
  mutate(complain_cowc = replace(complain_cowc, which(typeOfComplain != "State"), NA))# only states receive a country code

### Handcoding missing country codes
PCA_genCaseData <- PCA_genCaseData %>%
  mutate(typeOfResp = replace(typeOfResp, which(resp == "Italy"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "Italy"), "ITA"),
         typeOfComplain = replace(typeOfComplain, which(complain == "Italy"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="Italy"), "ITA"))

PCA_genCaseData <- PCA_genCaseData %>%  
  mutate(typeOfResp = replace(typeOfResp, which(resp == "India"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "India"), "IND"),
         typeOfComplain = replace(typeOfComplain, which(complain == "India"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="India"), "IND"))

PCA_genCaseData <- PCA_genCaseData %>%  
  mutate(typeOfResp = replace(typeOfResp, which(resp == "The Democratic Republic of Timor-Leste"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "The Democratic Republic of Timor-Leste"), "ETM"),
         typeOfComplain = replace(typeOfComplain, which(complain == "The Democratic Republic of Timor-Leste"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="The Democratic Republic of Timor-Leste"), "ETM"))

PCA_genCaseData <- PCA_genCaseData %>%  
  mutate(typeOfResp = replace(typeOfResp, which(resp == "The Kingdom of Denmark in respect of the Faroe Islands"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "The Kingdom of Denmark in respect of the Faroe Islands"), "DEN"),
         typeOfComplain = replace(typeOfComplain, which(complain == "The Kingdom of Denmark in respect of the Faroe Islands"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="The Kingdom of Denmark in respect of the Faroe Islands"), "DEN"))

PCA_genCaseData <- PCA_genCaseData %>% 
  mutate(typeOfResp = replace(typeOfResp, which(resp == "Republic of Croatia"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "Republic of Croatia"), "CRO"),
         typeOfComplain = replace(typeOfComplain, which(complain == "Republic of Croatia"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="Republic of Croatia"), "CRO"))

PCA_genCaseData <- PCA_genCaseData %>%  
  mutate(typeOfResp = replace(typeOfResp, which(resp == "The Slovak Republic"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "The Slovak Republic"), "SLO"),
         typeOfComplain = replace(typeOfComplain, which(complain == "The Slovak Republic"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="The Slovak Republic"), "SLO"))

PCA_genCaseData <- PCA_genCaseData %>%  
  mutate(typeOfResp = replace(typeOfResp, which(resp == "Republic of Singapore"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "Republic of Singapore"), "SIN"),
         typeOfComplain = replace(typeOfComplain, which(complain == "Republic of Singapore"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="Republic of Singapore"), "SIN"))

PCA_genCaseData <- PCA_genCaseData %>% 
  mutate(typeOfResp = replace(typeOfResp, which(resp == "Malaysia"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "Malaysia"), "MAL"),
         typeOfComplain = replace(typeOfComplain, which(complain == "Malaysia"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="Malaysia"), "MAL"))


PCA_genCaseData <- PCA_genCaseData %>%  
  mutate(typeOfResp = replace(typeOfResp, which(id == 91), "Other"))


PCA_genCaseData <- PCA_genCaseData %>%  
  mutate(typeOfResp = replace(typeOfResp, which(resp == "Republic of Slovenia"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "Republic of Slovenia"), "SLV"),
         typeOfComplain = replace(typeOfComplain, which(complain == "Republic of Slovenia"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="Republic of Slovenia"), "SLV"))

PCA_genCaseData <- PCA_genCaseData %>%  
  mutate(typeOfResp = replace(typeOfResp, which(resp == "The Government of Barbados"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "The Government of Barbados"), "BAR"),
         typeOfComplain = replace(typeOfComplain, which(complain == "The Government of Barbados"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="The Government of Barbados"), "BAR"))

PCA_genCaseData <- PCA_genCaseData %>% 
  mutate(typeOfResp = replace(typeOfResp, which(str_detect(resp, "Serbia")), "State"),
         resp_cowc = replace(resp_cowc, which(str_detect(resp, "Serbia")), "SRB"),
         typeOfComplain = replace(typeOfComplain, which(str_detect(complain, "Serbia")), "State"),
         complain_cowc = replace(complain_cowc, which(str_detect(complain, "Serbia")), "SRB"))  


PCA_genCaseData <- PCA_genCaseData %>% 
  mutate(typeOfResp = replace(typeOfResp, which(resp == "The Commonwealth of Australia"), "State"),
         resp_cowc = replace(resp_cowc, which(resp == "The Commonwealth of Australia"), "AUL"),
         typeOfComplain = replace(typeOfComplain, which(complain == "The Commonwealth of Australia"), "State"),
         complain_cowc = replace(complain_cowc, which(complain =="The Commonwealth of Australia"), "AUL"))

# inspect it
PCA_genCaseData%>% filter(typeOfComplain=="State" & is.na(complain_cowc)==T)%>% 
  select(id, complain) # it is basically the multi country cases! Repeat this step later after the tidying up!!!

PCA_genCaseData%>% filter(typeOfResp=="State" & is.na(resp_cowc)==T)%>% 
  select(id, resp, typeOfResp) # the resp at id: 116 and 82 are not states. Correct it.

### Correcting typeOfResp for id: 116, 82, 35
PCA_genCaseData <- PCA_genCaseData%>% 
  mutate(typeOfResp = replace(typeOfResp, which(id == c(116, 82)), "Private Entity"))

## export it
save(PCA_genCaseData,
     file="data/PCA_genCaseData_v2.RData")
write.csv(PCA_genCaseData, 
          file="data/PCA_genCaseData_v2.csv")


#########################################################################

                  # Pulling Procedural Transparency Data #

#########################################################################
#Pulling Procedural Transparency Data -----------------------------------

#### General information on the variables
## List of relevant procedural transparency variables.
#This list is based not only on the practice of other courts but also on the practices in this very same institution (e.g. "https://www.pcacases.com/web/view/72"). It tells us what was possible, and what the parties opted to disclose.

## "NoteOfSubmission": dummy variable indicating whether or not the "notice of submission" was published in the repository. *** DONE ***

## "writtenSubmission": dummy variable indicating whether or not written submissions made by the parties were published in the repository.Written submissions are crucial parts of the dispute. Still, you must control for the possibility that they never reached that level. This could be done by checking (i) length of dispute (dateCom - dateConc) and (ii) if a decision was reched. Later, inspect the procedural orders.  *** DONE ***

## "WrittenSub_count": count value of all the written submissions published by at the repository. The remarks above also apply. Furthermore, and this is crucial, beware of the possibility of translations!!! Either (i) extract only one language for all cases or (ii) use regex to extract identifiers of translations (e.g. (spanish) or (russian)). *** still missing!!! ***

## "proceduralOrder": dummy variable indicating whether or not any procedural order was published. *** DONE ***

## "AA_Action": dummy variable indicating whether or not any action by the appointing authority was published. Beaware of the possibility that there was no AA in that dispute! control for it. *** still missing!!! ***

## "Apointment of arbitrors": dummy variable indicating whether or not any notices of the appointment of arbitrors was published. Until know, I have only found this as a title of the node "others", if so, use regex for extracting it from the href of the documents under others. *** still missing!!! ***

## "Audiovisual": dummy variable indicating whether or not any notices of the appointment of arbitrors was published. The relevant data is usually found under two separate nodes: (i) audio and (ii) visual. During the extraction put them together. *** DONE! ***

## "Audiovisual_count": count variable indicating the sum of published documents under the heading Audio or visual. The idea is that, after controling for the issue mentioned above such as discontinuaty, it may be an interesting measure of how much of the proceedings were recorded. Can even subtract the number of hearings from the recordings. Should we add image??? *** still missing!!! ***

## "PubHear": dummy variable indicating whether or not any of the hearings were held publicly. Same warnign about discontinuaty apply. The problem with this variable, is that it does not seem to appear in any of the main documental nodes in the main nodes of the case pages. Might have to do it by hand. *** still missing!!! ***

## "Transcripts": dummy variable indicating whether or not any of the hearings, statements of the parties, arbitros etc. were made into a transcript/minute. *** DONE!! ***

## "transcript_count": count the number of transcripts that were made public by case.#            *** still missing!!! ***

## "awardOrdecision": dummy variable indicating whether or not the decision/award was made public. It covers not only awards bu also "other decisions" Here it is crucial that you control for discontinued cases!!! ********** DONE ! ****************

## "pressRelease": dummy variable indicating whether or not any press releases were made public. *** Done! ***

## "pressReleas_count": count variable of the number of press releases by case. Control for translation!!!*** still missing!!! ***

#### Load the "data/PCA_genCaseData_v2.RData
data <- "data/PCA_genCaseData_v2.RData"
load(data)

### Strategy. Loop aroung all case pages and extract all the available publication headers and links, assign them to different containers. Afterwards, pass it to objects. Then use it for making the variables. See below.

### set the URLs
staticURL <- "https://www.pcacases.com/web/view/"
dynURL <- PCA_genCaseDatav3$id

### CSS Selector for the main type of document nodes
cssTypeDoc <- "b"
cssLinks <- ".allCasesItems"

### containers
doc_container <- list(id = list(header=list(links=list())))
URL_container <- c(rep(NA, 122))

### regex
uglyPat <- c("\\\n", "\\\t", "\\s+")

#### The loop

for (i in seq_len(nrow(PCA_genCaseData))) {
  
  ## ** interm df container**
  URL_container[i] <- paste0(staticURL, dynURL[i])
  
  ## ** get the id **
  doc_container$id[[i]] <- dynURL[i]
  
  ## ** get the document headers**
  doc_container$header[[i]] <- getURL(paste0(staticURL, dynURL[i])) %>%
    read_html() %>%
    html_nodes(cssTypeDoc) %>%
    html_text()
  
  ifelse(identical(doc_container$header[[i]], character(0))==T,doc_container$header[[i]] <- "empty? check", doc_container$header[[i]] <- doc_container$header[[i]] )
  
  print(paste0("The case with the id : ", doc_container$id[[i]], " had the following doc. header : ", doc_container$header[[i]]))
  
  ## ** get the links ##
  
  doc_container$links[[i]] <-getURL(paste0(staticURL, dynURL[i])) %>%
    read_html() %>%
    html_nodes(cssLinks) %>%
    html_text() %>%
    gsub(pattern = paste(uglyPat, collapse="|"), replacement = " ")%>% # remove large space between begg and end
    str_trim()
  
  ifelse(identical(doc_container$header[[i]], character(0))==T,doc_container$links[[i]] <- "empty? check", doc_container$links[[i]] <- doc_container$links[[i]] )
  
  print(paste0("The case with the id : ", doc_container$id[[i]], " had the following docs : ", doc_container$links[[i]]))
  
  Sys.sleep(runif(2,1,3)) # for the server
}

### Assign output to objects
allHeaders <- doc_container[[2]]

allHeadersAndLinks <- doc_container[[3]]

### save them as R objects

save(allHeaders,
     file = "data/interm_df/allHeaders.RData")
save(allHeadersAndLinks,
     file = "data/interm_df/allHeadersAndLinks.RData")

### Get the unique document type headers
uniqueHeaders <- unique(unlist(allHeaders))
uniqueHeaders <- uniqueHeaders[-10]# removes the category for empty.
## save as objet 
save(uniqueHeaders,
     file = "data/interm_df/uniqueHeaders.RData")

### Create an interm_df for handcoding missing observations and inpecting extracted content
### create an interm df
pastedHeader <- c(rep(NA, 122))
pastedDocs <- c(rep(NA, 122))
# paste together the titles of the doc headers and docs for each case page
for (i in 1:122) {
  
  pastedHeader[i] <- paste(doc_container$header[[i]], collapse = " || !NEW HEADER! || ")
  pastedDocs[i] <- paste(doc_container$links[[i]], collapse = " || !NEW DOC! || ")
  
}

intermDF_procTransp <- data.frame(id = unlist(doc_container$id),
                                  headers = pastedHeader,
                                  docs = pastedDocs,
                                  source = URL_container)
### Export
save(intermDF_procTransp,
     file = "data/interm_df/intermDF_procTransp.RData")
write.xlsx(intermDF_procTransp,
           file = "data/interm_df/intermDF_procTransp.xlsx",
           col.names = TRUE)

#### Code the procedural transparency variables

# strategy for coding the dummy variables. If there there is a string match between on ofthe relevant categories (see unique docTitles) assign 1 to the relevant variable, else assign 0.

### Create the working dataframe
PCA_procTransp <- PCA_genCaseData

#### "AwardOrdecision": getting, cleaning and adding as a variable

### loop over the cases find which have a doc type header == "Award or other decision". If yes, assign 1 (meaning it published a award or decision), if no assign 0 (meaning no award or decision were published).

### step 1: create a pattern for awards and decisions

# pull all the available categories
uniqueHeaders

#choose the one for awards and decisions
patAward <- "Award or other decision"
patNotSub <- "Notice of Arbitration"
patWritSub <- "Written submission"
patProcOrder <- "Procedural Order"
patAudioVis <- c("Video", "Audio")#As stated above, this variable covers both (i) the publication of videos of the hearings as well as (ii) audio. Thus, we have to create two patterns.
patPressR <- "Press Release"
patTranscript <- "Transcript/Minutes" 


### step 2: create a container object for the variable

### Create the procedural transparency variables
PCA_procTransp <- PCA_procTransp%>%
  mutate(awardOrDecision = rep(NA, nrow(PCA_procTransp)),
         notOfSubmission = rep(NA, nrow(PCA_procTransp)),
         writtenSubmission = rep(NA, nrow(PCA_procTransp)),
         procOrder = rep(NA, nrow(PCA_procTransp)),
         audioVisual = rep(NA, nrow(PCA_procTransp)),
         pressRelease =rep(NA, nrow(PCA_procTransp)),
         transcript =rep(NA, nrow(PCA_procTransp)))

#### The loop

for (i in seq_len(nrow(PCA_procTranspv1))) {
  
  ## test the existance of any award
  if (any(str_detect(allHeaders[[i]], patAward))==TRUE) {
    
    # The award was published...
    PCA_procTransp$awardOrDecision[i] <- 1 # get 1
    
    print(paste0(" The case in row, ", i, ", published an award ||| gets: ",PCA_procTransp$awardOrDecision[i]))
    
  } else {
    
    PCA_procTransp$awardOrDecision[i] <- 0
    print(paste0(" The case in row, ", i, ", published no award||| gets: ", PCA_procTransp$awardOrDecision[i]))
    
  }
  ## test the existance of any notice of submission
  if (any(str_detect(allHeaders[[i]], patNotSub))==TRUE) {
    
    # There is a match
    PCA_procTransp$notOfSubmission[i] <- 1 # get 1
    
    print(paste0(" The case in row, ", i, ", published a note of submission ||| gets: ",PCA_procTransp$notOfSubmission[i]))
    
  } else {
    
    PCA_procTransp$notOfSubmission[i] <- 0
    print(paste0(" The case in row, ", i, ", published no not. of submission. ||| gets: ", PCA_procTransp$notOfSubmission[i]))
    
  }
  
  ## test the existance of any published written submissions
  if (any(str_detect(allHeaders[[i]], patWritSub))==TRUE) {
    
    # There is a match
    PCA_procTransp$writtenSubmission[i] <- 1 # get 1
    
    print(paste0(" The case in row, ", i, ", published a note of submission ||| gets: ",PCA_procTransp$notOfSubmission[i]))
    
  } else {
    
    PCA_procTransp$writtenSubmission[i] <- 0
    print(paste0(" The case in row, ", i, ", published no written submission ||| gets: ", PCA_procTransp$writtenSubmission[i]))
    
  }
  
  ## test the existance of any published prcedural orders
  if (any(str_detect(allHeaders[[i]], patProcOrder))==TRUE) {
    
    # There is a match
    PCA_procTransp$procOrder[i] <- 1 # get 1
    
    print(paste0(" The case in row, ", i, ", published proced. orders ||| gets: ",PCA_procTransp$procOrder[i]))
    
  } else {
    
    PCA_procTransp$procOrder[i] <- 0
    print(paste0(" The case in row, ", i, ", published no proced. orders ||| gets: ", PCA_procTransp$procOrder[i]))
    
  }
  
  ## test the existance of any published audiovisuals
  if (any(str_detect(allHeaders[[i]], paste(patAudioVis, collapse = "|")))==TRUE) {
    
    # There is a match
    PCA_procTransp$audioVisual[i] <- 1 # get 1
    
    print(paste0(" The case in row, ", i, ", published audiovisuals ||| gets: ",PCA_procTransp$audioVisual[i]))
    
  } else {
    
    PCA_procTransp$audioVisual[i] <- 0
    print(paste0(" The case in row, ", i, ", published no audiovisuals ||| gets: ", PCA_procTransp$audioVisual[i]))
    
  }
  
  ## test the existance of any press releases
  if (any(str_detect(allHeaders[[i]], patPressR)==TRUE)) {
    
    # There is a match
    PCA_procTransp$pressRelease[i] <- 1 # gets 1
    
    print(paste0(" The case in row, ", i, ", published press r. ||| gets: ",PCA_procTransp$pressRelease[i]))
    
  } else {
    
    PCA_procTransp$pressRelease[i] <- 0
    print(paste0(" The case in row, ", i, ", published no press r. ||| gets: ", PCA_procTransp$pressRelease[i]))
    
  }
  
  ## test the existance of transcripts
  if (any(str_detect(allHeaders[[i]], patTranscript)==TRUE)) {
    
    # There is a match
    PCA_procTransp$transcript[i] <- 1 # gets 1
    
    print(paste0(" The case in row, ", i, ", published transcripts ||| gets: ",PCA_procTransp$transcript[i]))
    
  } else {
    
    PCA_procTransp$transcript[i] <- 0
    
    print(paste0(" The case in row, ", i, ", published no transcripts. ||| gets: ", PCA_procTransp$transcript[i]))
    
  }
  
}

### Add date of access2

PCA_procTransp <- PCA_procTransp%>%
  mutate(dateOfAccess2 = rep(Sys.Date(), nrow(PCA_procTransp)))

### Export it
save(PCA_procTransp,
     file = "data/PCA_procTranspv1.RData")

write.csv(PCA_procTransp,
          file = "data/PCA_procTranspv1.csv")

#########################################################################

   # Handcoding missing observations, some formatting and adding new variables (Proc transp)#

#########################################################################
#### Handcoding missing observations, some formatting and adding new variables (proc. transp) -----------------------------------------------------

# load PCA_ProcTranspv1
load("data/PCA_procTranspv1.RData")

### Pending cases and awardOrDecision

#If a case is still pending, then we should not extrapolate anything from the absence of publication of award. Set all the observations which still have pending to NA.

PCA_procTransp <- PCA_procTransp %>%
  mutate(awardOrDecision  = replace(awardOrDecision , which(status=="Pending"), NA))

# inspect it
PCA_procTransp %>%
  filter(status == "Pending" & is.na(awardOrDecision)==FALSE)# All good here.

# export it
save(PCA_procTransp,
     file="data/PCA_procTranspv2.RData")

write.csv(PCA_procTransp,
  file="data/PCA_procTranspv2.csv")

#####################################################################

                              # Tidying up #

#####################################################################
# Tidying up-----------------------------------------------------

# load the latest dataset
load("data/PCA_procTranspv2.RData")

#### Replicate rows which contain multiple parties - one row per country unit.

# ## Find the rows with multiple state parties
 multiComplain <- PCA_procTransp%>%
   filter(typeOfComplain=="State" & nchar(complain)>20)%>%
   select(id, complain) # ids: 69 (Germany , France , Great Britain), and 76(Germany, Great Britain, Italy)

multiResp <- PCA_procTransp%>%
  filter(typeOfResp=="State" & nchar(resp)>20)%>%
  select(id, resp)# no multi-parties as respondants..

## id: 69 (Germany , France , Great Britain)
#Make one row data frame with the multi party row
x <- PCA_procTransp%>%
  filter(id == 69)
# replicate it twice (two more parties) and add it to the x df
x <- do.call(rbind, replicate(2, x, simplify=FALSE))

# append it and arrange by id
PCA_procTransp <- rbind(PCA_procTransp, x)%>%
  arrange(as.numeric(as.character(id)))

#now we just have to use a magritr two way pipe '%<>%' to replace the names by index postion
PCA_procTransp[49:51,]%<>% mutate(complain = c("Germany" , "France" , "Great Britain"),
                                   complain_cowc =countrycode(c("Germany" , "France" , "Great Britain"),"country.name", "cowc"))

##id: 76(Germany, Great Britain, Italy)
# Make one row data frame with the multi party row
x <- PCA_procTransp%>%
  filter(id == 76)
#replicate it twice (two more parties) and add it to the x df
x <- do.call(rbind, replicate(2, x, simplify=FALSE))
#append it and arrange by id
PCA_procTransp <- rbind(PCA_procTransp, x)%>%
  arrange(as.numeric(as.character(id)))
# # replace complain and cowc
PCA_genCaseData[58:60,]%<>% mutate(complain = c("Germany", "Great Britain", "Italy"),
                                   complain_cowc =countrycode(c("Germany", "Great Britain", "Italy"),"country.name", "cowc"))


### Export it
save(PCA_procTransp,
     file="data/PCA_procTranspv3.RData")

write.csv(PCA_procTransp,
          file="data/PCA_procTranspv3.csv")
