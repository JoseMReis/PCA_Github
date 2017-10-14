# PCA Disputes: Pulling General Case and Procedural Transparency Data
José M. Reis
(4/10/2017)

# Pulling PCA data

In this session we will extract general case and procedural transparency data from PCA's pulbic dispute repository. The PCA Case Repository contains a full listing of cases commenced since 1996 in respect of which the parties have agreed to publication. In addition, the PCA continues to make information about further public cases since 1902 available on the PCA Case Repository (https://www.pcacases.com/web/allcases/).

### Start

First, we load the relevant packages. The "rvest" packages will be used for parsing and extracting nodes from the HTML objects. "RCurl" for managing HTTP requests. The package "xlsx" will come in hand in the making of intermediate datasets with the raw extracted data (for hand checking its quality). Tidyverse packages, namely "dplyr", "stringr"", and "lubridate" will mostly be used for shapping up the extracted content. Finally, "magritrr" and "countrycode" will be used for tidying upt the dataset.

```{r}
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
```


## Pulling general case data

The general extraction strategy for obtaining general case data is to first extract the case id's from the web links encoded in the main page. Then use those case id's to loop around the case specific pages extracting general case information.

### Pull the case id's

At the website which lists all the cases ("https://pca-cpa.org/en/cases/"), the HTML code reveals that the id numbers for each code are stored as href links in anchor tags. Parse the page and use a xpath query to extract the id's. Obviously, there will be more links than those refering to the cases. So we will use regex and string manipulation to keep just the case id's.

```{r}
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

```

Next, we create a data frame with the id vector composing the id variable.

```{r}
PCA_genCaseData <- data.frame(id = id_raw)
```

Now, we can begin the extraction for the general case data using the id's. From now on, I will resort to another PCA case repository (https://www.pcacases.com/web/allcases/), simply for being less java injected and easier to scrape.

### Pull the case titles

Using the relevant css selector we pull and clean the case titles. Next, we add it to the data frame with the variable name of title.

```{r}
title_raw <- RCurl::getURL("https://www.pcacases.com/web/allcases/") %>%
  read_html() %>%
  html_nodes(css = ".allCasesItems") %>%
  html_text()

### clean it
title <- title_raw %>%
  str_extract("\\:(.*)") %>%
  str_replace("\\:", "") %>%
  str_trim()

### "title". Variable which represents the title of the case as adopted by the PCA. New var name------ > "title"

PCA_genCaseData <- PCA_genCaseData %>%
  mutate(title = title)

```


### Pulling the available general case data

In particular:
* "**complain**" - the complainant

* "**resp**" - the respondant

* "**status**" - status of dispute as of date of access

* "**dateOfCom**" - date of commencement of the proceedings

* "**dateOfConc**" - date of conclusion of the proceedings

* "**typeOfDisp**" - type of dispute as defined by the PCA. Takes five categories: (1)"Inter-state arbitration", (2) "Investment arbitration", (3) "Contract-based or other arbitration", (4) "Other proceeding",(5) "Inter-state conciliation".

* "**procRules**" - adopted procedural rules.

* "**subj**" - subject matter as defined by the PCA

* "**invInstrument**" - invoked instrument.

### Extraction

The above mentioned information can be found at the case specific pages from the https://www.pcacases.com/web/view/. This is the static part of the URL. To access to the case specific pages we just have to add the case id and some other components "/view/149". So this website would be https://www.pcacases.com/web/view/149. Then we will loop across each case specific page and, using the css selectors for the relevant nodes, extract the relevant data after trimming and cleaning it. Websites where this data is missing return an empty string. We control for this possibility with an ifelse condition which assings them an NA.

We starting by setting up some things for the loop. Namely, the static and dynamic URLs, container objects for storing the extracted data, and some regex patters which will be used in the loop to clean the strings.

```{r}
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

```

### The loop

```{r}
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
```

Add the extracted vectors as new variables in the main data frame. Will also build an intermediate data frame (interm_df) which will later be used for double-checking the quality of the extracted data and for handocing missing observations.

```{r}
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
```

## General Case data: handcoding missing observations, some formatting and adding new variables

First, we load the latest dataset.

```{r}

### load the data
load("data/PCA_genCaseData_OriginalExt.RData")
```

We start by turning the date variables into date class using lubridates dmy() as well as creating variables for the year of commencement of the dispute and year of conclusion of the dispute.

```{r}
### Turning dates into date class 
PCA_genCaseData <- PCA_genCaseData%>% 
  mutate(dateCom = dmy(dateCom),
         dateConc = dmy(dateConc))

### Generating yearCom and yearConc variables
PCA_genCaseData <- PCA_genCaseData%>% 
  mutate(yearCom = year(dateCom),
         yearConc = year(dateConc))
```

The extraction went well, but many cases obtained an NA in many variables. Using the interm_df we can check directly at the website and, where applicable, handcode the missing data.

```{r}
### Handcoding the missing observations in the complain and resp variables in PCA_genCaseData
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

```

The disputes vary on the type of parties. At times state-state, other times private-IO or investor-state... . We create a varible for controlling for the type of party by first extracting the information within brackets (as classified by the PCA) in each observation under the complain and resp variables, and then using it for the conditional assignment of type of party. Afterwards, we remove the information in brackets from the complain and resp variables.

```{r}
# Add to the dataset as new variables
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
```


Next, we turn the countries into country code. For the purpose of this dataset we chose the "correlates of war" country code. We use the country code package for the conversion. Some countries were not conversed, so their codes were handcoded.

```{r}
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
  select(id, complain) # it is basically the multi complainant cases! Repeat this step later after the tidying up. See below!!
```

We finish by correcting some mistakes.

```{r}
CA_genCaseData%>% filter(typeOfResp=="State" & is.na(resp_cowc)==T)%>% 
  select(id, resp, typeOfResp) # the resp at id: 116 and 82 are not states. Correct it.

### Correcting typeOfResp for id: 116, 82, 35
PCA_genCaseData <- PCA_genCaseData%>% 
  mutate(typeOfResp = replace(typeOfResp, which(id == c(116, 82)), "Private Entity"))
```

Export the data as a new file.

```{r}
## export it
save(PCA_genCaseData,
     file="data/PCA_genCaseDatav2.RData")
write.csv(PCA_genCaseData, 
          file="data/PCA_genCaseDatav2.csv")
```

## Pulling procedural transparency data

Procedural transparency in the dispute will be coded based on the published dispute relevant documents at the case specific page. This decision depends on the will of the disputing parties and it can range from publishing almost all proceedings-related documents (e.g. https://www.pcacases.com/web/view/72) to none.

Brief description of the extracted variables:

* "**AwardOrDecision**" - dummy variable indicating whether or not the decision/award was made public. It covers not only awards but also "other decisions". Here it is crucial that ww control for discontinued cases.

* "**notOfSubmission**" - dummy variable indicating whether or not the "notice of submission" was published in the repository.

* "**writtenSubmission**" - dummy variable indicating whether or not any written submission made by the parties was published in the repository.Written submissions are crucial parts of the dispute. Still, we must control for the possibility that they never reached this stage. This could be done by checking (i) length of dispute (dateCom - dateConc) and (ii) if a decision was reached.

* "**procOrder**" - dummy variable indicating whether or not procedural orders were published.

* "**audioVisual**" - dummy variable indicating whether or not audio visual coverage of a stage of the proceedings (tipically oral proceedings) was published at the case specific repository. The relevant data is usually found under two separate nodes: (i) audio and (ii) visual. During the extraction we put them together into one.

* "**pressRelease**" - dummy variable indicating whether or not any press releases were published at the case repository.

* "**transcript**" - dummy variable indicating whether or not any transcripts or minutes of a stage of the proceedings were made public.

### Getting the unique document type headers

The documents connected with our relevant variables are always under a specific header. So the first step is to loop around all case-specific pages and subset the unique categories from the general set of all document type headers.

```{r}
## set the URLs
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
```

Again, we build an intermedite data frame with the all the document headers as well as the titles of the published documents per case id and together with the extraction source.

```{r}
## create an interm df
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
```

### Code the procedural transparency variables

Now, we know (i) the existent types of documents ("uniqueHeaders") - the possible -, and (ii) we extracted data on what types of documents were **actually published** in the dispute. So, we can code our dummy variables by logically matching the possible categories with the existant ones. When a match occurs, the variable for that observation gets a 1, else gets a 0.

First, we pass our current data frame to a new one: PCA_procTransp.

```{r}
### Create the working dataframe
PCA_procTransp <- PCA_genCaseData
```

Next, we generate the patterns for our string matching tests based on the existent data and the research question at hand; and add the new procedural transparency variables to the data frame.

```{r}

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
```

Now we can **start the loop**

Step-by-step: We select a sub-list from the extracted headers (one sub-list per case in the order of the case id's at PCA_genCaseData). Using ```str_detect()``` we test if the elements of that list, which are the extracted document type headers available at the case page, match the categories of documents. When they do, the relevant variable gets a 1. Else, it gets a 0. 

```{r}
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
```

We conclude by adding a new date of access variable to record the date of extraction and by exporting the data in a new file.

```{r}
### Add date of access2
PCA_procTransp <- PCA_procTransp%>%
  mutate(dateOfAccess2 = rep(Sys.Date(), nrow(PCA_procTransp)))

### Export it
save(PCA_procTransp,
     file = "data/PCA_procTranspv1.RData")
write.csv(PCA_procTransp,
          file = "data/PCA_procTranspv1.csv")
```

## Procedural transparency data: handcoding missing observations, some formatting and adding new variables

Load the latest dataset.
```{r}
# load PCA_ProcTranspv1
load("data/PCA_procTranspv1.RData")
```

First, we deal with the pending cases. If a case is still pending, then we should not extrapolate anything from the absence of publication of award. Set all the observations which still have pending to NA.

```{r}
PCA_procTransp <- PCA_procTransp %>%
  mutate(awardOrDecision  = replace(awardOrDecision , which(status=="Pending"), NA))

# inspect it
PCA_procTransp %>%
  filter(status == "Pending" & is.na(awardOrDecision)==FALSE)# All good here.
```

Then we export the dataset under a new file name.

```{r}
# export it
save(PCA_procTransp,
     file="data/PCA_procTranspv2.RData")

write.csv(PCA_procTransp,
  file="data/PCA_procTranspv2.csv")
```

## Tidying up

Load the latest dataset.

```{r}
# load the latest dataset
load("data/PCA_procTranspv2.RData")
```


### Dealing with multiple party disputes

Multiple party disputes could be dealt in many ways. Given our interest in particular country characteristics it makes sense to replicate the data with multiple parties by the number of parties and change the countries and country codes where applicable.

Step-by-step: we find the rows with multiple parties. We extract them as a one row data frame, which is then replicated and appended to itself. Next, we append the replicated data frame to the main one and arrange it by id (facilitates inspection..). Finally, we use magritrr's two way pipe ``` %<>% ``` to change the country relevant differences directly to the row-postion of the replicated rows in the main data frame.

```{r}
### Replicate rows which contain multiple parties - one row per country unit.

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
```

Finally, we export the data.

```{r}
## Export it
save(PCA_procTransp,
     file="data/PCA_procTranspv3.RData")

write.csv(PCA_procTransp,
          file="data/PCA_procTranspv3.csv")

######################################### END ############################
```
