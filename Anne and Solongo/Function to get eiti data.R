getEitiData <- function(pathToFolder) {
    
    library(readxl); library(tidyverse)
    ##get the list of files from the Nigeria folder
    listOfFiles <- list.files(pathToFolder, pattern = "xlsx")
    
    ##Setup a tibble to store company ids and names
    companyTable <- tibble(companyID = character(), companyName = character())
    
    ##Setup a tibble to store payment data
    paymentTable <- tibble(gfsCode = character(), revStreamName = character(), agencyName = character(), companyName = character(), payment = numeric(), year = integer(), paymentUSD = numeric())
    
    ##Setup up a loop to load each excel file
    for(i in 1:length(listOfFiles)) {
        ##Setup up the path to each file
        pathToFile <- paste(pathToFolder, "/", listOfFiles[i], sep = "")
        
        # print(paste(excel_sheets(pathToFile), listOfFiles[i], sep = " - "))
        
        ##Check if the Revenue sheet exists
        extractData <- sum(str_detect(excel_sheets(path = pathToFile), "3. Revenues")) == 1
        
        if(extractData) {
            ##get the year of the data
            currYear <- as.integer(substr(listOfFiles[i], 1, 4))
            
            ##Extract company id and names
            tempCompanyData <- read_excel(path = pathToFile, 
                                          sheet = "3. Revenues", 
                                          col_types = "text")
            
            ##Extract the company range in the sheet
            companyRange <- match("C. Companies", names(tempCompanyData)):ncol(tempCompanyData)
            
            ##Get the list of companies
            tempCompanies <- tempCompanyData %>% 
                select(companyRange) %>% 
                filter(`C. Companies` == "Legal name") %>% 
                select(-`C. Companies`) %>% 
                unlist(use.names = F)
            
            ##Get list of company ids
            tempCompanyIDs<- tempCompanyData %>% 
                select(companyRange) %>% 
                filter(`C. Companies` == "Identification #") %>% 
                select(-`C. Companies`) %>% 
                unlist(use.names = F)
            
            ##Construct the company table for this file
            tempCompanyTable <- tibble(companyID = tempCompanyIDs, companyName = tempCompanies) %>% 
                group_by(companyName) %>% 
                mutate(numObs = n(), rowNum = row_number()) %>% 
                ungroup() %>% 
                mutate(companyName = if_else(numObs > 1, paste(companyName, rowNum, sep = "_"), companyName)) %>%
                select(1:2)
            
            ##Add to company table
            companyTable <- bind_rows(companyTable, tempCompanyTable)
            
            ##Extract the payment data
            
            #Find the number of rows to skip
            skipRows <- max(which(str_detect(str_to_lower(tempCompanyData$`Government revenues from extractive companies, per revenue stream`), "gfs codes"))) + 1
            
            endOfData <- which(str_detect(str_to_lower(tempCompanyData$`Government revenues from extractive companies, per revenue stream`), "notes")) - 2
            
            print(endOfData)
            
            ##Find the exchange rate
            exchangeRate <- read_excel(path = pathToFile, sheet = "1. About") %>%
                filter(str_detect(X__1, "Conversion rate"))%>%
                select(X__2) %>% 
                unlist(use.names = F)
            
            ##Load the excel file but skip rows so that we have payment data
            tempPaymentData <- read_excel(path = pathToFile, 
                                          sheet = "3. Revenues", 
                                          skip = skipRows, 
                                          col_types = "text")  %>%
                select(1, 4, 5, 8:ncol(tempCompanyData))

            ##rename the first 3 columns
            names(tempPaymentData)[1:3] <- c("gfsCode", "revStreamName", "agencyName")

            ##rename the payment columns with company names
            names(tempPaymentData)[4:ncol(tempPaymentData)] <- tempCompanyTable$companyName

            ##gather the columns to convert to long format table
            tempPaymentData <- tempPaymentData %>%
                gather(key = "companyName", value = "payment", -(1:3)) %>%
                mutate(payment = as.numeric(payment)) %>%
                filter(!is.na(payment) | payment != 0) %>%
                mutate(year = currYear) %>% 
                mutate(paymentUSD = payment/as.numeric(exchangeRate))


            ##add payment data to payment table
            paymentTable <- bind_rows(paymentTable, tempPaymentData)
            
        }
        
    }
    
    list(companyTable = companyTable, paymentTable = paymentTable %>% select(1:3, companyName, year, payment, paymentUSD)) #
    
}