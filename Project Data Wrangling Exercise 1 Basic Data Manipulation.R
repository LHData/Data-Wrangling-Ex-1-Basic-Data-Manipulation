install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
library(dplyr)
library(tidyr)
library(readr)

#0: Load data from  csv
refine_df <- read_csv("refine_original.csv")

#rename first two columns so they are easier to work with
refine_colnames <- colnames(refine_df)
refine_colnames[1] <- "company"
refine_colnames[2] <- "product_code_number"
names(refine_df) <- refine_colnames

#1: Clean up brand names. 
#start by making the company column all lower case
refine_df$company = tolower(refine_df$company)

#correct misspellings in company column. I used distinct(refine_df, company) to find values. 
for (i in 1:length(refine_df$company)) {
  if (grepl(pattern = "^p", x = refine_df[i,1]) | (grepl(pattern = "^f", x = refine_df[i,1]))) {
    refine_df[i,1] = "philips"
  } else if (grepl(pattern = "^a", x = refine_df[i,1])) {
    refine_df[i,1] = "akzo"
  } else if (grepl(pattern = "^u", x = refine_df[i,1])) {
    refine_df[i,1] = "unilever" 
  }
}

#2: Separate product code and number - break the product_code_number column into two columns
refine_df <- separate(refine_df, product_code_number, c("product_code", "product_number"), sep = "-")

#3: Add product categories
#create a data frame for mapping product codes to product categories
product_code <- c("p", "v", "x", "q")
product_category <- c("Smartphone", "TV", "Laptop", "Tablet")
product_code_lookup <- data.frame(product_code, product_category, stringsAsFactors = FALSE)

#Add product_category column to refine_df by joining to product_code_lookup 
refine_df <- left_join(refine_df, product_code_lookup)

#4: Add full address for geocoding. Add the column full_address, which concatenates addres, city and country
refine_df <- refine_df %>% mutate(full_address = paste(address, ",", city, ",", country))

#5: Create dummy variables for company and product category. Add one column for each of the f
refine_df <- mutate(refine_df, company_philips = ifelse(company == "philips", 1, 0))
refine_df <- mutate(refine_df, company_azko = ifelse(company == "akzo", 1, 0))
refine_df <- mutate(refine_df, company_van_houten = ifelse(company == "van houten", 1, 0))
refine_df <- mutate(refine_df, company_unilever = ifelse(company == "unilever", 1, 0))

refine_df <- mutate(refine_df, product_smartphone = ifelse(product_category == "Smartphone", 1, 0))
refine_df <- mutate(refine_df, product_tv = ifelse(product_category == "TV", 1, 0))
refine_df <- mutate(refine_df, product_laptop = ifelse(product_category == "Laptop", 1, 0))
refine_df <- mutate(refine_df, product_tablet = ifelse(product_category == "Tablet", 1, 0))

#save data frame to csv
write_csv(refine_df, "refine_clean.csv")
