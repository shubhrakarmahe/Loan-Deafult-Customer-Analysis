# -------------------------- EDA Gramaner Case Study ---------------------------------------
# Group Member 1 - Harkirat Dhillon
# Group Member 2 - Shubhra Karmahe
# Group Member 3 - Yogesh BS Raju
# Group Member 4 - Vartika Tewari
# -------------------------------------------------------------------------------------------
# -------------------------- Problem Statement ----------------------------------------------
# 
# The data given below contains the information about past loan applicants and whether they 
# 'defaulted' or not. The aim is to identify patterns which indicate if a person is likely to 
# default, which may be used for taking actions such as denying the loan, reducing the amount 
# of loan, lending (to risky applicants) at a higher interest rate, etc.
# -------------------------------------------------------------------------------------------
# -------------------------- Load the required libraries ------------------------------------ 

load.libraries <- c('data.table', 'ggplot2', 'lubridate', 'dplyr','ggcorrplot','stringr',
                    'treemap','xray','devtools','ggcorrplot')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]

for(libs in install.lib) install.packages(libs)
sapply(load.libraries, require, character = TRUE)

# -------------------------- Raw File import and Preliminary analysis -------------------------

# It will interpret (NA,n/a and empty string) in the raw file as NA.

dataset <- fread(input = "loan.csv", stringsAsFactors = TRUE, na.strings = c("NA","","n/a"), data.table = F)

# 39717 rows and 111 columns
nrow(dataset)
ncol(dataset)

head(dataset)
tail(dataset)

str(dataset)
summary(dataset)

# -------------------------- Data Preparation ------------------------------------------------

# 0 complete rows in dataset i.e. All the rows have missing data.
sum(complete.cases(dataset)) 

# 1. Validate Duplicated values for dataset; there are 39717 unique rows i.e. no duplicate rows
uniqueN(dataset) 

# 2. checking for NA values 
data.frame(colSums(is.na(dataset)))

# Out of 111 columns - 54 columns have all their row Values as NA.
# so ,we can exclude them from our analysis dataset.
sum(sapply(dataset, function(x) all(is.na(x))))

# create a new dataset loan after exclusion of those 54 columns
loan <- dataset[,-(which(colMeans(is.na(dataset)) == 1))]

# New dataset loan have 57 columns and 39717 rows
dim(loan)
str(loan)
# -------------------------- Data Cleaning & Univariate Analysisfor loan  ---------------------------------------------

# 39703 incomplete rows
sum(!complete.cases(loan))

# -------------------------- Validate Case Mismatch & Other Data Issue-------------------------

# loan_status column - 3 unique values : Fully Paid,Charged Off and Active  - no case mismatch
# most of the loan are in fully paid status.
# Pie chart indicate that a significant number of borrowers in our dataset paid off their loan - 
# 83% of loan borrowers Fully paid the they amount borrowed, 2.9% of loan borrowers are paying currently
# 14.2% unfortunately defaulted. 

table(loan$loan_status)

ggplot(loan, aes(x = "", fill = loan_status)) + 
  geom_bar(width = 1, col="black", aes(y=(..count..)/sum(..count..))) +
  geom_text(aes(y=((..count..)/sum(..count..)),
                label = scales::percent((..count..)/sum(..count..))),
            stat = "count", size = 4, 
            position = position_stack(vjust = 0.5) ) +
  coord_polar(theta = "y", start = 90) + theme_void() 

# grade column - 7 unique values : A,B,C,D,E,F,G - no case mismatch
# B grade were issued most of the loans and G grade were issued the least number of loans.
table(loan$grade)

ggplot(data = loan, aes(grade)) + geom_bar(color = 'black', fill = 'dodgerblue') + 
  xlab('Grade') + ylab('No. of borrowers')

# sub_grade column - 35 unique values : example - A1,A2,A3,B1,C1 etc - no case mismatch
# B3 sub-grade were issued most of the loans and G5 sub-grade were issued the least number of loans.
table(loan$sub_grade)

ggplot(data = loan, aes(sub_grade)) + geom_bar(color = 'black', fill = 'dodgerblue') + 
  xlab ("Sub Grade") +  ylab('No. of borrowers')

# verification_status column - 3 unique values : verified,source verified ans not verified- no mismatch
# applicants having verification status as NOT Verified were issued most of the loans
table(loan$verification_status)

ggplot(data = loan, aes(verification_status)) + geom_bar(color = 'black', fill = 'dodgerblue') +
  xlab("Verification Status") + ylab('No. of borrowers')

# home_ownership column - 5 unique values : MORTGAGE,RENT,OWN,OTHER & NONE - no case mismatch
# as per data dictionary there is no NONE category.So we are assuming it as NA.
# applicants having house_ownership status as RENT or MORTGAGE were issued most of the loans
table(loan$home_ownership)

ggplot(data = loan, aes(home_ownership)) + geom_bar(color = 'black', fill = 'dodgerblue') + 
  xlab('House Ownership') + ylab('No. of borrowers')

# purpose column - 14 unique values : car,debt_consolidation etc.- no case mismatch
# Most of the loans were issued for debt_consolidation purpose
table(loan$purpose)

ggplot(data = loan, aes(purpose)) + geom_bar(color = 'black', fill = 'dodgerblue') +
   ylab('No. of borrowers') + xlab("Purpose") + theme(axis.text.x = element_text(angle = 90))

# term column - 2 values : 36 months & 60 months - no case mismatch
# Data issue - remove months from 36 months and 60 months. Rename column as term_in_mths
# most of the loans were issued for term -36 months.
table(loan$term)

loan$term_in_mths <- str_replace_all(string = loan$term, pattern = 'months', replacement = '')

ggplot(data = loan, aes(term_in_mths)) + geom_bar(color = 'black', fill = 'dodgerblue', width = 0.5) +
  xlab('Term (in months)') + ylab('No. of borrowers')

# int_rate column - 371 unique values
# Data issue :- remove % symbol and convert int_rate to numeric
# most of the loans were issued for 10-15 % interest rate
table(loan$int_rate)

loan$int_rate <- str_replace_all(string = loan$int_rate, pattern = "%", 
                                 replacement = "") %>% as.double()

ggplot(data = loan, aes(int_rate)) + geom_histogram(color = 'black', fill = 'dodgerblue', binwidth = 2) +
  xlab('Interest Rate') + ylab('No. of borrowers')

# loan_amnt column - 885 unique values - as 29900,30500 etc
# majority of people have taken loan in range of  5000 - 10000 & very few people have taken loan >= 30000
table(loan$loan_amnt)

ggplot(loan,aes(loan_amnt)) + geom_histogram(binwidth = 5000 ,color = "black" , fill = "dodgerblue") +
  xlab('Loan amount') + ylab('No. of borrowers') + scale_x_continuous(breaks = seq(0, 36000, by = 5000))
  
# dti column - 2868 unique values - as 10,9.77 etc
# majority of people have dti between 10 to 20 & few loan were granted for dti > 25
table(loan$dti)

ggplot(data = loan, aes(dti)) + geom_histogram(color = 'black', fill = 'dodgerblue' ,binwidth = 3) +
  xlab('Debt to Income Ratio') + ylab('No. of borrowers')

# issue_d column - date in Mon-YY format - 55 unique values
# convert date to yyyy-mm-dd format
# most of the loans were issued in the year range of 2011-2012. 
table(loan$issue_d)

loan$issue_d <- myd(loan$issue_d, truncated = 1)

ggplot(data = loan, aes(issue_d)) + geom_histogram(color = 'black', fill = 'dodgerblue', binwidth = 100) +
  xlab("Issue Date") + ylab('No. of borrowers')

# addr_state column - 50 unique values :  - no mismatch
# most of the loans were issued for CA state.
table(loan$addr_state)

ggplot(data = loan, aes(addr_state)) + geom_bar(color = 'black', fill = 'dodgerblue') +
  ylab('No. of borrowers') + xlab('State') 

# earliest_cr_line column - date format Mon-YY -526 unique values
# convert date to yyyy-mm-dd format
table(loan$earliest_cr_line)

loan$earliest_cr_line <- myd(loan$earliest_cr_line, truncated = 1)

# total_pymnt column : Round off decimal digits to 2 to maintain data consistency for the column.
# data issue :- round off to 2 decimal digits
table(loan$total_pymnt)

loan$total_pymnt <- round(x = loan$total_pymnt, digits = 2)

# total_rec_late_fee column :- Round off decimal digits to 2 to maintain data consistency for the column.
table(loan$total_rec_late_fee)

loan$total_rec_late_fee <- round(x = loan$total_rec_late_fee, digits = 2)

# collection_recovery_fee column : Round off decimal digits to 2 to maintain data consistency for the column.
table(loan$collection_recovery_fee)

loan$collection_recovery_fee <- round(x = loan$collection_recovery_fee, digits = 2)

# last_pymnt_d column - Format Mon-YY 
# convert date to YYYY-MM-DD format
table(loan$last_pymnt_d)

loan$last_pymnt_d <- myd(loan$last_pymnt_d, truncated = 1)

#---------------------------------Missing value (NA) analysis----------------------------
# Looking for NA & NaN values
data.frame(colSums(is.na(loan)))

sapply(loan, function(x) length(which(is.nan(x))))

# emp_length column - 12 unique values : example 10+ years,1 year, <1 year,n/a
# data issue - If required then remove year/years,<,+ strings and convert emp_length to numeric
# As per data dictionary - 10 + years is assumed as 10 and <1 year is assumed as 0 
# 1075 - Na values (no imputation is done for NA values)

# most of the loans were issued to emp_length of 10 + years 
# and least number of loans were issued to emp_length of 9 years
table(loan$emp_length)

loan$emp_length_in_yrs <- str_replace_all(string = loan$emp_length, pattern = "years",
                                   replacement = "") %>%
  str_replace_all(pattern = "year", replacement = "") %>%
  str_replace_all(pattern = "< 1", replacement = "0") %>%
  str_replace_all(pattern = "10\\+", replacement = "10")

ggplot(data = loan, aes(emp_length_in_yrs)) + geom_bar(color = 'black', fill = 'dodgerblue') + 
  xlab('Employment Length(in years)') + ylab('No. of borrowers')

# title column - 19617 unique values including NA: Computer,JAL loan,personal etc.
# data issue - 10 NA values (no imputation is done for NA values)

# revol_util column : 1089 unique values including NA
# data issue - 50 NA values (no imputation is done for NA values) and (remove % sign and convert to double)
# most of the revol_util is at 0%
table(loan$revol_util)

loan$revol_util <- str_replace_all(string = loan$revol_util, pattern = "%",
                                 replacement = "") %>% as.double()

# next_pymnt_d column - date format Mon-YY : 3 unique values including NA - Jul-16,Jun-16
# 38577 Na values - (no imputation is done for NA values)
# convert date to YYYY-MM-DD format
table(loan$next_pymnt_d)

loan$next_pymnt_d <- myd(loan$next_pymnt_d, truncated = 1)

# mths_since_last_delinq column  - 96 unique values including NA - example 35,103,107,85 etc.
# 25682 NA values (no imputation is done for NA values)
table(loan$mths_since_last_delinq)

# mths_since_last_record column - 112 unique values including NA - example 113,71,0,19 etc
# 36931 NA values (no imputation is done for NA values)
table(loan$mths_since_last_record)

# last_credit_pull_d column - date format Mon-YY 
# 2 NA values (no imputation is done for NA values)
# convert date to YYYY-MM-DD format
table(loan$last_credit_pull_d)

loan$last_credit_pull_d <- myd(loan$last_credit_pull_d, truncated = 1)

# chargeoff_within_12_mths column : 2 unique values including NA - 0
# Na values - 56 (no imputation is done for NA values)
table(loan$chargeoff_within_12_mths)

# pub_rec_bankruptcies column :- 4 unique values including NA - 0,1,2
# 697 NA values (no imputation is done for NA values)
# most of the applicants have 0 bankruptcies record.
table(loan$pub_rec_bankruptcies)

loan$pub_rec_bankruptcies <- as.factor(loan$pub_rec_bankruptcies)

ggplot(data = loan, aes(pub_rec_bankruptcies)) + geom_bar(color = 'black', fill = 'dodgerblue') + 
  xlab('Public Bankruptcies') + ylab('No. of borrowers')

# tax_liens column :- 2 unique values including NA - 0
# 39 NA values (no imputation is done for NA values)
table(loan$tax_liens)

# collections_12_mths_ex_med column : 2 unique values including NA - 0
# 56 NA values (no imputation is done for NA values)
table(loan$collections_12_mths_ex_med)

# emp_title column - 28823 unique values : example - US Army,Bank of AMerica,IBM,AT&T,Kraiser Permanete & NA
# 2453 Na values (no imputation is done for NA values)
table(loan$emp_title)

# ---------------------------Remove columns --------------------------------------------------
# Removing columns that will not be used in further analysis

data.frame(sapply(loan,function(y) length(unique(y))))

# term - its redundant column as new column term_in_mths is created during data cleaning
# emp_length - its redundant column as new column emp_length_in_yrs is created during data cleaning
# url & member_id- As we are keeping id as unique identifier
# desc - descriptive column
# zip_code - masked data can't be used for analysis.
# pymnt_plan column - 1 unique value : n
# initial_list_status - 1 unique value : f
# policy_code column - 1 unique  value : 1
# application_type column : 1 unique value - INDIVIDUAL
# acc_now_delinq column : 1 unique value - 0
# delinq_amnt column : 1 unique value - 0

drop_cols <- c('term','url','member_id','desc','initial_list_status','zip_code','pymnt_plan',
               'emp_length','policy_code','application_type','acc_now_delinq','delinq_amnt')

loan[,drop_cols] <- NULL

# ---------------------- Univariate analysis ends ---------------------------------------

# ---------------------- Segmented Univariate Analysis starts ---------------------------

# Term-wise loan status
# Charged off rate is lower for loan re-payment term for 60 months.
ggplot(loan,aes(x="",fill=loan_status))+ geom_bar(col="black",position = "fill") +
  labs(x = "Loan Term (in months)", y = "Proportion", fill = "Loan status") +  facet_grid(facets=. ~ term_in_mths) 

# Effect of default rate on Loan Purpose
## It makes sense that if the borrower's small business is not doing well then it is difficult to repay the loan.
ggplot(data = loan %>% filter(loan_status == 'Charged Off'), aes(purpose)) + 
  geom_bar(color = "black" ,fill = 'dodgerblue') + xlab("Loan Purpose") + ylab('No. of borrowers') +
theme(axis.text.x = element_text(angle = 90), axis.text.y = element_blank(),
      axis.ticks.y = element_blank())

# Effect of Grade on Interest Rate
ggplot(data = loan , aes(int_rate)) +  geom_histogram(color = "black" ,fill = 'coral', binwidth = 5) + 
   facet_grid(.~grade) + xlab("Interest Rate") + ylab("Count")

# Bar plot of verification_status by loan status
ggplot(loan,aes(x=verification_status,fill=loan_status))+ 
  geom_bar(col="black",position = "fill") + 
  labs(x="Verification Status", y="Proportion",
       title="Proportion of borrowers by income verification status")
# Conclusion: Results show opposite of what we would expect.

# Loan amount segmented by loan_status
ggplot(loan,aes(x=loan_amnt,fill=loan_status))+ 
  geom_histogram(col="black",position = "fill",
                 breaks=seq(0,35000,by=5000)) + 
  labs(x="Loan Amount($)", y="Proportion",
       title="Proportion of borrowers by loan amount")
# Conclusion: With increase in loan amount the proportion of 
# defaulted loans is increasing.

# ---------------------- Segmented Univariate Analysis ends ---------------------------

#----------------------------------------------------------------------------------------

# Identify potential columns for further analysis

# 1. Grade & sub_grade - each loan is rated with a grade that tries to capture the risk of default.
#                        the lower the grade, the higher the default risk is and, consequently, 
#                         the higher the interest rate will be.
# 2. loan purpose - small business and debt consolidation are the most riskiest one.
# 3. dti - as low the ratio the better chances of not being default. The few factors affecting DTI are as below :-
#  3.1 annual income - 
#  3.2 emp_length -
#  3.3 home ownership - 
#
# 4. Interest_rate - the higher the interest rate, the higher the probability of default is. .  
# 5. loan term - default rate is low for longer term loan.
# 6. inq_last_6mths - if there is credit inquiry in last 6 mths then the chances of default are higher
# 7. delinq_2yrs - if there is delinq record in 2 yrs then the chances of default are higher
# 8. pub_rec - if there is public record then the chances of default are higher
# 9. pub_rec_bankruptcies - f there is public record bankruptcies then the chances of default are higher
# ---------------------------------------------------------------------------------------
# --------------------------Bi variate Analysis -----------------------------------------

# -------------Bi variate analysis of loan amount with major categorical variables-------
# We are not treating outliers in this analysis.

# boxplot for loan amount and grade (to look for outliers)
# loan given to A grade applicants had most of the outlier values
# outliers decreases as we move from grade A to G.

ggplot(data = loan, aes(y = loan_amnt, x = grade)) +  geom_boxplot(fill = 'orange') + ylab('Loan Amount') +  
  xlab('Grade')

# boxplot for loan_amount Vs term_in_mths with facets of grade (to look for outliers)
# There are no outliers for 60 months term loan. Outliers are present for 36 months term from Grade A to D.
# No. of loan are higher for 60 months term.
ggplot(data = loan, aes(y = loan_amnt, x = term_in_mths)) + geom_boxplot(fill = 'orange') + 
  ylab('Loan Amount') +  xlab('Term(in months)')
  
# boxplot for loan_amount Vs home_ownership (to look for outliers)
# less loan amount is approved for applicant's having  house_ownership as NONE.
# Most of the outliers are present for RENT.
ggplot(data = loan, aes(y = loan_amnt, x = home_ownership)) + geom_boxplot(fill = 'orange') +
  ylab('Loan Amount') +  xlab('House Ownership') +  theme(axis.text.x = element_text(angle = 90))
        
# boxplot for loan_amount Vs loan_purpose (to look for outliers)
# so many outlier values.
# high loan amount is approved for loan purpose - small business
ggplot(data = loan, aes(y = loan_amnt, x = purpose)) + geom_boxplot(fill = 'orange') + 
  ylab('Loan Amount') + xlab('Loan Purpose') + theme(axis.text.x = element_text(angle = 90))

# boxplot for loan_amount Vs emp_length (to look for outliers)
# so many outlier values.
# most of the loan amount is approved for emp_length greater than 10 years
ggplot(data = loan, aes(y = loan_amnt, x = emp_length_in_yrs)) + geom_boxplot(fill = 'orange') + 
  ylab('Loan Amount') + xlab('Employment Length (in years)')

#-------------Bi variate analysis of interest rate with major categorical variables--------

# boxplot for int_rate and grade (to look for outliers)
# As the grade moves from A to G the interest rate also increases.
ggplot(data = loan, aes(y = int_rate, x = grade)) + geom_boxplot(fill = 'pink') + 
  ylab('Interest Rate') + xlab('Grade')

# boxplot for int_rate Vs term (to look for outliers)
# interest rate is bit lower for 36 months term and few outliers for 36 months term
ggplot(data = loan, aes(y = int_rate, x = term_in_mths)) + geom_boxplot(fill = 'pink') + 
  ylab('Interest Rate') + xlab('Term')

# boxplot for int_rate Vs home_ownership (to look for outliers)
# less loan amount is approved for applicant's having  house_ownership as NONE.
ggplot(data = loan, aes(y = int_rate, x = home_ownership)) + geom_boxplot(fill = 'pink') + 
  ylab('Interest Rate') + xlab('House Ownership') + theme(axis.text.x = element_text(angle = 90))

# boxplot for int_rate Vs loan_purpose (to look for outliers)
# interest rate is usally higer for loan purpose - small business & debt consolidation
ggplot(data = loan, aes(y = int_rate, x = purpose)) + geom_boxplot(fill = 'pink') +  
  ylab('Interest Rate') + xlab('Purpose') +  theme(axis.text.x = element_text(angle = 90))

# boxplot for int_rate Vs emp_length (to look for outliers)
# interest rate is almost same for all emp_length except for emp_length = NA
ggplot(data = loan, aes(y = int_rate, x = emp_length_in_yrs)) + geom_boxplot(fill = 'pink') + 
  ylab('Interest Rate') + xlab('Employment Length(in years)')

#-------------Bi variate analysis of dti with major categorical variables--------

# boxplot for dti and grade (to look for outliers)
# Lower dti - better grade (No outliers)
ggplot(data = loan, aes(y = dti, x = grade)) + geom_boxplot(fill = 'blue') + 
  xlab('Grade') + ylab('DTI')

# boxplot for dti Vs term (to look for outliers)
# dti is low for applicants having 36 months term  ( No outliers)
ggplot(data = loan, aes(y = dti, x = term_in_mths)) + geom_boxplot(fill = 'blue') +
  xlab('Term( in months)') + ylab('DTI')

# boxplot for dti Vs loan_purpose (to look for outliers)
# dti is very high for loan purpose - credit card and debt consolidation
ggplot(data = loan, aes(y = dti, x = purpose)) + geom_boxplot(fill = 'blue') +
  xlab('Purpose') + ylab('DTI') + theme(axis.text.x = element_text(angle = 90))

# --------------------------Tree Map ------------------------------------------
# Tree map for analysing loan purpose with loan amount
lpurp_df <- loan %>% select(purpose, loan_amnt) %>% 
  na.omit() %>% group_by(purpose) %>% 
  dplyr::summarise(volume = n(), 
                   average_amnt = 
                     sum(as.numeric(loan_amnt), rm.na = TRUE)/n())

lpurp_df <- lpurp_df[!lpurp_df$purpose == "", ]

treemap(lpurp_df, index = "purpose", vSize = "volume", 
        vColor = "average_amnt", 
        range = c(5000, 14000), 
        type = "manual", 
        palette = c("yellow", "green", "orange", "orange2", "firebrick"), 
        algorithm = "pivotSize", 
        sortID = "-size", 
        title = "Loan Purpose", 
        title.legend = "Avg Amount", 
        fontfamily.labels = "serif", 
        fontsize.labels = 16, 
        fontsize.legend = 10, 
        fontface.labels = 1, 
        position.legend = "bottom", 
        force.print.labels = T, 
        align.labels = list(c("left", "top")),
        border.col = "white")

# Debt consolidation is the most common reason for # borrowing. The greatest advantage of peer-to-peer 
# lending is the low cost. Most consumers choose to consolidate debt to enjoy lower borrowing costs.
# The different color is related to the average amount of a loan. Loans for debt consolidation, 
# credit card, house, and small business usually have higher average amount than other purposes.
#-------------------------------------------------------------------------------

# --------------------------- Derived Matrix -----------------------------------

# 1. ROI metric- Return on funds invested by investors
loan$returns_from_inv <- round(((as.numeric(loan$total_pymnt - loan$funded_amnt_inv))/
                                  as.numeric(loan$funded_amnt))*100,2)


ggplot(loan,aes(x = returns_from_inv, fill = loan_status)) +  geom_density() + 
  labs(x = 'Returns from Investments', y = 'Proportion', fill = 'Loan status')
  
# Investors have a negative ROI for charged off loans, huge loss of money 
# on the other hand ROI from fully paid loans is highest

# 2. is_bad metric - It is applied on the observations having loan_status as ''Charged Off' to check 
# whether they were likely to default or not.
loan_default <- loan %>% filter(loan_status == 'Charged Off')

loan_default$is_bad <- ifelse(((loan_default$delinq_2yrs > 0) | 
                         (loan_default$pub_rec > 0) | 
                         (as.numeric(loan_default$pub_rec_bankruptcies) > 0) |
                         (as.numeric(loan_default$emp_length) < 1)), 1, 0)

# 5561 applicant's are most probably to default.
table(loan_default$is_bad)

# 3. Ratio of loan amount by annual income
loan$loan_amnt_by_annual_inc <- round(loan$loan_amnt/loan$annual_inc, digits = 2)

# Histogram of loan_amnt_by_annual_inc

ggplot(loan,aes(x = loan_amnt_by_annual_inc,fill = loan_status)) + 
  geom_histogram(col = "black", position = "fill", breaks = seq(0,1,by = 0.2)) +
  labs(x = "Ratio of loan amount to annual income", y = "Proportion", fill = "Loan status")

# Conclusion: Higher the ratio, higher is the proportion of defaulters.

# ---------------------------Correlation Matrix -------------------------------

xray::anomalies(loan)

num_vars <- loan %>%  sapply(is.numeric) %>%  which() %>% names()

loan_corr <- loan[, num_vars]

loan_corr_drop <- c(xray::anomalies(loan_corr)$problem_variables$Variable)

loan_corr[ ,loan_corr_drop] <- NULL

corr <- cor(loan_corr, use = "complete.obs")

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", 
           colors = c("tomato2", "white", "springgreen3"), title="Correlogram", ggtheme=theme_bw)

# ------------------------------------------------------------------------------