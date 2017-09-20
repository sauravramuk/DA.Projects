
loan <- read.csv("loan.csv",header=T)
require('zoo')
require('dplyr')
require('ggplot2')
require('tidyr')
require('reshape2')
#package scales is required for some plots

# Data modification and cleaning 
head(loan)
View(loan)
# Remove the NA columns 
loan_data <- loan[,colSums(is.na(loan))<nrow(loan)]

# Converting to Date format - the date columns 

loan_data$issue_d_1 <-  as.yearmon(loan_data$issue_d, "%b-%y")
loan_data$last_pymnt_d_1 <-  as.yearmon(loan_data$last_pymnt_d, "%b-%y")
loan_data$earliest_cr_line_1 <-  as.yearmon(loan_data$earliest_cr_line, "%b-%y")
loan_data$last_credit_pull_d_1 <-  as.yearmon(loan_data$last_credit_pull_d, "%b-%y")
loan_data$next_pymnt_d_1 <-  as.yearmon(loan_data$next_pymnt_d, "%b-%y")

# Converting the int_rate column into numeric 

loan_data$int_rate<- round(as.numeric(gsub("%","",loan_data$int_rate)),2)

# Converting the revol_util column into numeric 

loan_data$revol_util<- round(as.numeric(gsub("%","",loan_data$revol_util)),2)

# Round off the decimal part from dti column 

loan_data$dti<- round(loan_data$dti,2)

# Split dataset into 2 parts based on defaulted and fully paid loans

loan_data_charged_off <- filter(loan_data, loan_data$loan_status=="Charged Off")
loan_data_fully_paid <- filter(loan_data, loan_data$loan_status=="Fully Paid")



######################################CORRELATION MATRIX#######################################
#this plot will give us a birds eye view of the significant variables and 
#their correlation

#loan_columns_corr_mat_col <- c("loan_amnt","funded_amnt","funded_amnt_inv","term","int_rate","installment","sub_grade","emp_length","annual_inc","issue_d_month","dti","delinq_2yrs","inq_last_6mths","mths_since_last_delinq","mths_since_last_record","open_acc","pub_rec","revol_bal","revol_util","total_acc","out_prncp","out_prncp_inv","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","collection_recovery_fee","last_pymnt_amnt","collections_12_mths_ex_med","acc_now_delinq","chargeoff_within_12_mths","delinq_amnt","pub_rec_bankruptcies","tax_liens")            
loan_columns_corr_mat_col <- c("loan_amnt","funded_amnt","loan_status","funded_amnt_inv","term","int_rate","installment","sub_grade","emp_length","annual_inc","issue_d_month","dti","delinq_2yrs","inq_last_6mths","mths_since_last_delinq","mths_since_last_record","open_acc","pub_rec","revol_bal","revol_util","total_acc","out_prncp","out_prncp_inv","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","collection_recovery_fee","last_pymnt_amnt","pub_rec_bankruptcies")
loan_columns_corr_mat <- loan_data[,loan_columns_corr_mat_col]

#Converting to to numeric values
unique(loan_columns_corr_mat$term)
term <- lapply(X = loan_columns_corr_mat$term, FUN = function(x){if(x ==' 36 months')return(36)else return(60)})
term <- as.vector(x = term, mode = 'numeric')
loan_columns_corr_mat$term <- term

#Loan Sub-grade
levels(loan_columns_corr_mat$sub_grade)
subGrade <- sapply(X = as.character(loan_columns_corr_mat$sub_grade), FUN = switch, "A1" = 35,
                   "A2" = 34,
                   "A3" = 33,
                   "A4" = 32,
                   "A5" = 31,
                   "B1" = 30,
                   "B2" = 29,
                   "B3" = 28,
                   "B4" =27,
                   "B5" =26,
                   "C1"=25,
                   "C2"=24,
                   "C3"=23,
                   "C4"=22,
                   "C5"=21,
                   "D1"=20,
                   "D2"=19,
                   "D3"=18,
                   "D4"=17,
                   "D5"=16,
                   "E1"=15,
                   "E2"=14,
                   "E3"=13,
                   "E4"=12,
                   "E5"=11,
                   "F1"=10,
                   "F2"=9,
                   "F3"=8,
                   "F4"=7,
                   "F5"=6,
                   "G1"=5,
                   "G2"=4,
                   "G3" = 3,
                   "G4" = 2,
                   "G5" = 1, USE.NAMES = F)

loan_columns_corr_mat$sub_grade <-  subGrade

#Employment length: converting to numeric value
unique(loan_columns_corr_mat$emp_length)
employmentLength <- loan_columns_corr_mat$emp_length %>% gsub(pattern = '<', replacement = '',fixed = T) %>%
  gsub(pattern = 'years', replacement = '',fixed = T) %>%
  gsub(pattern = 'year', replacement = '',fixed = T) %>%
  gsub(pattern = '+', replacement = '',fixed = T) %>%
  as.numeric()

employmentLength[is.na(employmentLength)] <- 0
unique(employmentLength)
loan_columns_corr_mat$emp_length <- employmentLength

#issued month, convert it to month numbers
unique(loan_columns_corr_mat$issue_d_month)
monthsNumber <- sapply(X = loan_columns_corr_mat$issue_d_month, FUN = switch, "Dec" = 12, 
                       "Nov"  = 11,
                       "Oct" =10,
                       "Sep" =9,
                       "Aug" =8,
                       "Jul"=7,
                       "Jun"=6,
                       "May"=5,
                       "Apr" =4,
                       "Mar"=3,
                       "Feb"=2,
                       "Jan"=1,
                       USE.NAMES = F)
monthsNumber
unique(monthsNumber)
loan_columns_corr_mat$issue_d_month <- monthsNumber

#month since last default
unique(loan_columns_corr_mat$mths_since_last_delinq)

#revol balance
unique(loan_columns_corr_mat$revol_util)
loan_columns_corr_mat$revol_util[is.na(loan_columns_corr_mat$revol_util)] <- 0

#public bankruptcies
loan_columns_corr_mat$pub_rec_bankruptcies[is.na(loan_columns_corr_mat$pub_rec_bankruptcies)] <- 0

# Month since last delinq
loan_columns_corr_mat$mths_since_last_delinq[is.na(loan_columns_corr_mat$mths_since_last_delinq)] <- 1000
#month since last public record
loan_columns_corr_mat$mths_since_last_record[is.na(loan_columns_corr_mat$mths_since_last_record)] <- 1000

#convert the loan status to 
unique(loan_columns_corr_mat$loan_status)
loanStatus <- sapply(X = as.character(loan_columns_corr_mat$loan_status), FUN = switch, "Charged Off" = 1, 
                     "Current" = 2, 
                     "Fully Paid" = 3,
                     USE.NAMES = F)

loan_columns_corr_mat$loan_status <-  loanStatus

#generate a coorelation matrix

loan_columns_correlation_mat <- round(cor(loan_columns_corr_mat),2)
loan_columns_correlation_mat_melted <- melt(loan_columns_correlation_mat)
loan_columns_correlation_mat_melted
#plot a correlation Matrix using ggplot tile

ggplot(data = loan_columns_correlation_mat_melted, aes(x = Var1, y = Var2)) + 
  geom_tile(aes(fill = value)) + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_gradient( low = "#ff0400", high = "#08ff00",
                       space = "Lab", na.value = "grey50", guide = "colourbar") +
  xlab('Variables X Axis') + 
  ylab('Variables Y Axis') 


########################################################  Plots  ##########################################################

#loan Grade
# There is a strong correlation
ggplot(data = loan_data, aes( x = sub_grade)) + geom_histogram(stat = 'count', aes(fill = loan_status), position = 'fill')



#Verification Status vs Loan Grade Vs Declared Annual Income
ggplot(loan_data[loan_data$annual_inc<quantile(x = loan_data$annual_inc, c(.98)), ],
       aes( x =loan_status, y = annual_inc)) + 
  geom_point(aes(col = factor(verification_status), size = int_rate), ) +
  ylab('Declared Annual Income') + xlab('Loan Grade')


#Yearly Trend 
loan_data <-  separate(data = loan_data, 
                       col = 'issue_d', 
                       into = c('issue_d_month', 'issue_d_year'),
                       sep = '-',
                       remove = F
)
loan_data$issue_d_year <- 2000 + as.numeric(loan_data$issue_d_year)

#Considerable increase in LOAN, yearwise
ggplot(data = loan_data, aes(x = issue_d_year)) + geom_bar(stat = 'count', aes(fill = factor(issue_d_month)), position = position_dodge())+ scale_colour_manual(values = c("")) + ylab('Count of Loans') + xlab('Year (YY)')

#Purpose
unique(loan_data$purpose)

ggplot(data = loan_data, aes(x= factor(purpose,levels = names(sort(table(purpose), decreasing = TRUE))))) + 
         geom_histogram(stat = 'count')

ggplot(data = loan_data, aes(x = purpose)) + geom_histogram(stat = 'count', aes(fill=loan_status))

#credit card,other, small business debt consolidation have higher risk
loan_data_factor <- within(loan_data, loan_status <-  factor(loan_status, levels = names(sort(table(loan_status), decreasing = T))))
loan_data_factor_chargedoff <- loan_data_charged_off %>% group_by(addr_state,loan_status) %>% summarise(charged_off = length(loan_status))
loan_data_factor_fullypaid <- loan_data_fully_paid %>% group_by(addr_state,loan_status) %>% summarise(fully_paid = length(loan_status))

merged_data <- merge(x = loan_data_factor_chargedoff,y =loan_data_factor_fullypaid , by = 'addr_state', all = T)

#states - more than 50% of the loans in NE are defaulting
ggplot(data = merged_data, aes(x = reorder(addr_state, -(charged_off/(fully_paid + charged_off))*100), y = (charged_off/(fully_paid + charged_off))*100 )) +
  geom_col() +
  ylab('Default in Percent') + xlab('State Codes')



#slight correlation between the number of inquiries in past 6 months and 
inquiriesVsDefault <- as.data.frame(loan_data %>% 
                                      group_by(inq_last_6mths, loan_status) %>% 
                                      summarise(length(loan_status)))

ggplot(inquiriesVsDefault, aes(x = inq_last_6mths, y = `length(loan_status)`)) + geom_point(aes(col = loan_status))

count_percent <- vector(mode = 'numeric', length = nrow(inquiriesVsDefault))
total_c <- vector(mode = 'numeric', length = nrow(inquiriesVsDefault))
x <- as.data.frame(cbind(count_percent, total_c))
inquiriesVsDefault <- cbind(inquiriesVsDefault, x)

for(i in 0:8){
  inquiriesVsDefault$total_c[inquiriesVsDefault$inq_last_6mths == i] <- sum(inquiriesVsDefault$`length(loan_status)`[inquiriesVsDefault$inq_last_6mths == i])
}
inquiriesVsDefault$count_percent <- inquiriesVsDefault$`length(loan_status)`/inquiriesVsDefault$total_c
inquiriesVsDefault$count_percent <- inquiriesVsDefault$count_percent *100

#We see that as the number of inquiries increases there is an increase in the default trend
ggplot(inquiriesVsDefault, aes(x = inq_last_6mths, y = count_percent)) + geom_point(aes(col = loan_status)) +geom_smooth(aes(col = loan_status))

#open credit lines
#people with 1,2 or 3 credit line seems to have a slightly higher default %
#there are people with 25-26 lines that have much higher default rate
unique(loan_data$open_acc)
ggplot(data = loan_data, aes(x = open_acc)) + geom_histogram(binwidth = 1, aes(fill = loan_status), position = 'fill') + ylab("Percentage of count of people") + xlab("Number of Open credit lines")



#revolving balance utilization
loan_data$revol_util <- gsub(x = loan_data$revol_util, pattern = '%', replacement = '', fixed = T)
loan_data$revol_util <- as.numeric(loan_data$revol_util)
loan_data$revol_util
length(loan_data$revol_util[is.na(loan_data$revol_util)])
ggplot(data = loan_data, aes(x = revol_util)) + geom_histogram(binwidth = 2, aes(fill = loan_status), position = 'fill')
#there is a visible trend, people with higher balance to credit limit ratio seems to default more


#Revolving balance:
ggplot(data = loan_data, aes(x = revol_bal )) + 
  geom_histogram(binwidth = 3000, aes(fill = loan_status), position = 'fill') +
  xlab('Revolving Balance') + ylab('Count of people') 



#total_acc vs visible_acc
#seems to have a trend
ggplot(data = loan_data[loan_data$loan_status=='Charged Off',], aes(y = open_acc, x = total_acc)) + 
  stat_sum(aes(col = loan_status))




#last payment amnt
ggplot(data = loan_data[loan_data$loan_status=='Charged Off',], aes(y = (last_pymnt_amnt/loan_amnt) *100, x = grade)) + 
  geom_point( aes(col = annual_inc), position = position_jitter(width = 0.4), size = 1) + ylab("Percentage of total loan amount paid")

#month since last public record
ggplot(data = loan_data, aes(x = mths_since_last_record)) + geom_histogram(binwidth = 1, aes(fill = loan_status), position = 'stack')


#dti
quantile(x = loan_data$annual_inc, c())
ggplot(data = loan_data[loan_data$loan_status=="Charged Off" & loan_data$annual_inc<187000,], aes(y = dti, x = annual_inc)) + geom_point(aes(col = loan_status))

# Loan amounts is a statistically significant driver of loan default at p < 2.2e-16
# Null hypothesis is that there is no difference in the mean values of the loan amount for defaulted and fully paid loans
# We can reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level
# 2 sample t and z tests give the same results for the samples

t.test(loan_data_charged_off$loan_amnt, 
       loan_data_fully_paid$loan_amnt, 
       paired=FALSE, 
       conf.level=0.95)

# Funded amount is a statistically significant driver of loan default at p < 2.2e-16
# Null hypothesis is that there is no difference in the mean values of the funded amount for defaulted and fully paid loans
# We can reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$funded_amnt, 
       loan_data_fully_paid$funded_amnt, 
       paired=FALSE, 
       conf.level=0.95)

# Funded amount by investors is a statistically significant driver of loan default at p-value = 5.755e-12
# Null hypothesis is that there is no difference in the mean values of the funded amount by investors for defaulted and fully paid loans
# We can reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$funded_amnt_inv, 
       loan_data_fully_paid$funded_amnt_inv, 
       paired=FALSE, 
       conf.level=0.95)

# Monthly loan installment amount is a statistically significant driver of loan default at p-value = 5.755e-12
# Null hypothesis is that there is no difference in the mean values of the monthly loan installment amount for defaulted and fully paid loans
# We can reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$installment, 
       loan_data_fully_paid$installment, 
       paired=FALSE, 
       conf.level=0.95)

# Annual income of loan applicant is a statistically significant driver of loan default at p-value < 2.2e-16
# Null hypothesis is that there is no difference in the mean values of the Annual income of loan applicant for defaulted and fully paid loans
# We can reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$annual_inc, 
       loan_data_fully_paid$annual_inc, 
       paired=FALSE, 
       conf.level=0.95)

# Total amount received from loan applicant is a statistically significant driver of loan default at p value< 2.2e-16
# Null hypothesis is that there is no difference in the mean values of the Total amount received from loan applicant for defaulted and fully paid loans
# We reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$total_pymnt, 
       loan_data_fully_paid$total_pymnt, 
       paired=FALSE, 
       conf.level=0.95)

# Total amount received by investors from loan applicant is a statistically significant driver of loan default at p value< 2.2e-16
# Null hypothesis is that there is no difference in the mean values of the Total amount received by investors from loan applicant for defaulted and fully paid loans
# We reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$total_pymnt_inv, 
       loan_data_fully_paid$total_pymnt_inv, 
       paired=FALSE, 
       conf.level=0.95)

# Revolving Balance from loan applicant is a statistically insignificant driver of loan default at p value =0.2328
# Null hypothesis is that there is no difference in the mean values of the Revolving balance of loan applicant for defaulted and fully paid loans
# We fail to reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$revol_bal, 
       loan_data_fully_paid$revol_bal, 
       paired=FALSE, 
       conf.level=0.95)

# Revolving Utilization from loan applicant is a statistically significant driver of loan default at p value <2,2e-16
# Null hypothesis is that there is no difference in the mean values of the Revolving Utilization of loan applicant for defaulted and fully paid loans
# We reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$revol_util, 
       loan_data_fully_paid$revol_util, 
       paired=FALSE, 
       conf.level=0.95)

# Open credit lines from loan applicant is a statistically insignificant driver of loan default at p value =0.07947
# Null hypothesis is that there is no difference in the mean values of the open credit lines of loan applicant for defaulted and fully paid loans
# We fail to reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$open_acc, 
       loan_data_fully_paid$open_acc, 
       paired=FALSE, 
       conf.level=0.95)

# Total interest received till date from loan applicant is a statistically significant driver of loan default at p value =0.01564
# Null hypothesis is that there is no difference in the mean values of the total interest received of loan applicant for defaulted and fully paid loans
# We reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$total_rec_int, 
       loan_data_fully_paid$total_rec_int, 
       paired=FALSE, 
       conf.level=0.95)

# Total interest principle till date from loan applicant is a statistically significant driver of loan default at p value<2.2 e-16 
# Null hypothesis is that there is no difference in the mean values of the total principle received of loan applicant for defaulted and fully paid loans
# We reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level


t.test(loan_data_charged_off$total_rec_prncp, 
       loan_data_fully_paid$total_rec_prncp, 
       paired=FALSE, 
       conf.level=0.95)

# Total late fee recovered till date from loan applicant is a statistically significant driver of loan default at p value<2.2 e-16 
# Null hypothesis is that there is no difference in the mean values of the total late fee recovered of loan applicant for defaulted and fully paid loans
# We reject the Null hypothesis based on the p value (applying 2 sided test) based on 5% significance level

t.test(loan_data_charged_off$total_rec_late_fee, 
       loan_data_fully_paid$total_rec_late_fee, 
       paired=FALSE, 
       conf.level=0.95)

