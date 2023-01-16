library(dplyr)

# load data ------------------------------------------------------------------------------------------------------
# https://www.newyorkfed.org/microeconomics/databank.html
dat <- readxl::read_excel(path = "./data-raw/SCE/sce-labor-microdata-public.xlsx",
                          sheet = 3,skip = 1,guess_max = 5000)
dim(dat)
length(unique(dat$userid))

# observation number expectations
sum(!is.na(dat$oo2e2))
#observation number income
sum(!is.na(dat$l3))
# observation number joint
sum(!is.na(dat$l3)&!is.na(dat$oo2e2))


#rename l3 as income and oo2e2 as expectation
dat <- rename(dat, income = l3, expectation = oo2e2)

range(dat$date)


# load sce data (main survey) and merge -----------------------------------
dat_main1 <- readxl::read_excel(path = "./data-raw/SCE/Microdata-13-16.xlsx",
                          sheet = 1,skip = 1,guess_max = 5000)
dat_main2 <- readxl::read_excel(path = "./data-raw/SCE/Microdata-17-present.xlsx",
                               sheet = 4,skip = 1,guess_max = 5000)


dat_main <- plyr::rbind.fill(dat_main1,dat_main2)

nrow(dat_main)
n_distinct(dat_main$userid)

# keep first interview only
dat_main <- dat_main %>% group_by(userid) %>% arrange(survey_date) %>% filter(row_number()==1)

dat_main$age <- dat_main$Q32
dat_main$age[dat_main$age %in% c(0,1)] <- NA


dat_main$age_cat <- dat_main$`_AGE_CAT`
dat_main$edu_cat <- dat_main$`_EDU_CAT`
dat_main$num_cat <- dat_main$`_NUM_CAT`
dat_main$hhinc_cat <- dat_main$`_HH_INC_CAT`


dat_main$female <- dat_main$Q33==1
dat_main$hispanic <- dat_main$Q34==1
dat_main$white <- ifelse(dat_main$hispanic, FALSE, dat_main$Q35_1==1)

dat_main$edu <- dat_main$Q36

# and 8 "professional degree" to average
#dat_main$edu[dat_main$edu==8]<-4

dat_main$edu_high <- dat_main$edu>=5 #bachelor and more

dat_main$married <- dat_main$Q38==1

dat_main$better.past <- dat_main$Q1-3
dat_main$better.future <- dat_main$Q2-3

dat_main_small <- dat_main %>%
  select(userid,date,age,female,hispanic,white,edu,edu_high,married,
         num_cat,hhinc_cat,edu_cat,age_cat,better.future,better.past)

### merge

#compute date
dat$rdate <- as.Date(paste(dat$date,1,sep="-"),"%Y%m-%d")
range(dat$rdate)
unique(dat$rdate)
dat_main_small$rdate <- as.Date(paste(dat_main_small$date,1,sep="-"),"%Y%m-%d")
range(dat_main_small$rdate)
unique(dat_main_small$rdate)


datm <- dat %>% left_join(dat_main_small,by = c("userid"),suffix = c("",".main"))


length(unique(datm$userid))
length(unique(dat$userid))

dim(dat)
dim(datm)

mean(is.na(datm$age))
mean(is.na(datm$married))

dat <- datm

# add instruments ------------------------------------------------------------------------------------------------------

# self employed
dat$self_employed <- dat$l11==2
dat$work_so_else <- dat$l11==1

dat$gov <- dat$lmtype==1
dat$gov_nonprofit <- dat$lmtype==1 | dat$lmtype ==3
dat$privat <- dat$lmtype==2


# CAREFUL: own interpretation of blue and white collar jobs!
dat$bluecollar <- dat$lmind<=5
dat$whitecollar <- dat$lmind>5 & dat$lmind <=13

# CAREFUL: this is only the experience in THIS job
dat$experience <- as.numeric(substr(dat$date,1,4))-dat$l7by

# 1 if somehow did anything in the last 4 weeks to look for work
dat$looking_for_job <- (dat$js5 %in% c(1,2))

# amount and number of job offers received
dat$job_offers_binary <- (dat$nl1 >= 1)
dat$job_offers_number <- dat$nl1

# anticipated percentage chance to work for different employer
dat$percentage_different_employer <- dat$oo1_3

# anticipated percentage chance to stay at current job
dat$percentage_same_job <- dat$oo1_2

# score (1-5) if satisfied with the money/ or overall in the job
dat$satisfaction_money <- dat$lmsat1
dat$satisfaction_all <- dat$lmsat5



# Match realizations and forecasts -------------------------------------------------------------------------------

#select data
# df <- distinct(dat, expectation, income, date, userid, gov, privat, bluecollar, whitecollar, self_employed, work_so_else,
#                experience, looking_for_job, job_offers_binary, job_offers_number, percentage_same_job, satisfaction_money, satisfaction_all, lmtype, lmind)

dat$date <- as.numeric(substr(dat$date,1,4))+as.numeric(substr(dat$date,5,6))/12
df <- dat

#safe old income (known when forecasting)
df$lagincome <- df$income
df$log_lagincome <- pmax(log(df$lagincome), -100) # replace -Inf by -100


#prepare new income (target of forecast)
df$income <- NA
df$date_income <- NA

#df <- tail(df,200)

# numeric date number for the following loop
dates.hlp.numeric <- as.numeric(as.Date(paste(dat$date,1,sep="-"),"%b %Y-%d"))
date_list <- unique(as.Date(paste(dat$date,1,sep="-"),"%b %Y-%d"))

# count number of observations for userid
df <- df %>% group_by(userid) %>% mutate(num_surveys = length(unique(date)),
                                         count = n(),
                                         rank = rank(date))

#match income from dat to expectation from df
for(id in unique(dat$userid))
{
  # for(date in unique(as.Date(paste(dat$date[dat$userid==id],1,sep="-"),"%b %Y-%d")))
  for(date in unique(dat$date[dat$userid==id]))
  {

    # expectation entry in df
    pos <- which(df$date==date & df$userid==id)

    #income entry in dat four months later
    match <- which( abs(as.numeric(dat$date)-(date+1/3))<0.2 & dat$userid==id)
    #rdate <- unique(dat$rdate[dat$date==date])
    #if(length(rdate)>1)
    #  stop("doesn't work")
    #match <- which(abs(as.numeric(dat$rdate-(rdate+lubridate::month(4)))) <=20 & dat$userid==id)
    #match <- which( abs( dates.hlp.numeric - as.numeric(as.Date(paste(date,1,sep="-"),"%b %Y-%d")+months(4)) )<0.1 & dat$userid==id)
    # if(!identical(match_old,match))
    #   stop("not same")

    if( length(pos) > 1 | length(match) > 1 )
      stop("double")

    if(length(match)>0) {
      #add future income
      df$income[pos]<-dat$income[match]
      #add date
      df$date_income[pos] <- dat$date[match]
    }

  }
}




dim(df)
length(unique(df$userid))

dim(df[!complete.cases(df[,c("expectation","income")]),])
range(df[!complete.cases(df[,c("expectation","income")]),][,c("date")])
length(unique(df[!complete.cases(df[,c("expectation","income")]),]$userid))

table((df[complete.cases(df[,c("expectation","income")]),][,c("rank","date")]))

table((df[!complete.cases(df[,c("expectation","income")]),][,c("count","date")]))

#drop incomplete cases
df <- df[complete.cases(df[,c("expectation","income")]),]
dim(df)

#add forecast error
df$e <- df$expectation-df$income



# subsets patrick ---------------------------------------------------------

df$ss_old <- df$age > median(df$age,na.rm = TRUE)
df$ss_highincome <- df$lagincome > median(df$lagincome,na.rm = TRUE)
df$ss_married <- df$married
df$ss_eduhigh <- df$edu_high
df$ss_white <- df$white
df$ss_privat <- df$privat
#df$ss_selfemployed <- df$self_employed
df$ss_goverment <- df$gov
df$ss_sameemployer100 <- df$oo1_2==100
df$ss_newemployer10 <- df$oo1_3>=10
df$ss_jobofferspast <- df$job_offers_binary
df$ss_joboffersexpect1 <- df$oo2new>=1
df$ss_looking <- df$looking_for_job


# Filter Timo -----------------------------------------------------------------------------------

# Delete zero income and zero expectation data
df <- df[!df$income==0,]
df <- df[!df$expectation==0,]

# Use Log "returns"
df$elog <- log(df$expectation) - log(df$income)


# Filter 1:
# Delete the following:
# - below 1.000 income and expectation
# - above 1.000.000 income and expectation
filter1 <- !((df$income<1000) | (df$expectation<1000) | (df$expectation>1000000) | (df$income>1000000) )
df$filter1 <- filter1


# Filter 2:
# Delete the following:
# - below 10.000 income and expectation
# - above 200.000 income and expectation
filter2 <- !((df$income<10000) | (df$expectation<10000) | (df$expectation>200000) | (df$income>200000) )
df$filter2 <- filter2


# Filter 3:
# Delete the following:
# -filter: 9 < |e/income| < 13
# -filter: 9 < |e/expectation| < 13
income_is_factor10 <- ((abs(df$e/df$income) > 9) & (abs(df$e/df$income) < 13))
expectation_is_factor10 <- ((abs(df$e/df$expectation) > 9) &  (abs(df$e/df$expectation) < 13))
filter3 <- !( income_is_factor10 | expectation_is_factor10 )
df$filter3 <- filter3


# Filter 4
# Delete the following:
# -filter: |e/income| > 8
# -filter: |e/expectation| > 8
income_factor_ge8 <- (abs(df$e/df$income) > 8)
expectation_factor_ge8 <- (abs(df$e/df$expectation) > 8)
filter4 <- !( income_factor_ge8 | expectation_factor_ge8 )
df$filter4 <- filter4


# Filter 5
# Delete the following:
# - Multiple observations of the same id but the FIRST
index_sort_userid_date <- with(df, order(userid,date))
df_sorted <- df[index_sort_userid_date,]
filter5 <- !duplicated(df_sorted$userid, fromLast = FALSE)
df$filter5 <- filter5


# Filter 6
# Delete the following:
# - Multiple observations of the same id but the LAST
index_sort_userid_date <- with(df, order(userid,date))
df_sorted <- df[index_sort_userid_date,]
filter6 <- !duplicated(df_sorted$userid,fromLast = TRUE)
df$filter6 <- filter6


# Save data as sce.income
sce <- as.data.frame(df)
usethis::use_data(sce,overwrite = TRUE)





