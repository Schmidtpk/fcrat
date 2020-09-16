library(lubridate)
library(openxlsx)
library(zoo)


# Load GDP Observations
Y <- read.xlsx("./data-raw/GDP/routput_first_second_third.xlsx", sheet=2, startRow = 5)
Y[,-1] <- t(apply(Y[,-1], 1,function(x) as.numeric(as.character(x))))    # convert to numeric
Y <- na.locf(Y)
Y$Date <- gsub(":Q",".",Y$Date)
names(Y)[names(Y) == "First"] <- "Obs.first"
names(Y)[names(Y) == "Second"] <- "Obs.second"
names(Y)[names(Y) == "Third"] <- "Obs.third"
names(Y)[names(Y) == "Most_Recent"] <- "Obs.recent"

# Load GDP Greenbook Forecasts
X <- read.xlsx("./data-raw/GDP/GBweb_Row_Format.xlsx", 2)[,c("DATE","gRGDPF1", "GBdate")]
X$gRGDPF1 <- as.numeric(as.character(X$gRGDPF1))
# format date
X$date <- as.Date(as.character(X$GBdate),"%Y%m%d")
X$date.target <- parse_date_time(X$DATE,orders = "%Y.%q") + months(3)
X$date.target <- paste0(year(X$date.target),".",quarter(X$date.target))


# Choose forecast closest to middle of quarter
year(X$date) <- 2000
X$comp.middle <- as.Date("02152000","%m%d%Y")
X$comp.middle[(month(X$date)>3)] <- as.Date("05152000","%m%d%Y")
X$comp.middle[(month(X$date)>6)] <- as.Date("08152000","%m%d%Y")
X$comp.middle[(month(X$date)>9)] <- as.Date("11152000","%m%d%Y")
X$diff.middle <- abs(X$comp.middle - X$date)

# Choose first forecast
X$comp.first <- as.Date("01012000","%m%d%Y")
X$comp.first[(month(X$date)>3)] <- as.Date("04012000","%m%d%Y")
X$comp.first[(month(X$date)>6)] <- as.Date("07012000","%m%d%Y")
X$comp.first[(month(X$date)>9)] <- as.Date("10012000","%m%d%Y")
X$diff.first <- abs(X$comp.first - X$date)

# Choose last forecast in quarter
X$comp.last <- as.Date("03312000","%m%d%Y")
X$comp.last[(month(X$date)>3)] <- as.Date("06302000","%m%d%Y")
X$comp.last[(month(X$date)>6)] <- as.Date("09302000","%m%d%Y")
X$comp.last[(month(X$date)>9)] <- as.Date("12312000","%m%d%Y")
X$diff.last <- abs(X$comp.last - X$date)

#drop incomplete cases
X <- X[complete.cases(X),]

X <- transform(X, date.rank.first = ave(diff.first, DATE, FUN = function(x) rank(x, ties.method = "first")))
X <- transform(X, date.rank.middle = ave(diff.middle, DATE, FUN = function(x) rank(x, ties.method = "first")))
X <- transform(X, date.rank.last  = ave(diff.last, DATE, FUN = function(x) rank(x, ties.method = "last")))

# drop other forecasts, rename and drop redundant variables
X.first <- X[X$date.rank.first==1,]
names(X.first)[names(X.first) == "gRGDPF1"] <- "FC.first"
names(X.first)[names(X.first) == "date"] <- "date.first"
X.first <- X.first[ ,c('DATE','date.first','date.target','FC.first')]

X.middle <- X[X$date.rank.middle==1,]
names(X.middle)[names(X.middle) == "gRGDPF1"] <- "FC.middle"
names(X.middle)[names(X.middle) == "date"] <- "date.middle"
X.middle <- X.middle[ ,c('DATE','date.middle','date.target','FC.middle')]

X.last <- X[X$date.rank.last==1,]
names(X.last)[names(X.last) == "gRGDPF1"] <- "FC.last"
names(X.last)[names(X.last) == "date"] <- "date.last"
X.last <- X.last[ ,c('DATE','date.last','date.target','FC.last')]

# Merge data frames
X.all <- merge(X.first, X.middle, by=c("DATE","date.target"))
X.all <- merge(X.all, X.last, by=c("DATE","date.target"))

GDP <- merge(X.all, Y, by.x='date.target', by.y='Date')
names(GDP)[names(GDP) == "DATE"] <- "Q.FC.Issued"
GDP$date.target <- as.numeric(GDP$date.target)

# Save data as GDP
usethis::use_data(GDP,overwrite = TRUE)




