
#' SCE: Survey of consumer expectations
#'
#' Data extracted from the Survey of consumer expectations and the labour market survey of the Federal Reserve Bank of New York.
#'
#' Expectation is taken from Question oo2e2: "What do you believe your annual earnings will be in 4 months?(dollars per year)"
#'
#' Income is extracted from Question l3: "How much do you make before taxes and other deductions at your [main/current] job, on an annual basis?
#' Please include any bonuses, overtime pay, tips or commissions. (dollars per year)"
#'
#' Labour market survey from https://www.newyorkfed.org/microeconomics/databank.html. Part of the Surevey of Consumer expectations.
#'
#' Questionaire can be found under https://www.newyorkfed.org/medialibrary/media/research/microeconomics/interactive/downloads/sce-labor-questionnaire.pdf.
#'
#' @source Source: Survey of Consumer Expectations, © 2013-2020 Federal Reserve Bank of New York (FRBNY). The SCE data are available without charge at http://www.newyorkfed.org/microeconomics/sce and may be used subject to license terms posted there. FRBNY disclaims any responsibility for this analysis and interpretation of Survey of Consumer Expectations data.
#'
"sce"



#' real GDP Greenbook forecasts (1967-2015)
#'
#' A dataset containing quarterly real GDP growth rates (annualized percentage points) in the United States and according one quarter ahead point forecasts from the Federal Reserve's Greenbook.
#'
#' Often, multiple forecasts are issued in one quarter,
#' as the Federal Board meets more than 4 times year
#' and in irregular time spans.
#'
#' The forecasts are available as the first and last
#' forecast issued in any quarter. Additionally,
#' the column FC.middle denotes the forecast issued
#' closest to the middle of the quarter.
#'
#' @format A data frame with 176 rows and 2 variables:
#' \itemize{
#'   \item date.target: date of the target quarter (1967.2 -- 2015.1)
#'   \item FC.first, FC.middle, FC.last: Forecasts issued first, closest to the middle, and last in the respective quarter.
#'   \item Obs.first, Obs.second, Obs.third, Obs.recent: realized GDP growth rate in percentage measured at first, second, third, or most recent vintage
#' }
#'
#' @source \url{https://www.philadelphiafed.org/research-and-data/real-time-center/greenbook-data/philadelphia-data-set}
"GDP"


#' Exchange rates from the European Central Bank's Statistical Data Warehouse (SDW)
#'
#' A dataset containing several exchange rates as denoted by the European Central Bank's Statistical Data Warehouse (SDW).
#' The data includes:
#' \itemize{
#'  \item USD: US-Dollar
#'  \item GBP: British Pound
#'  \item JPY: Japanese Yen
#'  \item CAD: Canadian Dollars
#'  \item CHF: Suisse Franks
#'  \item AUD: Australian Dollars
#'  \item CNY: Chinese Yuan Renminbi
#'  }
#'
#'
#'
#' @source \url{https://sdw.ecb.europa.eu/}
#'
#' @examples
#' #Subset by
#' exchange[exchange$to=="USD",]
"exchange"

