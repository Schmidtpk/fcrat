#' Creates instrument matrix from character vector
#'
#' @return list containing forecast x, observation y, and instrment marix w
#'
#' @examples
#' create.instrument.matrix(c(0.1,0.2,0.3),c(1.1,1.2,1.3), c("1","X","lag(Y)"))
create.instrument.matrix <- function(X,Y,instruments,other_data=NULL,silent=TRUE)
{

  if (class(instruments) != "character") {stop("instruments need character descrition")}

      # add other data if necessary
      if (is.null(other_data)) {
        other_data <- data.frame(X = X, Y = Y)
      } else {
        if("X" %in% colnames(other_data) | "Y" %in% colnames(other_data))
          warning("name already exists")

        if(nrow(other_data)!=length(Y))
          warning("Y was shortened. Check NA mistake!")

        other_data <- cbind(data.frame(X = X, Y = Y),
                            other_data)
      }


      # add lower case, such that instrument descriptions can be lower case
      other_data$x <- other_data$X
      other_data$y <- other_data$Y


      #create instruments matrix w
      w<-NULL
      for (inst_cur in instruments) {
        if (grepl("Y", inst_cur,ignore.case = TRUE) & !grepl("lag", inst_cur))
          warning("Y without lags is not a valid instrument as it is not in the information set of the forecaster.")

        #add constant or apply string
        if(inst_cur %in% c("const","1","constant"))
          w <- cbind(w, rep(1,length(Y)))
        else
          w <- cbind(w, eval(parse(text = inst_cur), other_data))
      }

      #drop incomplete cases
      compl <- complete.cases(data.frame(w,X,Y))

      if(!silent)
        message(paste("Drop ", length(Y) - sum(compl), "case(s) because of chosen instruments"))

      w <- w[compl,]
      Y <- Y[compl]
      X <- X[compl]

 return(list(x=X,y=Y,w=as.matrix(w)))
}


#' Tests mean median and mode rationality
#'
#' Tests mean median and mode for given instruments. Allows to specifiy instruments, covariance matrix estimation, an asymptotic identification function, and the bandwidth choice.
#'
#' @param x forecast
#' @param y observation
#' @param instruments lists of character variables describing the instruments
#' (also takes a list of lists if several instruments choices should be considered simultanuously)
#' @param vcov covariance estimator
#' @param bw standard NULL (then id_modegaussion function chooses bw_rule)
#' @param other_data matrix can be provided to use additional instruments
#'
#' @return returns data.frame of p-values
#'
#' @export
test_central <- function(x,y,
                         instruments=c("1","X"),
                         vcov=iid,
                         bw=NULL,
                         other_data=NULL,
                         id_mode=id_modegaussian)
{

  if(all(sapply(instruments,length)==1))
    instruments <- list(instruments)

  if(is.function(bw))
    bw <- bw(x,y)

  res <- NULL
  for (inst.cur in instruments) {

      data <- create.instrument.matrix(x,y,inst.cur,other_data = other_data)

      RatMean <- rationality.test(Y=data$y, X=data$x, id.fct=id_mean, instruments=data$w, vcov=vcov)
      RatMedian<- rationality.test(Y=data$y, X=data$x, id.fct=id_median, instruments=data$w, vcov=vcov)
      RatMode <- rationality.test(Y=data$y, X=data$x, id.fct=id_mode, instruments=data$w, bw=bw, vcov=vcov)
      res <- rbind(res,c(paste(inst.cur,collapse  = "|"),
                         round(RatMean$pval,3),
                         round(RatMedian$pval,3),
                         round(RatMode$pval,3)))
  }

  colnames(res)<-c("instruments", "mean", "median","mode")

  return(as.data.frame(res))
}


#' Computes conditional calibration test
#'
#' @param Y observation
#' @param X forecast
#' @param id.fct identification that determines the functional that is tested for
#' @param instruments instruments that determine the information tested for
#' @param vcov covariance estimation technique. Either a character that is among the predefined methods
#' or a function that has ids (matrix of identification function values) as input.
#' @param bw numeric, function or vector. optional if iden fct requires bandwidth.
#'
#' @return
#' @export
rationality.test <- function(Y,X, id.fct=id_mean, instruments=NULL, vcov = iid, bw=NULL,...) {


  # Specify instruments
  n <- length(Y)
  if (is.null(instruments)) {
    instruments = matrix(1,nrow=n,ncol=1)
  } else if(is.character(instruments))
  {
    dat_all <- create.instrument.matrix(X,Y,instruments)
    instruments <- dat_all$w
    Y <- dat_all$y
    X <- dat_all$x
  }

  #compute dof
  J_DoF <- dim(instruments)[2]

  #call with or without bw
  if(is.null(bw))
    id_values <- id.fct(Y=Y,X=X)
  else
    id_values <- id.fct(Y=Y,X=X,bw=bw)

  id_inst_values <- id_values * instruments
  mean_id_values <- colMeans(id_inst_values)

  if(!is.function(vcov))
    stop("vcov parameter is not a function.")

  var_id <- tryCatch(vcov(ids = id_inst_values), error = function(e) NA)
  vcov <- var_id$name
  var_id <- var_id$cov

  J <- tryCatch(n * mean_id_values %*% solve(var_id) %*% mean_id_values, error=function(e) NA)
  pval <- tryCatch(1-pchisq(J,J_DoF), error=function(e) NA)

  return( list(J=J, pval=pval, mean_id= mean_id_values, var_id=as.numeric(diag(var_id)), vcov = vcov))
}


#' Identification function of mean
#'
#' @param Y observation
#' @param X forecast
#'
#' @export
id_mean <- function(Y,X) {
  return( (X-Y) )
}




#' Identification function of median
#'
#' @param Y observation
#' @param X foreast
#'
#' @export
id_median <- function(Y,X) {
  return( (Y<X) - (Y>X) )
}


#' Asymptotic identification functions of mode based on Gaussian kernel
#'
#' @param Y observation
#' @param X foreast
#' @param bw bandwidth. Numeric or function. (standard selects autamtically with rule of thumb)
#'
#' @export
id_modegaussian <- function(Y, X, bw=bw_rule) {

  if(sum(is.na(c(Y,X))>0))
     warning("NA values in data for id_mode function")

  if(is.function(bw))
    bw <- bw(Y,X)

  if(sum(is.na(bw))>0)
    stop("bandwith selection failed: NA")

  if(!is.numeric(bw))
    stop("bandwith not a number")

    return(((X-Y)/bw^2) * dnorm((X-Y)/bw))
}



#' Asymptomatic identification functions of mode based on Bi-square kernel
#'
#' @param Y observation
#' @param X foreast
#' @param bw bandwidth. Numeric or function. (standard selects autamtically with rule of thumb)
#'
#' @export
id_mode_BiSquare <- function(Y, X, bw=bw_rule) {

  if(sum(is.na(c(Y,X))>0))
    warning("NA values in data for id_mode_BiSquare function")

  if(is.function(bw))
    bw <- bw(Y,X)

  if(sum(is.na(bw))>0)
    stop("bandwith selection failed: NA")

  if(!is.numeric(bw))
    stop("bandwith not a number")

  u <- (Y-X)/bw
  return(- 1/bw^2 * 15/4 * u * (1-u^2) * (abs(u) <= 1))
}


#' Bandwidth selection with rule of thumb
#'
#' @param Y observation
#' @param X foreast
#'
#' @return bandwidth as numeric
#' @export
bw_rule <- function(Y, X) {

  n <- length(Y)
  MAD <- median(abs(X-Y-median(X-Y)))
  skew <- abs(mean(X-Y) - median(X-Y)) / sd(X-Y)
  skew_factor <- exp(-9*skew)

  return(2.4 *skew_factor * MAD * n^(-0.143))
}

