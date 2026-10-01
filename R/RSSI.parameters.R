# Generate gamma distribution parameters (shape and scale) across an RSSI range

RSSI.parameters<- function(model,buffer=40) {

  mf<- model.frame(model)

  if(ncol(mf) != 2) {
    stop("The model must contain exactly one predictor.")
  }

  rssi<- mf[[2]]

  if(!is.numeric(rssi)) {
    stop("The predictor must be numeric.")
  }

  predictor_name<- names(mf)[2]

  param_df<- data.frame(seq(min(rssi, na.rm = TRUE) - buffer,
                 max(rssi, na.rm = TRUE) + buffer,by = 1))

  names(param_df)<- predictor_name

  Shape<- fitted(model, newdata=param_df, dpar ="shape")[,"Estimate"]
  Mu<- fitted(model, newdata = param_df)[,"Estimate"]

  param_df$Shape<- Shape
  param_df$Lambda<- Mu / Shape
  param_df$Rate<-1/(param_df$Lambda)

  param_df<-param_df[,!names(param_df)%in%c("Lambda"),drop=FALSE]

  # Return a dataframe with a shape and rate for each RSSI value
  param_df
}
