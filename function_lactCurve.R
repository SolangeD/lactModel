# These two functions are used to calculate coefficients and plot the lactation curve of mountain-pastured cows

# They are used in the article
# Modelling lactation curves of mountain-pastured Braunvieh cows in Switzerland: influence of morphological and environmental factors. S. Duruz et al.

# These two functions can also be used with other data to model lactation curve (of mountain-pastured cows but not necessarily)



#Calculate coefficients
#' @title Calculate coefficients to model lactation curve of (mountain-pastured) dairy cows
#' @details The modelling is based on three types of possible base model (see \code{model}), and adaptation is done for mountain-pastured cows: 1) increased linear decrease during high alpine grazing 2) sharp increase after the transhumance is over 3) smoother decrease for the end of the lactation
#' @param model char One of 'AS' (for Ali-Schaeffer), 'Wilmink','Wood' to describe the type of regression
#' @param dataInput dataframe The dataframe containing the observations in rows with the columns as givenin y_field, t_field etc...
#' @param y_field char The name of the column in \code{dataInput} that corresponds to the response field (typically milk)
#' @param t_field char The name of the column in \code{dataInput} that corresponds to the time of observation (typically days in milk)
#' @param w_field char The name of the column in \code{dataInput} that corresponds to the weight of the observation (typically the number of cows when working on average of cows)
#' @param t1_field char The name of the column in \code{dataInput} that corresponds to the time (same unit as \code{t_field}) at which the time is alped. If groups of cows are taken, this time must be different for each calving season. If NULL, the base model as presented by AS/Wood/Wilmink will be returned
#' @param g_field char The name of the column in \code{dataInput} that corresponds to the field for which we want the alp term (or all terms if \code{fullInteraction} is TRUE) to vary
#' @param fullInteraction boolean If FALSE only the alp term will vary according to \code{g_field}. If TRUE all terms are allowed to vary according to \code{g_field}.
#' @param k_wilmink real If model='Wilmink', the value of the k coefficient. By defautl, 0.1. 
#' @param diff_value char The possible value that \code{g_field can have}. The first value will be taken as the reference value
#' @param endCurve boolean Whether the end of the curve (after high alpine grazing is over) should be plotted or not
#' @param alpDuration integer The duration of the high alpine grazing season (same unit as \code{t_field})
#' @examples 
#' 
calc_coeff=function(model, dataInput, y_field, t_field, w_field=NULL,t1_field=NULL, g_field=NULL, fullInteraction=FALSE, k_wilmink=0.1, diff_value=NULL, endCurve=FALSE, alpDuration=115){
  #Parameter check
  if(!(model %in% c('AS','Wilmink','Wood'))) stop("model must be one of 'AS' (for Ali-Schaeffer), 'Wilmink','Wood'")
  if(!(y_field %in% colnames(dataInput))) stop("y_field must be a column name of dataInput")
  if(!(t_field %in% colnames(dataInput))) stop("t_field must be a column name of dataInput")
  if(!is.null(w_field)){
    if(!(w_field %in% colnames(dataInput))) stop("w_field must be a column name of dataInput or set to NULL")
  }
  if(!is.null(t1_field)){
    if(!(t1_field %in% colnames(dataInput))) stop("t1_field must be a column name of dataInput or set to NULL")
    if(model=='Wood') stop("The model cannot be Wood if alp term must be calculated (not linear regression)")
  }
  if(!is.null(g_field)){
    if(!(g_field %in% colnames(dataInput))) stop("g_field must be a column name of dataInput or set to NULL")
  }
  if(!is.logical(fullInteraction)) stop("fullInteraction supposed to be boolean")
  if(!is.null(k_wilmink)){
    if(!is.double(k_wilmink)) stop("k_wilmink supposed to be double")
  }
  if(!is.null(endCurve)){
    if(!is.logical(endCurve)) stop("endCurve supposed to be boolean")
  }
  if(!is.null(alpDuration)){
    if(!is.double(alpDuration)) stop("alpDuration supposed to be integer")
  }  
  
  # Assign columns of dataInput to variables (otherwise problem to predict the returning values)
  y_obs=dataInput[,y_field]
  t_obs=dataInput[,t_field]
  if(!is.null(w_field)){
    w_field=dataInput[,w_field]
  }
  
  # Models are discribed in Macciota et al 2005, Detection of different shapes of lactation curve for milk yield in dairy cattle by empirical mathematical models 
  if(model=='AS'){
    #Ali-Schaeffer
    base_model=lm(y_obs~1+I(t_obs/340)+I((t_obs/340)^2)+I(log(340/t_obs))+I((log(340/t_obs))^2), weights = w_field)
  } else if (model=='Wilmink'){
    #Wilmink
    base_model=lm(y_obs~1+I(exp(-k_wilmink*t_obs))+ t_obs,weights =w_field)
  } else if (model=='Wood'){
    # Wood
    #Careful, the result will be log(y)
    base_model=lm(I(log(y_obs))~1+I(log(t_obs))+t_obs,weights = w_field)
  } else {
    stop('Model unknown')
  }
  if(is.null(t1_field)){
    return(base_model) #If t1 is not given, the base model (AS, Wilmink...) is returned
  } else {
    # Add the alp term
    t1_obs=dataInput[,t1_field]
    alp_model=update(base_model, .~.+I(pmax(t_obs-t1_obs,0))) 
    if(endCurve==TRUE){
      # If model also the end of the curve, then the alp-model should be updated to add the increase at 
      alp_model=update(alp_model, .~.+I(pmax(ceiling((t_obs-(t1_obs+alpDuration))/305),0))+I(pmax(t_obs-(t1_obs+alpDuration),0)))
    }
    if(is.null(g_field)){
      return(alp_model)
    } else {
      if(!is.null(diff_value)){
        g_field=relevel(as.factor(dataInput[,g_field]), ref = diff_value[2])
      } else {
        g_field=as.factor(dataInput[,g_field])
      }
      #g_field=dataInput[,g_field]
      if(fullInteraction==FALSE){
        # Add only an interaction between g_field and alp term
        diff_model=update(alp_model, .~.+g_field:I(pmax(t_obs-t1_obs,0)))
        if(endCurve==TRUE){
          diff_model=update(diff_model, .~.+g_field:I(pmax(ceiling((t_obs-(t1_obs+alpDuration))/305),0))+g_field:I(pmax(t_obs-(t1_obs+alpDuration),0)))
        }
        return(diff_model)
      } else {
        # Add full interaction
        interaction_model=update(alp_model, .~g_field*.)
        #interaction_model=lm(y_obs~1*g_field+I(exp(-k_wilmink*t_obs))*g_field+ t_obs*g_field,weights =w_field)
        return(interaction_model)
      }
    }
  }
}


#Plot and Model the curve

#' @title Calculate the coefficients of a regression models fitting laction curve of alped cows
#' @description Calculate the coefficients of a regression models fitting laction curve of alped cows. Several base models can be used (see model) and an additional term is put to take the alping into account
#' @author Solange Duruz
#' @param dataInput a matrix containing lines observations at different days in milk. 
#' @param y_field char The name of the column containing the y-observation (typically milk yield but could also be protein yield)
#' @param t_field char The name of the column containing the time of the observation (typically in days in milk)
#' @param t1_field char The name of the column containing the time at which the cow (or group of cow) is alped (same unit as t_field)
#' @param g_field char The name of the column containing the 
#' @param w_field char The name of the column containing the weight of the observation (typically the number of cows when we consider groups of cows)
#' @param completeInteraction logical Whether or not the model should contain interactions g_field and all terms of the model. If false, only interaction between g_field and parameter describing alping is taken into account
#' @param plot logical Whether the result should be plotte (uses function plot_f)
#' @param model One of 'AS', '', '' for Ali-Schaeffer model, ...
#' @return 
#' @examples
#' #
#' 
plot_f=function(sql_result, y_field, t_field, cm_field, diff_field=NULL, diff_value=c(1,0), month=c(12,11,10,9,8,3,2,1), ylabel='Milk yield [kg]', xlabel='Days in milk', pal='Blues', predictionCol='gray', pch=c(1,19,3),prediction=TRUE, endCurve=FALSE, alpDuration=115,fullInteraction=FALSE, model='Wilmink', k_wilmink=0.1, t1_field, w_field=NULL, interactionMonth=FALSE, diffLegend=NULL){
  
  colors = brewer.pal(max(3,length(month)+1), pal)
  if(!is.null(diff_field)){
    #If diff_field (test difference among groups), select only the values present in diff_values (remove NA and other for example)
    sql_result=sql_result[sql_result[,diff_field]%in%diff_value,]
  }
  diff_value=as.character(diff_value)
  
  #Prepare the plot
  plot(NA, xlim=c(0,300), ylim=c(min(sql_result[,y_field], na.rm=TRUE),max(sql_result[,y_field], na.rm=TRUE)), xlab=xlabel, ylab=ylabel)
  
  j=2 # color selection. 1 is too pale
  
  #Weight calculation
  if(!is.null(w_field)){
    #Normalize weights between 0.5 and 1 (arbitrarly chosen values)
    w_plot=0.5+(sql_result[,w_field]-min(sql_result[,w_field], na.rm=TRUE))*0.5/(max(sql_result[,w_field], na.rm=TRUE)-min(sql_result[,w_field], na.rm=TRUE))
  } else {
    #If no weight, weight=1
    w_plot=rep(1,nrow(sql_result))
  }
  
  #Prepare empty arrays
  pval=vector('double',length=length(month))
  alp_coeff=vector('double',length=length(month))
  alpdiff_coeff=vector('double',length=length(month))
  if(!is.null(diff_field)){
    milk_alp=matrix(nrow=length(diff_value),ncol=length(month), dimnames=list(diff_value, month))
    milk_total=matrix(nrow=length(diff_value),ncol=length(month), dimnames=list(diff_value, month))
  } else {
    milk_alp=matrix(ncol=length(month), dimnames=list(1, month))
    milk_total=matrix(ncol=length(month), dimnames=list(1, month))    
  }
  for(i in month){
    if(prediction==TRUE){
      # Calculate the number of days in milk for each calving month where alping starts (middle of May for earliest)
      if(i<5){
        t1=155-i*30
      }  else {
        t1=275-(i-8)*30 
      }
    }
    if(is.null(diff_field)){
      # select observation of right month
      points(sql_result[sql_result[,cm_field]==i,y_field]~sql_result[sql_result[,cm_field]==i,t_field], col=colors[j], pch=19, cex=w_plot)
      if(prediction==TRUE){        # calculate model, predict value and draw predicted value
        if(interactionMonth==TRUE){ #To change the parameter values according to calving month (otherwise only t1 will differ from month to month)
          sql_result_sub=sql_result[sql_result[,cm_field]==i,]
        } else {
          sql_result_sub=sql_result
        }
        
        #Fit the model with the ad hoc function
        coeff=calc_coeff(model, sql_result_sub, y_field, t_field, t1_field=t1_field,w_field=w_field, fullInteraction=fullInteraction, diff_value=diff_value, endCurve=endCurve, k_wilmink=k_wilmink)
        #Identify the alp-term
        alp_coeff[j-1]=coeff$coefficients['I(pmax(t_obs - t1_obs, 0))']
        
        #Prediction and integration
        function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=t1)) 
        #Integrate value over alp period and whole lactation
        milk_alp[j-1]=as.double(integrate(function_predict,t1,t1+alpDuration)$value)
        milk_total[j-1]=as.double(integrate(function_predict,0,305)$value)
        #Predict the value over whole lactation (to draw line)
        pred_t=data.frame(t_obs=c(1:365), t1_obs=rep(t1, 365))
        pred_y=predict(coeff, pred_t) 
        lines(t(pred_t['t_obs']), pred_y, col=predictionCol)
        
      }
    } else { #diff_field is not null
      # select observation of right month and separate into two groups according to diff_field
      pch_num=1
      for(k in diff_value){
        points(sql_result[sql_result[,cm_field]==i & sql_result[,diff_field]==k,y_field]~sql_result[sql_result[,cm_field]==i & sql_result[,diff_field]==k,t_field], col=colors[j], pch=pch[pch_num], cex=w_plot)
        pch_num=pch_num+1
      }
      if(prediction==TRUE){
        #Will draw the prediction, and calculate the p-value of interaction diff_field x alp-term
        if(interactionMonth==TRUE){
          #Select only the month of the loop
          sql_result_sub=sql_result[sql_result[,cm_field]==i,]
        } else {
          sql_result_sub=sql_result
        }
        coeff=calc_coeff(model, sql_result_sub, y_field, t_field, t1_field=t1_field, k_wilmink=k_wilmink,w_field=w_field, g_field=diff_field, fullInteraction=fullInteraction, diff_value=diff_value, endCurve=endCurve)
        
        if(endCurve==TRUE){
          full_model=calc_coeff(model, sql_result_sub[sql_result_sub[,t_field]-sql_result_sub[,t1_field]<alpDuration,], y_field, t_field, t1_field=t1_field, k_wilmink=k_wilmink,w_field=w_field, g_field=diff_field, fullInteraction=fullInteraction, diff_value=diff_value, endCurve=FALSE)
        } else {
          full_model=coeff
        }
        
        #Isolate the alp term and the interaction diff_field x alp-term (might have several name, check which one exists)
        alp_coeff[j-1]=coeff$coefficients['I(pmax(t_obs - t1_obs, 0))']
        alpdiff_coeff[j-1]=coeff$coefficients[paste0('g_field',diff_value[1],':I(pmax(t_obs - t1_obs, 0))')]
        if(is.na(alpdiff_coeff[j-1])){
          alpdiff_coeff[j-1]=coeff$coefficients[paste0('g_field',diff_value[2],':I(pmax(t_obs - t1_obs, 0))')]
        }
        if(is.na(alpdiff_coeff[j-1])){
          alpdiff_coeff[j-1]=coeff$coefficients[paste0('I(pmax(t_obs - t1_obs, 0)):g_field',diff_value[1])]
        }
        if(is.na(alpdiff_coeff[j-1])){
          alpdiff_coeff[j-1]=coeff$coefficients[paste0('I(pmax(t_obs - t1_obs, 0)):g_field',diff_value[2])]
        }
        #Remove only last term
        null_model=update(full_model, .~.-g_field:I(pmax(t_obs - t1_obs, 0)))
        
        #???
        fterms = terms(coeff$call[[2]])
        fac = attr(fterms, "factors")
        new_fterms <- drop.terms(fterms, dropx = ncol(fac), keep.response = TRUE)
        #null_model=update(coeff,formula(new_fterms))
        
        #Calculate p-value
        pval[j-1]=lmtest::lrtest(full_model, null_model)$'Pr(>Chisq)'[2]
        
        k_i=1
        for(k in diff_value){
          #Predict and draw line 
          pred_t=data.frame(t_obs=c(1:365), t1_obs=rep(t1, 365), g_field=rep(k,365))
          pred_y=predict(coeff, pred_t) 
          lines(t(pred_t['t_obs']), pred_y, col=predictionCol)
          # Calculate the quantity of milk during alping
          function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=t1, g_field=k)) 
          milk_alp[k_i,(j-1)]=as.double(integrate(function_predict,t1,t1+alpDuration)$value)
          milk_total[k_i,(j-1)]=as.double(integrate(function_predict,0,305)$value)
          k_i=k_i+1
        }
        if(interactionMonth==FALSE){
          #break()
        }
      }
    }
    j=j+1
  }
  #Legend showing the month color and the prediction color 
  if(length(month)==1 & !is.null(diff_field)){
    legendc=c()
    colc=c()
    pchc=c()
  } else if (length(month)==1 & is.null(diff_field)) {
    legendc=c('observation')
    colc=c(colors[2])
    pchc=c(pch[1])
  } else {
    legendc=c('Calving month',month)
    colc=c('white',colors[2:length(colors)])
    pchc=c(15,rep(15, length(month)))
  }
  if(!is.null(diff_field)){
    if(is.null(diffLegend)) diffLegend=diff_field
    legendc=c(legendc,diffLegend,diff_value)
    colc=c(colc, 'white',rep(colors[as.integer(length(colors)/2)+1],length(diff_value)))
    pchc=c(pchc, 15, pch[1:length(diff_value)])    
  }
  if(prediction==TRUE){
    legendc=c(legendc,'Prediction','prediction')
    colc=c(colc, 'white','black')
    pchc=c(pchc, 15,95)        
  } 
  legend(x='topright', legend=legendc, cex=0.8, col=colc, pch=pchc, inset=0.02)
  if(!is.null(diff_field)){
    return(list('coeffs'=coeff,'alp_coeff'=alp_coeff,'alpdiff_coeff'=alpdiff_coeff,'pval'=pval, 'milk_alp'=milk_alp, 'milk_total'=milk_total, 'full_model'=full_model,'null_model'=null_model ))
  } else {
    return(list('coeffs'=coeff,'alp_coeff'=alp_coeff,'milk_alp'=milk_alp, 'milk_total'=milk_total))
    #a modifier
  }
  
  #legend(x='topright', legend=c('observation','prediction'), cex=0.8, col=c(colors[2],predictionCol), pch=c(19,175), inset=0.02)
  #legend(x='topright', legend=c(month, 'prediction'), cex=0.8, col=c(colors[2:length(colors)],predictionCol), pch=c(rep(19,length(colors)-1),175), title="Calving month", inset=0.02)
  
}