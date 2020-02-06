# These two functions are used to calculate coefficients and plot the lactation curve of mountain-pastured cows

# They are used in the article
# Modelling lactation curves of mountain-pastured Braunvieh cows in Switzerland: influence of morphological and environmental factors. S. Duruz et al.

# These two functions can also be used with other data to model lactation curve (of mountain-pastured cows but not necessarily)



#Calculate coefficients
#' @title Calculate coefficients to model lactation curve of (mountain-pastured) dairy cows
#' @details The modelling is based on three types of possible base model (see \code{model}), and adaptation is done for mountain-pastured cows: 1) increased linear decrease during high alpine grazing 2) sharp increase after the transhumance is over 3) smoother decrease for the end of the lactation
#' @author Solange Duruz
#' @param model char One of 'AS' (for Ali-Schaeffer), 'Wilmink','Wood' to describe the type of regression
#' @param dataInput dataframe The dataframe containing the observations in rows with the columns as givenin y_field, t_field etc...
#' @param y_field char The name of the column in \code{dataInput} that corresponds to the response field (typically milk)
#' @param t_field char The name of the column in \code{dataInput} that corresponds to the time of observation (typically days in milk)
#' @param w_field char The name of the column in \code{dataInput} that corresponds to the weight of the observation (typically the number of cows when working on average of cows)
#' @param t1_field char The name of the column in \code{dataInput} that corresponds to the time (same unit as \code{t_field}) at which the time is alped. If groups of cows are taken, this time must be different for each calving season. If NULL, the base model as presented by AS/Wood/Wilmink will be returned
#' @param diff_field char The name of the column in \code{dataInput} that corresponds to the field for which we want the alp term (or all terms if \code{fullInteraction} is TRUE) to vary
#' @param fullInteraction boolean If FALSE only the alp term will vary according to \code{diff_field}. If TRUE all terms are allowed to vary according to \code{g_field}.
#' @param k_wilmink real If model='Wilmink', the value of the k coefficient. By defautl, 0.1. 
#' @param diff_value char/integer vector The possible value that \code{diff_field can have}. The first value will be taken as the reference value
#' @param endCurve boolean Whether the end of the curve (after high alpine grazing is over) should be plotted or not
#' @param alpDuration integer The duration of the high alpine grazing season (same unit as \code{t_field})
#' @return An obeject of class "lm", as the result of the lm() call 
#' @export
#' @examples 
#' 
calc_coeff=function(model, dataInput, y_field, t_field, w_field=NULL,t1_field=NULL, diff_field=NULL, fullInteraction=FALSE, k_wilmink=0.1, diff_value=NULL, endCurve=FALSE, alpDuration=115){
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
  if(!is.null(diff_field)){
    if(!(diff_field %in% colnames(dataInput))) stop("diff_field must be a column name of dataInput or set to NULL")
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
    base_model=stats::lm(y_obs~1+I(t_obs/340)+I((t_obs/340)^2)+I(log(340/t_obs))+I((log(340/t_obs))^2), weights = w_field)
  } else if (model=='Wilmink'){
    #Wilmink
    base_model=stats::lm(y_obs~1+I(exp(-k_wilmink*t_obs))+ t_obs,weights =w_field)
  } else if (model=='Wood'){
    # Wood
    #Careful, the result will be log(y)
    base_model=stats::lm(I(log(y_obs))~1+I(log(t_obs))+t_obs,weights = w_field)
  } else {
    stop('Model unknown')
  }
  if(is.null(t1_field)){
    return(base_model) #If t1 is not given, the base model (AS, Wilmink...) is returned
  } else {
    # Add the alp term
    t1_obs=dataInput[,t1_field]
    alp_model=stats::update(base_model, .~.+I(pmax(t_obs-t1_obs,0))) 
    if(endCurve==TRUE){
      # If model also the end of the curve, then the alp-model should be updated to add the increase after alpine grazing + smoother decrease afterwards
      alp_model=stats::update(alp_model, .~.+I(pmax(ceiling((t_obs-(t1_obs+alpDuration))/305),0))+I(pmax(t_obs-(t1_obs+alpDuration),0)))
    }
    if(is.null(diff_field)){ 
      return(alp_model) # If no difference among groups, return the alp model 
    } else {
      if(!is.null(diff_value)){
        diff_field=stats::relevel(as.factor(dataInput[,diff_field]), ref = diff_value[2])
      } else {
        # Convert to factor (will treat it as boolean variable)
        diff_field=as.factor(dataInput[,diff_field])
      }
      if(fullInteraction==FALSE){
        # Add only an interaction between diff_field and alp term
        diff_model=stats::update(alp_model, .~.+diff_field:I(pmax(t_obs-t1_obs,0)))
        if(endCurve==TRUE){
          diff_model=stats::update(diff_model, .~.+diff_field:I(pmax(ceiling((t_obs-(t1_obs+alpDuration))/305),0))+diff_field:I(pmax(t_obs-(t1_obs+alpDuration),0)))
        }
        return(diff_model)
      } else {
        # Add full interaction
        interaction_model=stats::update(alp_model, .~diff_field*.)
        return(interaction_model)
      }
    }
  }
}


#Plot and Model the curve

#' @title Plot, model and test the significance of parameters for lactation curve modelling of (mountain-pastured cows)
#' @description The modelling will be performed with the function \code{calc_coeff}
#' @author Solange Duruz
#' @param dataInput a matrix containing lines observations at different days in milk. 
#' @param y_field char The name of the column in \code{dataInput} containing the y-observation (typically milk yield but could also be protein yield)
#' @param t_field char The name of the column in \code{dataInput}  containing the time of the observation (typically in days in milk)
#' @param cm_field char The name of the column in \code{dataInput}  containing the season of calving (if different calving month are present in the dataset)
#' @param month char/integer vector The list of months to consider as defined in \code{cm_field}
#' @param t1_field char The name of the column in \code{dataInput} that corresponds to the time (same unit as \code{t_field}) at which the time is alped. If groups of cows are taken, this time must be different for each calving season. If NULL, the base model as presented by AS/Wood/Wilmink will be returned
#' @param diff_field char The name of the column in \code{dataInput} containing the groups (if want to test the difference among groups)
#' @param diff_value char/integer vector The possible value that \code{diff_field can have}. The first value will be taken as the reference value
#' @param w_field char The name of the column in \code{dataInput} that corresponds to the weight of the observation (typically the number of cows when working on average of cows)
#' @param prediction boolean Whether the prediction line should be plotted on top of the curve. Necessarily TRUE if want to calculate p-values of 
#' @param model char One of 'AS' (for Ali-Schaeffer), 'Wilmink','Wood' to describe the type of regression
#' @param k_wilmink real If model='Wilmink', the value of the k coefficient. By defautl, 0.1. 
#' @param endCurve boolean Whether the end of the curve (after high alpine grazing is over) should be plotted or not
#' @param alpDuration integer The duration of the high alpine grazing season (same unit as \code{t_field})
#' @param fullInteraction boolean If FALSE only the alp term will vary according to \code{diff_field}. If TRUE all terms are allowed to vary according to \code{g_field}.
#' @param interactionMonth boolean If true, the coefficients of the regression will be estimated separately for each month
#' @param ylabel char The name of the y-axis label in the plot
#' @param xlabel char The name of the x-axis label in the plot
#' @param pal char The name of the color palette used to draw the data points as defined in color brewer. See ?RColorBrewer::brewer.pal for mor details
#' @param predictionCol char The color name for the prediction line
#' @param pch integer vector The pch code for the different \code{diff_value} present in the dataset
#' @param diffLegend ?
#' @return  A list containing the coefficients of the regression ($coeffs), the d-parameter ($alp_coeff), the estimated total milk production ($milk_total), and during alpine grazine only ($milk_alp). If \code{diff_field} is not null, the list will aslo contain  the change in the d-parameter according to the group ($alpdiff_coeff), the p-value of the difference between groups ($pval)
#' @export
#' @examples
#' 
#' 
plot_lc=function(dataInput, y_field, t_field, cm_field,month=c(12,11,10,9,8,3,2,1), t1_field,diff_field=NULL, diff_value=c(1,0),w_field=NULL,prediction=TRUE, model='Wilmink', k_wilmink=0.1,endCurve=FALSE, alpDuration=115,fullInteraction=FALSE,  interactionMonth=FALSE,  ylabel='Milk yield [kg]', xlabel='Days in milk', pal='Blues', predictionCol='gray', pch=c(1,19,3), diffLegend=NULL){
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
  if(!is.null(diff_field)){
    if(!(diff_field %in% colnames(dataInput))) stop("diff_field must be a column name of dataInput or set to NULL")
  }
  if(!is.null(cm_field)){
    if(!(cm_field %in% colnames(dataInput))) stop("cm_field must be a column name of dataInput or set to NULL")
  }
  if(!is.logical(fullInteraction)) stop("fullInteraction supposed to be boolean")
  if(!is.null(k_wilmink)){
    if(!is.double(k_wilmink)) stop("k_wilmink supposed to be double")
  }
  if(!is.null(endCurve)){
    if(!is.logical(endCurve)) stop("endCurve supposed to be boolean")
  }
  if(!is.logical(prediction)) stop("prediction supposed to be boolean")
  if(!is.null(fullInteraction)){
    if(!is.logical(fullInteraction)) stop("fullInteraction supposed to be boolean")
  }
  if(!is.null(interactionMonth)){
    if(!is.logical(interactionMonth)) stop("interactionMonth supposed to be boolean")
  }
  if(!is.null(alpDuration)){
    if(!is.double(alpDuration)) stop("alpDuration supposed to be integer")
  }
  
  #Create color palette
  colors = RColorBrewer::brewer.pal(max(3,length(month)+1), pal)
  if(!is.null(diff_field)){
    #If diff_field (test difference among groups), select only the values present in diff_values (remove NA and other for example)
    dataInput=dataInput[dataInput[,diff_field]%in%diff_value,]
  }
  diff_value=as.character(diff_value) #convert to character: will be considered as a dummy variable
  
  #Prepare the plot
  graphics::plot(NA, xlim=c(0,300), ylim=c(min(dataInput[,y_field], na.rm=TRUE),max(dataInput[,y_field], na.rm=TRUE)), xlab=xlabel, ylab=ylabel)
  
  j=2 # color selection. 1 is too pale
  
  #Weight calculation
  if(!is.null(w_field)){
    #Normalize weights between 0.5 and 1 (arbitrarly chosen values)
    w_plot=0.5+(dataInput[,w_field]-min(dataInput[,w_field], na.rm=TRUE))*0.5/(max(dataInput[,w_field], na.rm=TRUE)-min(dataInput[,w_field], na.rm=TRUE))
  } else {
    #If no weight, weight=1
    w_plot=rep(1,nrow(dataInput))
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
  
  for(i in month){ # Loop on calving month
    if(prediction==TRUE){
      # Calculate the number of days in milk for each calving month where alping starts (middle of May for earliest)
      #if(i<5){
      #  t1=155-i*30
      #}  else {
      #  t1=275-(i-8)*30 
      #}
      t1=dataInput[dataInput[,cm_field]==i,t1_field]
      t1=t1[1]
    }
    if(is.null(diff_field)){
      # select observation of right month
      graphics::points(dataInput[dataInput[,cm_field]==i,y_field]~dataInput[dataInput[,cm_field]==i,t_field], col=colors[j], pch=19, cex=w_plot)
      if(prediction==TRUE){        # calculate model, predict value and draw predicted value
        if(interactionMonth==TRUE){ #To change the parameter values according to calving month (otherwise only t1 will differ from month to month)
          dataInput_sub=dataInput[dataInput[,cm_field]==i,]
        } else {
          dataInput_sub=dataInput
        }
        
        #Fit the model with the ad hoc function
        coeff=calc_coeff(model, dataInput_sub, y_field, t_field, t1_field=t1_field,w_field=w_field, fullInteraction=fullInteraction, diff_value=diff_value, endCurve=endCurve, k_wilmink=k_wilmink, alpDuration=alpDuration)
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
        graphics::lines(t(pred_t['t_obs']), pred_y, col=predictionCol)
        
      }
    } else { #diff_field is not null
      # select observation of right month and separate into two groups according to diff_field
      pch_num=1
      for(k in diff_value){
        graphics::points(dataInput[dataInput[,cm_field]==i & dataInput[,diff_field]==k,y_field]~dataInput[dataInput[,cm_field]==i & dataInput[,diff_field]==k,t_field], col=colors[j], pch=pch[pch_num], cex=w_plot)
        pch_num=pch_num+1
      }
      if(prediction==TRUE){
        #Will draw the prediction, and calculate the p-value of interaction diff_field x alp-term
        if(interactionMonth==TRUE){
          #Select only the month of the loop
          dataInput_sub=dataInput[dataInput[,cm_field]==i,]
        } else {
          dataInput_sub=dataInput
        }
        coeff=calc_coeff(model, dataInput_sub, y_field, t_field, t1_field=t1_field, k_wilmink=k_wilmink,w_field=w_field, g_field=diff_field, fullInteraction=fullInteraction, diff_value=diff_value, endCurve=endCurve, alpDuration=alpDuration)
        
        if(endCurve==TRUE){
          full_model=calc_coeff(model, dataInput_sub[dataInput_sub[,t_field]-dataInput_sub[,t1_field]<alpDuration,], y_field, t_field, t1_field=t1_field, k_wilmink=k_wilmink,w_field=w_field, g_field=diff_field, fullInteraction=fullInteraction, diff_value=diff_value, endCurve=FALSE)
        } else {
          full_model=coeff
        }
        
        #Isolate the alp term and the interaction diff_field x alp-term (might have several name, check which one exists)
        alp_coeff[j-1]=coeff$coefficients['I(pmax(t_obs - t1_obs, 0))']
        alpdiff_coeff[j-1]=coeff$coefficients[paste0('diff_field',diff_value[1],':I(pmax(t_obs - t1_obs, 0))')]
        if(is.na(alpdiff_coeff[j-1])){
          alpdiff_coeff[j-1]=coeff$coefficients[paste0('diff_field',diff_value[2],':I(pmax(t_obs - t1_obs, 0))')]
        }
        if(is.na(alpdiff_coeff[j-1])){
          alpdiff_coeff[j-1]=coeff$coefficients[paste0('I(pmax(t_obs - t1_obs, 0)):diff_field',diff_value[1])]
        }
        if(is.na(alpdiff_coeff[j-1])){
          alpdiff_coeff[j-1]=coeff$coefficients[paste0('I(pmax(t_obs - t1_obs, 0)):diff_field',diff_value[2])]
        }
        #Remove only last term
        null_model=stats::update(full_model, .~.-diff_field:I(pmax(t_obs - t1_obs, 0)))
        
        #fterms = terms(coeff$call[[2]])
        #fac = attr(fterms, "factors")
        #new_fterms <- drop.terms(fterms, dropx = ncol(fac), keep.response = TRUE)
        #null_model=update(coeff,formula(new_fterms))
        
        #Calculate p-value
        pval[j-1]=lmtest::lrtest(full_model, null_model)$'Pr(>Chisq)'[2]
        
        k_i=1
        for(k in diff_value){
          #Predict and draw line 
          pred_t=data.frame(t_obs=c(1:365), t1_obs=rep(t1, 365), g_field=rep(k,365))
          pred_y=stats::predict(coeff, pred_t) 
          graphics::lines(t(pred_t['t_obs']), pred_y, col=predictionCol)
          # Calculate the quantity of milk during alping
          function_predict=function(t) stats::predict(coeff, data.frame(t_obs=t, t1_obs=t1, g_field=k)) 
          milk_alp[k_i,(j-1)]=as.double(integrate(function_predict,t1,t1+alpDuration)$value)
          milk_total[k_i,(j-1)]=as.double(integrate(function_predict,0,305)$value)
          k_i=k_i+1
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
  graphics::legend(x='topright', legend=legendc, cex=0.8, col=colc, pch=pchc, inset=0.02)
  
  #Return value 
  if(!is.null(diff_field)){
    return(list('coeffs'=coeff,'alp_coeff'=alp_coeff,'alpdiff_coeff'=alpdiff_coeff,'pval'=pval, 'milk_alp'=milk_alp, 'milk_total'=milk_total))
  } else {
    return(list('coeffs'=coeff,'alp_coeff'=alp_coeff,'milk_alp'=milk_alp, 'milk_total'=milk_total))
  }

}
