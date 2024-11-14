############################################
## Coefficient extraction function
############################################

coef_ex <- function(mod,i0=1,j0=1){ifelse(nrow(summary(mod)$coefficients)>=i0,summary(mod)$coefficients[i0,j0],NA)}


############################################
## Function to update bounding box of sf object
############################################

update_bbox <- function(sfobj){
  require(sf)
  require(data.table)
  require(dplyr)
  new_bb <- sfobj %>% sf::st_coordinates() %>% data.table::as.data.table() %>% (function(x){x[,c(min(X),min(Y),max(X),max(Y))]})
  names(new_bb) <- c("xmin", "ymin", "xmax", "ymax")
  attr(new_bb, "class") <- "bbox"
  attr(sf::st_geometry(sfobj), "bbox") <- new_bb
  return(sfobj)
}



############################################
## Function to cbind list, deleting duplicate column names
############################################

cbind_dupnames <- function(lst) {
  while(length(lst) > 1) {
    idxlst <- seq(from=1, to=length(lst), by=2)
    lst <- lapply(idxlst, function(i) {
      if(i==length(lst)) { return(lst[[i]]) }
      return(cbind(lst[[i]], lst[[i+1]]) %>% data.table::as.data.table() %>% .[,.SD,.SDcols=unique(names(.))])
    })
  }
  lst[[1]]
}

############################################
## RMSE
############################################

rmse <- function(x,y){sqrt(mean((x - y)^2,na.rm=T))}
nrmse <- function(x,y,basis="range"){
  if(basis=="mean"){
    sqrt(mean((x - y)^2,na.rm=T))/(abs(mean(x,na.rm=T)))
  } else {sqrt(mean((x - y)^2,na.rm=T))/(diff(range(x,na.rm=T)))}
}

############################################
## Smart rounding function
############################################


smart_round <- function(x,rnd,force_rnd=FALSE){
  x_ <- x
  x_[abs(round(x,rnd))<(1*10^(0-rnd))] <- signif(x[abs(round(x,rnd))<(1*10^(0-rnd))],1)
  x_[abs(round(x,rnd))>=(1*10^(0-rnd))] <- round(x[abs(round(x,rnd))>=(1*10^(0-rnd))],rnd)
  x_[abs(round(x_,rnd+1))>=(1*10^(0-(rnd+1)))] <- round(x_[abs(round(x_,rnd+1))>=(1*10^(0-(rnd+1)))],rnd+1)
  x_ <- as.character(x_)
  if(force_rnd==TRUE){
    if(grepl("\\.",x_)){
      if(nchar(sapply(strsplit(x_,split="\\."),"[",2))<rnd){
        x_ <- paste0(sapply(strsplit(x_,split="\\."),"[",1),".", sapply(strsplit(x_,split="\\."),"[",2),paste0(rep("0",rnd-nchar(sapply(strsplit(x_,split="\\."),"[",2)))))
      }
    }
    if(!grepl("\\.",x_)){
      x_ <- paste0(x_,".", paste0(rep("0",rnd),collapse=""))
    }
  }
  return(x_)
}


############################################
## Make empty raster
############################################

make_empty_raster <- function(nrowz=NULL, ncolz=NULL, ProjInput="+proj=merc +units=km", resz=NULL, extentInsert){
  if(is.null(resz)){
    out <- raster::raster(ncols = ncolz, nrows = nrowz, crs = sp::CRS(ProjInput),ext=raster::extent(extentInsert))
  }else{
    out <- raster::raster(res=resz, crs = sp::CRS(ProjInput),ext=raster::extent(extentInsert))
  }
  raster::values(out) <- 1:length(out)
  names(out) <- "ID"  
  return(out)
}

############################################
## Make RGF
############################################

make_gaussian_field <- function(newdataz=NULL,rasterz=NULL,nsimz=1,psillz=1,rangez=100,nuggetz=0,betaz=0,modelz="Ste",nmaxz=round(sqrt(length(rasterz))/2)){
  rdt_model <- gstat::gstat(
    formula=z~1,     # assume constant trend in the data
    locations=~x+y,  # z is linearly dependent on x and y
    dummy=TRUE,      # set to True for unconditional simulation
    beta=betaz,      # set average value over the field
    model=gstat::vgm(
      psill=psillz,
      range=rangez ,
      nugget=nuggetz,
      model=modelz), 
    nmax=nmaxz)
  out <- predict(rdt_model, newdata=newdataz, nsim=nsimz)
  return(out)
}


############################################
# TPRF 
############################################


#######################
## Spatial Trend Estimation
#######################

TPRS_Spatial_Trend_Estimation <- function(input_data, 
                                         family = 'gaussian',
                                         bd = 10,
                                         type = 'link',
                                         mask_insert = NULL,
                                         weights = NULL,
                                         cores = 1,
                                         data_frame = F,
                                         message_out = T){
  ## INFORMATIONAL MESSAGES
  if(message_out){
    message('Part 1 - Performing TPRS Interpolation of Data')
  }
  
  if(any(c('data.table', 'data.frame')%in%class(input_data))){
    data_frame <- T
    
    input_data <- as.data.frame(input_data)
  }
  
  if(data_frame){
    if(ncol(input_data) != 3){
      stop("Error: Please insert a three column matrix where the first two columns are coordinates named 'x' and 'y'. The third column must be the response variable you want to decompose.")
    }
    
    if(!all(c('x', 'y')%in%names(input_data))){
      stop("Error: Please insert a three column matrix where the first two columns are coordinates named 'x' and 'y'. The third column must be the response variable you want to decompose.")
    }
  }
  
  ## Prepare Raster Data for Further Processing
  if(!data_frame){
    # Preserve a raster layer copy of input_data
    input_data_Layer <- input_data 
    # Convert raster to data frame
    input_data_xy <- raster::as.data.frame(input_data, xy = T) 
    # Rename variable for input data
    names(input_data_xy)[3] <- 'Var' 
    # If the raster imputation layer is missing, then use input data
    impute_layer <- input_data
    raster::values(impute_layer) <- 1:raster::ncell(impute_layer)
    if(!is.null(mask_insert)){
      impute_layer <- raster::mask(impute_layer, mask_insert)
    }
    # Convert raster to data frame
    impute_layer_xy <- raster::as.data.frame(impute_layer, xy = T) 
    names(impute_layer_xy)[3] <- 'Var'
    # Create object with missing ID tags
    RemoveTPRS <- which(is.na(input_data_xy$Var)) 
    # Create object with missing ID tags
    RemoveImpute <- which(is.na(impute_layer_xy$Var)) 
  } else {
    # Preserve a raster layer copy of input_data
    input_data_xy <- input_data 
    # Rename variable for input data
    names(input_data_xy)[3] <- 'Var' 
    impute_layer_xy <- input_data_xy
    # Create object with missing ID tags
    RemoveTPRS <- which(is.na(input_data_xy$Var)) 
    # Create object with missing ID tags
    RemoveImpute <- which(is.na(impute_layer_xy$Var)) 
  }

  ## Thin-Plate Spline Regression Using MGCV Package
  suppressMessages({
    suppressWarnings({
      TPRS_GAM_Model <- mgcv::gam(Var ~ te(x, y, bs = c('tp', 'tp'), k = c(bd, bd)), 
                                  family = family, 
                                  data = if(length(RemoveTPRS) > 0){input_data_xy[-RemoveTPRS,]}else{input_data_xy},
                                  weights = weights,
                                  na.action = na.omit,
                                  control = list(nthreads = cores)) 
    })
  })
  # GLM Family Functions
  if(is.character(family)){
    if(family%in%'poisson'){
      linkFunction <- poisson()
    }
    if(family%in%'binomial'){
      linkFunction <- binomial()
    }
    if(family%in%'Gamma'){
      linkFunction <- Gamma()
    }
    if(family%in%'inverse.gaussian'){
      linkFunction <- inverse.gaussian()
    }
    if(family%in%'quasi'){
      linkFunction <- quasi()
    }
    if(family%in%'quasibinomial'){
      linkFunction <- quasibinomial()
    }
    if(family%in%'quasipoisson'){
      linkFunction <- quasipoisson()
    }
    if(family%in%'gaussian'){
      linkFunction <- gaussian()
    }
  } else {
    if(!is.character(family)){
      linkFunction <- family
    }
  }
  
  ## Predict Values for Imputed Data Layer (Can Be Equivalent to Input Layer)
  TPRS_Predict <- mgcv::predict.gam(TPRS_GAM_Model, 
                                    newdata = if(length(RemoveImpute) > 0){impute_layer_xy[-RemoveImpute,]}else{impute_layer_xy}, 
                                    type = type, 
                                    se.fit = T, 
                                    n.threads = cores)
  if(type%in%'link'){
    # Generate Data Frame for Response Predictions
    impute_layer_xy$TPRS_Predict_Link <- NA
    if(length(RemoveImpute) > 0){
      impute_layer_xy$TPRS_Predict_Link[-RemoveImpute] <- as.vector(TPRS_Predict$fit)
    } else {
      impute_layer_xy$TPRS_Predict_Link <- as.vector(TPRS_Predict$fit)
    } 
    # Generate Converted Prediction
    impute_layer_xy$TPRS_Predict <- linkFunction$linkinv(impute_layer_xy$TPRS_Predict_Link) 
    # Generate Raster Layer for Standard Error Predictions (Response)
    impute_layer_xy$TPRS_Predict_SE <- NA
    if(length(RemoveImpute) > 0){
      impute_layer_xy$TPRS_Predict_SE[-RemoveImpute] <- as.vector(TPRS_Predict$se.fit)
    } else {
      impute_layer_xy$TPRS_Predict_SE <- as.vector(TPRS_Predict$se.fit)
    } 
    
    ## Predict Values for Input Data Layer (Used to Generate Residual Layer)
    TPRS_Predict <- mgcv::predict.bam(TPRS_GAM_Model, newdata = if(length(RemoveTPRS) > 0){input_data_xy[-RemoveTPRS,]}else{input_data_xy}, type = type, se.fit = T, n.threads = cores)
    # Generate Data Frame for Response Predictions
    input_data_xy$TPRS_Predict <- NA
    if(length(RemoveTPRS) > 0){
      input_data_xy$TPRS_Predict[-RemoveTPRS] <- as.vector(TPRS_Predict$fit)
    } else {
      input_data_xy$TPRS_Predict <- as.vector(TPRS_Predict$fit)
    } 
    if(data_frame){
      impute_layer_xy$TPRS_Predict_Link <- impute_layer_xy$TPRS_Predict_Link
    }
    # Generate Data Frame for TPRS-Residuals
    input_data_xy$TPRS_Residuals <- NA
    if(length(RemoveTPRS) > 0){
      input_data_xy$TPRS_Residuals[-RemoveTPRS] <- resid(TPRS_GAM_Model)
    } else {
      input_data_xy$TPRS_Residuals <- resid( TPRS_GAM_Model)
    }
    if(!data_frame){
      Residual_Layer <- input_data 
      raster::values(Residual_Layer) <- input_data_xy$TPRS_Residuals
      names(Residual_Layer) <- 'TPRS_Residuals'
    }

    ## Generate Raster Layers for Prediction, Std. Errors, and Residuals
    if(!data_frame){
      input_data_layer <- input_data
      # Thin Plate Spline Predicted Values
      TPRS_Predict_Layer <- impute_layer
      raster::values(TPRS_Predict_Layer) <- as.vector(impute_layer_xy$TPRS_Predict_Link)
      # Thin Plate Spline Standard Errors
      TPRS_SE_Layer <- impute_layer
      raster::values(TPRS_SE_Layer) <- as.vector(impute_layer_xy$TPRS_Predict_SE)
      # Generate Residual Layer
      TPRS_Resid_Layer <- input_data
      raster::values(TPRS_Resid_Layer) <- input_data_xy$TPRS_Resid
      names(TPRS_Resid_Layer) <- 'Resid'
      # Generate Return Raster Brick 
      TPRS_Brick <- raster::brick(TPRS_Predict_Layer, TPRS_SE_Layer)
      names(TPRS_Brick) <- c('TPRS_Predict_Link', 'TPRS_SE')
      TPRS_Brick$TPRS_Predict <- TPRS_Brick$TPRS_Predict_Link
      raster::values(TPRS_Brick$TPRS_Predict) <- as.vector(impute_layer_xy$TPRS_Predict)
      TPRS_Brick$TPRS_Residuals <- Residual_Layer
    } else {
      Residual_Layer <- input_data_xy
      Residual_Layer <- Residual_Layer[,which(names(Residual_Layer)%in%c('x', 'y', 'TPRS_Residuals'))]
    }
    
    ## Return Output
    ReturnObject <- list('Model' = TPRS_GAM_Model, 
                         'TPRS_Estimates' = if(!data_frame){TPRS_Brick}else{impute_layer_xy}, 
                         'Residual_Layer' = Residual_Layer,
                         'linkFunction' = linkFunction)
  } else {
    ReturnObject <- list('Model' = TPRS_GAM_Model, 
                         'Prediction' = TPRS_Predict, 
                         'linkFunction' = linkFunction)
  }

  # RETURN
  return(ReturnObject)
}



#######################
## Spatial Trend Estimation (Metropolis Hastings)
#######################

TPRS_Spatial_Trend_Estimation_mh <- function(input_data, 
                                         familyz = 'gaussian',
                                         bd = 10,
                                         type = 'link',
                                         mask_insert = NULL,
                                         weights = NULL,
                                         cores = 1,
                                         data_frame = F,
                                         message_out = T,
                                         nsz=1000,
                                         burnz=100,
                                         t.dfz=40,
                                         rw.scalez=.25,
                                         thinz=1){
  ## INFORMATIONAL MESSAGES
  if(message_out){
    message('Part 1 - Performing TPRS Interpolation of Data')
  }
  if(any(c('data.table', 'data.frame')%in%class(input_data))){
    data_frame <- T
    
    input_data <- as.data.frame(input_data)
  }
  if(data_frame){
    if(ncol(input_data) != 3){
      stop("Error: Please insert a three column matrix where the first two columns are coordinates named 'x' and 'y'. The third column must be the response variable you want to decompose.")
    } 
    if(!all(c('x', 'y')%in%names(input_data))){
      stop("Error: Please insert a three column matrix where the first two columns are coordinates named 'x' and 'y'. The third column must be the response variable you want to decompose.")
    }
  }
  
  ## Prepare Raster Data for Further Processing
  if(!data_frame){
    # Preserve a raster layer copy of input_data
    input_data_Layer <- input_data 
    # Convert raster to data frame
    input_data_xy <- raster::as.data.frame(input_data, xy = T) 
    # Rename variable for input data
    names(input_data_xy)[3] <- 'Var' 
    # If the raster imputation layer is missing, then use input data
    impute_layer <- input_data
    raster::values(impute_layer) <- 1:raster::ncell(impute_layer)
    if(!is.null(mask_insert)){
      impute_layer <- raster::mask(impute_layer, mask_insert)
    }
    # Convert raster to data frame
    impute_layer_xy <- raster::as.data.frame(impute_layer, xy = T) 
    names(impute_layer_xy)[3] <- 'Var'
    # Create object with missing ID tags
    RemoveTPRS <- which(is.na(input_data_xy$Var)) 
    # Create object with missing ID tags
    RemoveImpute <- which(is.na(impute_layer_xy$Var)) 
  } else {
    # Preserve a raster layer copy of input_data
    input_data_xy <- input_data 
    # Rename variable for input data
    names(input_data_xy)[3] <- 'Var' 
    impute_layer_xy <- input_data_xy
    # Create object with missing ID tags
    RemoveTPRS <- which(is.na(input_data_xy$Var)) 
    RemoveImpute <- which(is.na(impute_layer_xy$Var)) 
  }

  ## Thin-Plate Spline Regression Using MGCV Package
  suppressMessages({
    suppressWarnings({
      TPRS_GAM_Model <- mgcv::gam(Var ~ te(x, y, bs = c('tp', 'tp'), k = c(bd, bd)), 
                                  family = familyz, 
                                  data = if(length(RemoveTPRS) > 0){input_data_xy[-RemoveTPRS,]}else{input_data_xy},
                                  weights = weights,
                                  na.action = na.omit,
                                  control = list(nthreads = cores)) 
    })
  })

  # GLM Family Functions
  if(is.character(familyz)){
    if(familyz%in%'poisson'){
      linkFunction <- poisson()
    }
    if(familyz%in%'binomial'){
      linkFunction <- binomial()
    }
    if(familyz%in%'Gamma'){
      linkFunction <- Gamma()
    }
    if(familyz%in%'inverse.gaussian'){
      linkFunction <- inverse.gaussian()
    }
    if(familyz%in%'quasi'){
      linkFunction <- quasi()
    }
    if(familyz%in%'quasibinomial'){
      linkFunction <- quasibinomial()
    }
    if(familyz%in%'quasipoisson'){
      linkFunction <- quasipoisson()
    }
    if(familyz%in%'gaussian'){
      linkFunction <- gaussian()
    }
  } else {
    if(!is.character(familyz)){
      linkFunction <- familyz
    }
  }

  ## Sample from posterior
  TPRS_GAM_samp <- mgcv::gam.mh(TPRS_GAM_Model,ns=nsz,burn=burnz,t.df=t.dfz,rw.scale=rw.scalez,thin=thinz)
  # lpmatrix for input & output layers
  TPRS_lpm_out <- mgcv::predict.gam(TPRS_GAM_Model, newdata = if(length(RemoveImpute) > 0){impute_layer_xy[-RemoveImpute,]}else{impute_layer_xy}, type = "lpmatrix", n.threads = cores)
  TPRS_lpm_in <- mgcv::predict.bam(TPRS_GAM_Model, newdata = if(length(RemoveTPRS) > 0){input_data_xy[-RemoveTPRS,]}else{input_data_xy}, type = "lpmatrix", n.threads = cores)
  # Matrix of predictions
  suppressMessages({
    samp_mat_out <- lapply(1:nrow(TPRS_GAM_samp[[1]]),function(i0){TPRS_lpm_out %*% TPRS_GAM_samp[[1]][i0,]}) %>% dplyr::bind_cols() %>% data.table::as.data.table() %>% data.table::setnames(paste0("pred_",1:nrow(TPRS_GAM_samp[[1]])))
    samp_mat_in <- lapply(1:nrow(TPRS_GAM_samp[[1]]),function(i0){TPRS_lpm_in %*% TPRS_GAM_samp[[1]][i0,]}) %>% dplyr::bind_cols() %>% data.table::as.data.table() %>% data.table::setnames(paste0("pred_",1:nrow(TPRS_GAM_samp[[1]])))
  })
  TPRS_Predict_mean_out <- samp_mat_out %>% apply(1,median)
  TPRS_Predict_sd_out <- samp_mat_out %>% apply(1,sd)
  TPRS_Predict_mean_in <- samp_mat_in %>% apply(1,median)
  TPRS_Predict_sd_in <- samp_mat_in %>% apply(1,sd)
  # Regular prediction
  TPRS_Predict <- mgcv::predict.gam(TPRS_GAM_Model, 
                                  newdata = if(length(RemoveImpute) > 0){impute_layer_xy[-RemoveImpute,]}else{impute_layer_xy}, 
                                  type = type, 
                                  se.fit = T, 
                                  n.threads = cores)
  # Loop over samples
  samp_mat <- lapply(1:ncol(samp_mat_out),function(i0){
    if(type%in%'link'){
      # Linear predictor
      TPRS_Predict_i <- samp_mat_out[,get(paste0("pred_",i0))]
      # Generate Data Frame for Response Predictions
      impute_layer_xy$TPRS_Predict_Link <- NA
      if(length(RemoveImpute) > 0){
        impute_layer_xy$TPRS_Predict_Link[-RemoveImpute] <- as.vector(TPRS_Predict_i)
      } else {
        impute_layer_xy$TPRS_Predict_Link <- as.vector(TPRS_Predict_i)
      } 
      # Generate Converted Prediction
      # impute_layer_xy$TPRS_Predict <- linkFunction$linkinv(impute_layer_xy$TPRS_Predict_Link) 
      # Generate Raster Layer for Standard Error Predictions (Response)
      impute_layer_xy$TPRS_Predict_SE <- NA
      if(length(RemoveImpute) > 0){
        impute_layer_xy$TPRS_Predict_SE[-RemoveImpute] <- as.vector(TPRS_Predict_sd_out)
      } else {
        impute_layer_xy$TPRS_Predict_SE <- as.vector(TPRS_Predict_sd_out)
      } 

      ## Predict Values for Input Data Layer (Used to Generate Residual Layer)
      # Linear predictor
      TPRS_Predict_i <- samp_mat_in[,get(paste0("pred_",i0))]
      # Generate Data Frame for Response Predictions
      input_data_xy$TPRS_Predict <- NA
      if(length(RemoveTPRS) > 0){
        input_data_xy$TPRS_Predict[-RemoveTPRS] <- as.vector(TPRS_Predict_i)
      } else {
        input_data_xy$TPRS_Predict <- as.vector(TPRS_Predict_i)
      } 
      if(data_frame){
        impute_layer_xy$TPRS_Predict_Link <- impute_layer_xy$TPRS_Predict_Link
      }
      # Generate Data Frame for TPRS-Residuals
      input_data_xy$TPRS_Residuals <- NA
      if(length(RemoveTPRS) > 0){
        input_data_xy$TPRS_Residuals[-RemoveTPRS] <- input_data_xy$Var[-RemoveTPRS]-input_data_xy$TPRS_Predict[-RemoveTPRS]
      } else {
        input_data_xy$TPRS_Residuals <- input_data_xy$Var-input_data_xy$TPRS_Predict
      }
      if(!data_frame){
        Residual_Layer <- input_data 
        raster::values(Residual_Layer) <- input_data_xy$TPRS_Residuals
        names(Residual_Layer) <- 'TPRS_Residuals'
      }

      ## Generate Raster Layers for Prediction, Std. Errors, and Residuals
      if(!data_frame){
        input_data_layer <- input_data
        # Thin Plate Spline Predicted Values
        TPRS_Predict_Layer <- impute_layer
        raster::values(TPRS_Predict_Layer) <- as.vector(impute_layer_xy$TPRS_Predict_Link)
        # Thin Plate Spline Standard Errors
        TPRS_SE_Layer <- impute_layer
        raster::values(TPRS_SE_Layer) <- as.vector(impute_layer_xy$TPRS_Predict_SE)
        # Generate Residual Layer
        TPRS_Resid_Layer <- input_data
        raster::values(TPRS_Resid_Layer) <- input_data_xy$TPRS_Resid
        names(TPRS_Resid_Layer) <- 'Resid'
        # Generate Return Raster Brick 
        TPRS_Brick <- raster::brick(TPRS_Predict_Layer, TPRS_SE_Layer)
        names(TPRS_Brick) <- c('TPRS_Predict_Link', 'TPRS_SE')
        TPRS_Brick$TPRS_Predict <- TPRS_Brick$TPRS_Predict_Link
        raster::values(TPRS_Brick$TPRS_Predict) <- as.vector(impute_layer_xy$TPRS_Predict)
        TPRS_Brick$TPRS_Residuals <- Residual_Layer
    } else {
        Residual_Layer <- input_data_xy
        Residual_Layer <- Residual_Layer[,which(names(Residual_Layer)%in%c('x', 'y', 'TPRS_Residuals'))]
    }  
    ## Return Output
    ReturnObject_i <- list('Model' = TPRS_GAM_Model, 
                         'TPRS_Estimates' = if(!data_frame){TPRS_Brick}else{impute_layer_xy}, 
                         'Residual_Layer' = Residual_Layer,
                         'linkFunction' = linkFunction)
    } else {
      ReturnObject_i <- list('Model' = TPRS_GAM_Model, 
                           'Prediction' = TPRS_Predict, 
                           'linkFunction' = linkFunction)
    }
    ReturnObject_i
  })
  # RETURN
  return(samp_mat)
}




#######################
## Geographic Field Regression
#######################

geographic_field_regression <- function(input_data,
                                        ntree = 500,
                                        mtry = 2,
                                        nodesize = 5,
                                        replace = T,
                                        verbose = T,
                                        cores = 1,
                                        parallelize = F,
                                        skip_variogram = T,
                                        ranger = F,
                                        quantreg = F,
                                        quantiles = seq(0,1,0.1),
                                        quantile_prediction = 0.5,
                                        save_forest = T,
                                        plot.return = F,
                                        data_frame = F,
                                        insert.seed = 1234,
                                        message_out = T){
  ## INFORMATIONAL MESSAGES
  if(!ranger){
    set.seed(insert.seed)
  }
  if(quantreg && !ranger){
    ranger <- T
  }
  if(message_out){
    message('Part 2 - Random Forest Estimation of TPRS Residuals')
  }
  if(any(c('data.table', 'data.frame')%in%class(input_data))){
    data_frame <- T
    
    input_data <- as.data.frame(input_data)
  }
  if(!data_frame){
    if(raster::nlayers(input_data) > 1){
      stop('Error: Only a single raster layer is accepted. Please insert a single raster layer')
    }
  }
  if(data_frame){
    if(ncol(input_data) != 3){
      stop("Error: Please insert a three column matrix where the first two columns are coordinates named 'x' and 'y'. The third column must be the response variable you want to decompose.")
    }
    if(!all(c('x', 'y')%in%names(input_data))){
      stop("Error: Please insert a three column matrix where the first two columns are coordinates named 'x' and 'y'. The third column must be the response variable you want to decompose.")
    }
  }

  ## Section 2 - 
  if(!data_frame){
    RF_Input <- raster::as.data.frame(input_data, xy = T)
  } else {
    RF_Input <- input_data
  }
  names(RF_Input)[3] <- 'Var'
  Missing_Values <- which(is.na(RF_Input$Var))
  if(ranger){
    RF_Output <- ranger::ranger(Var ~ x + y, 
                                data =  if(length(Missing_Values) > 0){RF_Input[-Missing_Values,]}else{RF_Input},
                                verbose = verbose, 
                                mtry = mtry,
                                num.trees = ntree,
                                write.forest = save_forest,
                                quantreg = quantreg,
                                seed = insert.seed,
                                replace = replace)
  } else {
    RF_Output <- randomForest::randomForest(x = if(length(Missing_Values) > 0){RF_Input[-Missing_Values,-3]}else{RF_Input[,-3]}, 
                                            y = if(length(Missing_Values) > 0){RF_Input[-Missing_Values,3]}else{RF_Input[,3]},
                                            do.trace = verbose, 
                                            mtry = mtry,
                                            ntree = ntree,
                                            replace = replace,
                                            nodesize = nodesize)
  }

  ## Section 3 - 
  if(quantreg && ranger){
    Predicted_Values_Output <-  predict(RF_Output, 
                                        data = RF_Input[,1:2], 
                                        type = 'quantiles',
                                        quantiles = unique(c(quantiles, quantile_prediction)), 
                                        num.threads = if(parallelize){cores}else{1},
                                        seed = insert.seed)
    Quantile_Predictions <- Predicted_Values_Output$predictions
    AggregateLoc <- which(quantiles%in%quantile_prediction)
    Predicted_Values_Frame <- data.table::data.table(RF_Input[,1:2], 
                                                     'Aggregate' = Quantile_Predictions[,AggregateLoc], 
                                                     Predicted_Values_Output$predictions)
    names(Predicted_Values_Frame) <- gsub('= ', '_', names(Predicted_Values_Frame))
  } 
  if(!quantreg && ranger){
    Predicted_Values_Output <-  predict(RF_Output, 
                                        data = RF_Input[,1:2], 
                                        predict.all = T,
                                        num.threads = if(parallelize){cores}else{1},
                                        seed = insert.seed)
    Predicted_Values_Frame <- data.table::data.table(RF_Input[,1:2], 
                                                     'Aggregate' = RF_Output$predictions, 
                                                     'Individual' = Predicted_Values_Output$predictions)
    names(Predicted_Values_Frame)[-c(1:3)] <- paste0('Individual_', 1:length(names(Predicted_Values_Frame)[-c(1:3)]))
  } 
  if(!quantreg && !ranger){
    Predicted_Values_Output <-  predict(RF_Output, 
                                        newdata = RF_Input[,1:2], 
                                        predict.all = T)
    Predicted_Values_Frame <- data.table::data.table(RF_Input[,1:2], 
                                                     'Aggregate' = Predicted_Values_Output$aggregate, 
                                                     'Individual' = Predicted_Values_Output$individual)
    names(Predicted_Values_Frame) <- gsub('.V', '_', names(Predicted_Values_Frame))
  }
  
  ## Section 4 - 
  if(!data_frame){
    RF_Aggregate_Tree_Estimates <- input_data
    raster::values(RF_Aggregate_Tree_Estimates) <- Predicted_Values_Frame$Aggregate
    names(RF_Aggregate_Tree_Estimates) <- 'Var'
    if(quantreg){
      Quantile_Regression_Brick <- lapply(1:length(unique(c(quantiles, quantile_prediction))), function(i) input_data)
      Quantile_Regression_Brick <- raster::brick(Quantile_Regression_Brick)
      raster::values(Quantile_Regression_Brick) <- Predicted_Values_Output$predictions
      names(Quantile_Regression_Brick) <- paste0('Quantile_', unique(c(quantiles, quantile_prediction)))
    }
    if(!skip_variogram){
      Variogram_SP <- as(RF_Aggregate_Tree_Estimates, 'SpatialPointsDataFrame')
      RF_Aggregate_Variogram <- automap::autofitVariogram(Var ~ 1, Variogram_SP)
    }
  names(RF_Aggregate_Tree_Estimates) <- 'Residuals'
    if(quantreg){
      Quantile_Return_Var <- raster::calc(Quantile_Regression_Brick, function(i) var(i, na.rm = T))
      RF_Aggregate_Tree_Estimates <- raster::brick(RF_Aggregate_Tree_Estimates, Quantile_Return_Var)
      names(RF_Aggregate_Tree_Estimates) <- c('Residuals', 'Variance')
    }
  } else {
    RF_Aggregate_Tree_Estimates <- Predicted_Values_Frame[,1:3]
    names(RF_Aggregate_Tree_Estimates)[3] <- 'Residuals'
    if(quantreg){
      Quantile_Regression_Brick <- Predicted_Values_Output$predictions
      Quantile_Return_Var <- apply(Quantile_Regression_Brick, 1, function(i) var(i, na.rm = T))
      RF_Aggregate_Tree_Estimates <- data.frame(RF_Aggregate_Tree_Estimates, Quantile_Return_Var)
      names(RF_Aggregate_Tree_Estimates) <- c('x', 'y', 'Residuals', 'Variance')
    }
  }

  ## Section 7 - 
  ReturnOutput <- list('Aggregate_Variogram' = if(!data_frame && !skip_variogram){RF_Aggregate_Variogram}else{NULL}, 
                       'Estimate_Layer' = RF_Aggregate_Tree_Estimates,
                       'Quantile_Brick' = if(quantreg){Quantile_Regression_Brick}else{NULL}, 
                       'Model' = RF_Output, 
                       'Predicted_Values_Frame' = Predicted_Values_Frame)
  # RETURN
  return(ReturnOutput)
} 











#######################
## TPRS predict
#######################

tprs_predict <- function(input_poly,output_poly,funz=function(x){sum(x,na.rm=TRUE)},resz=25000,varz=NULL,pycno=FALSE,mh=FALSE,ncorez_mh=1,include_resid=FALSE,nsz=1000,burnz=100,t.dfz=40,rw.scalez=.25,thinz=1,print_progress=FALSE){
  # Get CRS
  in_crs <- sf::st_crs(input_poly)
  # Rasterize polygons
  if(grepl("longlat|degree",as.character(raster::crs(input_poly)))){
    input_1 <- SUNGEO::sf2raster(
      polyz_from = SUNGEO::utm_select(input_poly),
      input_variable = varz,
      grid_res = c(resz,resz),
      message_out=FALSE)    
    new_crs <- sf::st_crs(input_1)
  }else{
    input_1 <- SUNGEO::sf2raster(
      polyz_from = input_poly,
      input_variable = varz,
      grid_res = c(resz,resz),
      message_out=FALSE)        
    new_crs <- sf::st_crs(input_poly)
  }
  # Create mask polygons
  poly_1 <- sf::st_transform(output_poly,new_crs)
  if(mh==FALSE){
    # TPRS
    out_1 <- TPRS_Spatial_Trend_Estimation(input_data=input_1, 
                                           family = 'gaussian',
                                           bd = 10,
                                           type = 'link',
                                           mask_insert = poly_1,
                                           weights = NULL,
                                           cores = 1,
                                           data_frame = F,
                                           message_out = F
                                           )
    # RF
    out_2 <- geographic_field_regression(input_data = out_1$Residual_Layer, 
                                          ntree = 500, 
                                          cores = 1, 
                                          plot.return = F, 
                                          skip_variogram = F,
                                          message_out = F,
                                          verbose = F)
    # Noise
    out_3 <- out_1$Residual_Layer - out_2$Estimate_Layer
    # Quantities of interest
    if(include_resid==TRUE){
      out_mean <- out_1$TPRS_Estimates$TPRS_Predict_Link + out_2$Estimate_Layer + out_3
    }else{
      out_mean <- out_1$TPRS_Estimates$TPRS_Predict_Link + out_3
    }
    out_l <- out_mean - 1.96*out_1$TPRS_Estimates$TPRS_SE
    out_u <- out_mean + 1.96*out_1$TPRS_Estimates$TPRS_SE
    # Extract average raster values
    output_poly$poly_mean <- raster::extract(x=out_mean,y=output_poly %>% sf::st_transform(new_crs),fun=funz,na.rm=TRUE)
    output_poly$poly_l <- raster::extract(x=out_l,y=output_poly %>% sf::st_transform(new_crs),fun=funz,na.rm=TRUE)
    output_poly$poly_u <- raster::extract(x=out_u,y=output_poly %>% sf::st_transform(new_crs),fun=funz,na.rm=TRUE)
    output_poly_dt <- output_poly %>% data.table::as.data.table() %>% data.table::setnames(c("poly_mean","poly_l","poly_u"),paste0(varz,c("_mean","_l","_u")))
    # Pycno
    if(pycno==TRUE){
      sum_from <- data.table::as.data.table(input_poly)[,sum(get(varz), na.rm = TRUE)]
      pycno_varz_to <- grep(paste0("^", varz),names(output_poly_dt), value = TRUE)
      for(p00 in 1:length(pycno_varz_to)) {
        output_poly_dt[, `:=`(eval(pycno_varz_to[p00]), get(pycno_varz_to[p00]) * sum_from/sum(get(pycno_varz_to[p00]), na.rm = TRUE))]
      }
    }
    # Return
    output_poly_sf <- output_poly_dt %>% sf::st_as_sf()
  }else{
    # TPRS
    out_1 <- TPRS_Spatial_Trend_Estimation_mh(input_data=input_1, 
                                         family = 'gaussian',
                                         bd = 10,
                                         type = 'link',
                                         mask_insert = poly_1,
                                         weights = NULL,
                                         cores = 1,
                                         data_frame = F,
                                         message_out = F,
                                         nsz=nsz,
                                         burnz=burnz,
                                         t.dfz=t.dfz,
                                         rw.scalez=rw.scalez,
                                         thinz=thinz
                                         )
    # RF
    out_mat <- parallel::mclapply(1:length(out_1),function(i0){
      if(print_progress==TRUE){cat(paste0(sprintf(paste0("%0",nchar(length(out_1)),"d"), i0),"/",length(out_1),"\r"))}
      out_2 <- geographic_field_regression(input_data = out_1[[i0]]$Residual_Layer, 
                                          ntree = 500, 
                                          cores = 1, 
                                          plot.return = F, 
                                          skip_variogram = F,
                                          message_out = F,
                                          verbose = F)
      # Noise
      out_3 <- out_1[[i0]]$Residual_Layer - out_2$Estimate_Layer
      # Quantities of interest
      if(include_resid==TRUE){
        out_ <- out_1[[i0]]$TPRS_Estimates$TPRS_Predict_Link + out_1[[i0]]$Residual_Layer + out_3
      }else{
        out_ <- out_1[[i0]]$TPRS_Estimates$TPRS_Predict_Link + out_3
      }
      out_
    },mc.cores=ncorez_mh)
    # Assign raster values
    out_mean <- out_l <- out_u <- out_mat %>% .[[1]]
    raster::values(out_mean) <- out_mat %>% sapply(function(x){raster::values(x) }) %>% apply(1,mean,na.rm=T)
    raster::values(out_l) <- out_mat %>% sapply(function(x){raster::values(x) }) %>% apply(1,mean,na.rm=T) - 1.96*(out_mat %>% sapply(function(x){raster::values(x) }) %>% apply(1,sd,na.rm=T))
    raster::values(out_u) <- out_mat %>% sapply(function(x){raster::values(x) }) %>% apply(1,mean,na.rm=T) + 1.96*(out_mat %>% sapply(function(x){raster::values(x) }) %>% apply(1,sd,na.rm=T))
    # Extract average raster values
    output_poly$poly_mean <- raster::extract(x=out_mean,y=output_poly %>% sf::st_transform(new_crs),fun=funz,na.rm=TRUE)
    output_poly$poly_l <- raster::extract(x=out_l,y=output_poly %>% sf::st_transform(new_crs),fun=funz,na.rm=TRUE)
    output_poly$poly_u <- raster::extract(x=out_u,y=output_poly %>% sf::st_transform(new_crs),fun=funz,na.rm=TRUE)
    output_poly_dt <- output_poly %>% data.table::as.data.table() %>% data.table::setnames(c("poly_mean","poly_l","poly_u"),paste0(varz,c("_mean","_l","_u")))
    # Pycno
    if(pycno==TRUE){
      sum_from <- data.table::as.data.table(input_poly)[,sum(get(varz), na.rm = TRUE)]
      pycno_varz_to <- grep(paste0("^", varz),names(output_poly_dt), value = TRUE)
      for(p00 in 1:length(pycno_varz_to)) {
        output_poly_dt[, `:=`(eval(pycno_varz_to[p00]), get(pycno_varz_to[p00]) * sum_from/sum(get(pycno_varz_to[p00]), na.rm = TRUE))]
      }
    }
    # Return
    output_poly_sf <- output_poly_dt %>% sf::st_as_sf()
  }
  return(output_poly_sf)
}



#######################
## Rasterization 
#######################

poly2poly_ras <- function(input_poly,output_poly,funz=mean,resz=25000,varz=NULL,na.rmz=TRUE,pycno=FALSE){
  # Get CRS
  in_crs <- sf::st_crs(input_poly)
  # Loop over variables
  for(v in 1:length(varz)){
    # Rasterize polygons
    if(grepl("longlat|degree",as.character(raster::crs(input_poly)))){
      input_1 <- SUNGEO::sf2raster(
        polyz_from = SUNGEO::utm_select(input_poly),
        input_variable = varz[v],
        grid_res = c(resz,resz),
        message_out=FALSE)    
      new_crs <- sf::st_crs(input_1)
    }else{
      input_1 <- SUNGEO::sf2raster(
        polyz_from = input_poly,
        input_variable = varz[v],
        grid_res = c(resz,resz),
        message_out=FALSE)        
      new_crs <- sf::st_crs(input_poly)
    }
    # Extract values
    output_poly$poly_out <- raster::extract(x=input_1,y=sf::st_transform(output_poly ,new_crs),fun=funz,na.rm=na.rmz)
    output_poly <- data.table::setnames(output_poly,"poly_out",varz[v])
  }
  if(pycno==TRUE){
    sum_from <- as.numeric(data.table::as.data.table(input_poly)[,lapply(.SD,sum, na.rm = TRUE),.SDcols=varz])
    pycno_varz_to <- grep(paste0("^", varz,"\\b",collapse="|"),names(output_poly), value = TRUE)
    output_poly_dt <- data.table::as.data.table(output_poly)
    p00 <- 1
    for(p00 in 1:length(pycno_varz_to)) {
      output_poly_dt[, `:=`(eval(pycno_varz_to[p00]), get(pycno_varz_to[p00]) * sum_from[p00]/sum(get(pycno_varz_to[p00]), na.rm = TRUE))]
    }
    output_poly <- sf::st_as_sf(output_poly_dt)
  }
  return(output_poly)
}




#######################
# TPRS AW
#######################

TPRS_AW_2 <- function(poly_to, 
                    poly_from, 
                    poly_to_id, 
                    poly_from_id, 
                    varz, 
                    k, 
                    extensive = F,
                    PopWeights = NULL,
                    family = 'gaussian', 
                    critical = 1.96, 
                    mc.sim = 1000,
                    boot.num = 100,
                    threads = 1, 
                    n.sim = 1000,
                    seed = 1234,
                    return_list = FALSE,
                    verbose = FALSE){
  set.seed(seed)
  timeStart <- Sys.time()
  if(extensive){
    family <- 'quasibinomial'
  }
  # GLM Family Functions
  if(is.character(family)){
    if(family%in%'poisson'){
      linkFunction <- stats::poisson()
    }
    if(family%in%'binomial'){
      linkFunction <- stats::binomial()
    }
    if(family%in%'Gamma'){
      linkFunction <- stats::Gamma()
    }
    if(family%in%'inverse.gaussian'){
      linkFunction <- stats::inverse.gaussian()
    }
    if(family%in%'quasi'){
      linkFunction <- stats::quasi()
    }
    if(family%in%'quasibinomial'){
      linkFunction <- stats::quasibinomial()
    }
    if(family%in%'quasipoisson'){
      linkFunction <- stats::quasipoisson()
    }
    if(family%in%'gaussian'){
      linkFunction <- stats::gaussian()
    }
  } else {
    if(!is.character(family)){
      linkFunction <- family
    }
  }
  
  fc <- function(d, i){
    d2 <- d[i,]
    Observed <- d2$Observed
    Fitted <- d2$Fit
    Residuals <- d2$Resid
    SE <- d2$SE
    Predicted_Values <- Fitted + Residuals
    Upper_Predicted <- Fitted + critical*SE
    Lower_Predicted <- Fitted - critical*SE
    Upper_Resid <- (Fitted + Residuals) + critical*SE
    Lower_Resid <- (Fitted + Residuals) - critical*SE
    CombinedValues <- c(Fitted, Upper_Predicted, Predicted_Values, Lower_Predicted, Upper_Resid, Lower_Resid)
    meanVal <- mean(Predicted_Values, na.rm = T)
    sdVal <- sd(CombinedValues, na.rm = T)
    meanResid <- mean(Residuals, na.rm = T)
    meanObsv <- mean(Observed, na.rm = T)
    sdObsv <- sd(Observed, na.rm = T)
    returnList <- c(meanVal, sdVal, meanResid, meanObsv, sdObsv)
    return(returnList)
  }
  if(sf::st_is_longlat(poly_from) || sf::st_is_longlat(poly_to)){
    # Reproject to equal area
    if(!grepl("4326",sf::st_crs(poly_from)["input"]$input)){
    poly_from <- sf::st_transform(poly_from,crs=4326)
    poly_to <- sf::st_transform(poly_to,crs=4326)
    }
    xy0 <- data.table::as.data.table(sf::st_coordinates(poly_to))[,lapply(.SD,median),.SDcols=c("X","Y")]
    poly_from <- sf::st_transform(poly_from, crs = paste0("+proj=laea +lon_0=",xy0[,X]," +lat_0=",xy0[,Y]))
    poly_to <- sf::st_transform(poly_to, crs = paste0("+proj=laea +lon_0=",xy0[,X]," +lat_0=",xy0[,Y]))
  }

  ## Intersecting Polygon Boundaries
  if(verbose==TRUE){message('Part 1 - Intersecting Polygon Boundaries')}
  MainVariable <- strsplit(varz, '~')[[1]][1]
  MainVariable <- trimws(MainVariable, which = 'both')
  poly_from$fromID <- 1:nrow(poly_from)
  poly_to$toID <- 1:nrow(poly_to)
  colSelFrom <- which(names(poly_from)%in%'fromID')
  colSelTo <- which(names(poly_to)%in%'toID')
  names(poly_from)[names(poly_from)%in%MainVariable] <- 'Primary'
  poly_from$Replace <- poly_from$Primary
  names(poly_from)[names(poly_from)%in%'Replace'] <- MainVariable
  if(extensive){
    poly_from$Density <- poly_from$Primary/sum(poly_from$Primary, na.rm = T)
    TotalVar <- sum(poly_from$Primary, na.rm = T)
    varz_ <- varz
    varz <- 'Density'
  }
  suppressWarnings({
    Intersection <- sf::st_collection_extract(sf::st_intersection(poly_from[,colSelFrom],poly_to[,colSelTo]), 'POLYGON')
  })
  Intersection$IDintersect <- 1:nrow(Intersection)
  Intersection$Area <- as.numeric(sf::st_area(Intersection))
  Intersection$Weight <- Intersection$Area/sum(Intersection$Area, na.rm = T)
  # if(!is.null(PopWeights)){
  #   # ????
  # }
  suppressWarnings({
    Intersection_centroid <- sf::st_coordinates(sf::st_centroid(Intersection))
  })
  Intersection$lon <- Intersection_centroid[,1]
  Intersection$lat <- Intersection_centroid[,2]
  poly_fromWO <- poly_from
  sf::st_geometry(poly_fromWO) <- NULL
  Intersection <- dplyr::left_join(Intersection, poly_fromWO, by = 'fromID')
  IntersectionWO <- Intersection
  sf::st_geometry(IntersectionWO) <- NULL
  IntersectionWO <- data.table::as.data.table(IntersectionWO)
  weightCollapse <- IntersectionWO[,list('N' = .N, 'Weight' = 1/.N), by = list(fromID)]
  suppressMessages({
    IntersectionWO <- dplyr::left_join(IntersectionWO, weightCollapse)
  })
  if(!extensive){
    names(IntersectionWO)[names(IntersectionWO)%in%MainVariable] <- 'Observed'
    IntersectionWO$Replace <- IntersectionWO$Observed
    names(IntersectionWO)[names(IntersectionWO)%in%'Replace'] <- MainVariable
  }

  ## Fitting TPRS to Point Locations
  if(verbose==TRUE){message('Part 2 - Fitting TPRS to Point Locations')}
  if(grepl('~', varz)){
    primaryFormula <- as.formula(paste(varz, ' + te(lon, lat, bs = c(\'tp\', \'tp\'), k = c(', k, ',', k, '))'))
  } else {
    primaryFormula <- as.formula(paste(varz, ' ~ te(lon, lat, bs = c(\'tp\', \'tp\'), k = c(', k, ',', k, '))'))
  }
  if(extensive==TRUE){
    IntersectionWO$Density[IntersectionWO$Density<0] <- 0
    IntersectionWO$Density[IntersectionWO$Density>1] <- 1
  }
  suppressWarnings({
    TPRS_Model <- mgcv::bam(primaryFormula, 
                            data = IntersectionWO, 
                            weights = IntersectionWO$Weight, nthreads = threads, family = linkFunction, discrete = T)
  })
  IntersectionWO_predict <- stats::predict(TPRS_Model, newdata = IntersectionWO, se.fit = TRUE)
  IntersectionWO$Fit <- as.vector(IntersectionWO_predict$fit)
  IntersectionWO$SE <- as.vector(IntersectionWO_predict$se.fit)
  IntersectionWO$Resid <- 0
  if(length(TPRS_Model$na.action)){
    IntersectionWO$Resid[-TPRS_Model$na.action] <- stats::resid(TPRS_Model, type = 'deviance')
  } else {
    IntersectionWO$Resid <- stats::resid(TPRS_Model, type = 'deviance')
  }
  Intersection$Fit <- IntersectionWO$Fit
  Intersection$SE <- IntersectionWO$SE
  Intersection$Resid <- IntersectionWO$Resid
  Intersection$Predict_lp <- Intersection$Fit + IntersectionWO$Resid
  Intersection$Predict <- linkFunction$linkinv(Intersection$Predict_lp)
  Intersection$Upper_lp <- Intersection$Predict_lp + critical*Intersection$SE
  Intersection$Upper <- linkFunction$linkinv(Intersection$Predict + critical*Intersection$SE)
  Intersection$Lower_lp <- Intersection$Predict - critical*Intersection$SE
  Intersection$Lower <- linkFunction$linkinv(Intersection$Predict - critical*Intersection$SE)

  ## Bootstrapping poly_from to identify TPRS errors
  if(verbose==TRUE){message('Part 3 - Bootstrapping poly_from to identify TPRS errors')}
  SubsetFROMpoints <- IntersectionWO %>% group_by(fromID) %>% group_split()
  lfun <- ifelse(verbose==TRUE,pbapply::pblapply(cl=1),lapply)
  BootstrapEstimatesFROM <- lfun(1:length(SubsetFROMpoints), function(iter){
    lenVal <- length(SubsetFROMpoints[[iter]])
    selCols <- which(names(SubsetFROMpoints[[iter]])%in%c('Fit', "SE", "Resid", 'Observed', 'Weight'))
    bootcorr <- boot::boot(SubsetFROMpoints[[iter]][,selCols], fc, R = boot.num, 
                     weights = SubsetFROMpoints[[iter]]$Weight)
    columnAverages <- colMeans(bootcorr$t)
    returnFrame <- data.table::data.table('ID' = unique(SubsetFROMpoints[[iter]]$fromID), 
                              'Fit' = columnAverages[1],
                              'SE' = columnAverages[2],
                              'Resid' = columnAverages[3],
                              'AW' = columnAverages[4],
                              'AW_SD' = columnAverages[5])
    return(returnFrame)
  })
  BootstrapEstimatesFROM <- data.table::rbindlist(BootstrapEstimatesFROM)
  names(BootstrapEstimatesFROM)[names(BootstrapEstimatesFROM)%in%'ID'] <-'fromID'
  geometryFrom <- sf::st_geometry(poly_from)
  sf::st_geometry(poly_from) <- NULL
  poly_from <- dplyr::left_join(poly_from, BootstrapEstimatesFROM, by = c('fromID'))
  sf::st_geometry(poly_from) <- geometryFrom
  poly_from$Predict_lp <- poly_from$Fit
  poly_from$Predict <- linkFunction$linkinv(poly_from$Predict_lp)
  poly_from$Upper_lp <- poly_from$Predict_lp + critical*poly_from$SE
  poly_from$Upper <- linkFunction$linkinv(poly_from$Predict_lp + critical*poly_from$SE)
  poly_from$Lower_lp <- poly_from$Predict_lp - critical*poly_from$SE
  poly_from$Lower <- linkFunction$linkinv(poly_from$Predict_lp - critical*poly_from$SE)
  poly_from$Reconstruction_Error <- poly_from$Primary - poly_from$AW
  poly_from$TPRS_Error <- poly_from$Primary - poly_from$Predict

  ## Bootstrapping poly_to to generate TPRS-AW estimates
  if(verbose==TRUE){message('Part 4 - Bootstrapping poly_to to generate TPRS-AW estimates')}
  SubsetTOpoints <- dplyr::group_split(dplyr::group_by(IntersectionWO,toID))
  BootstrapEstimatesTO <- lfun(1:length(SubsetTOpoints), function(iter){
    lenVal <- nrow(SubsetTOpoints[[iter]])
    selCols <- which(names(SubsetTOpoints[[iter]])%in%c('Fit', "SE", "Resid", 'Observed', 'Weight'))
    bootcorr <- boot::boot(SubsetTOpoints[[iter]][,selCols], fc, R = boot.num, 
                     weights = SubsetTOpoints[[iter]]$Weight)
    columnAverages <- colMeans(bootcorr$t)
    returnFrame <- data.table::data.table('ID' = unique(SubsetTOpoints[[iter]]$toID), 
                              'N_AW' = lenVal,
                              'Fit' = columnAverages[1],
                              'SE' = columnAverages[2],
                              'Resid' = columnAverages[3],
                              'AW' = columnAverages[4],
                              'AW_SD' = columnAverages[5])
    return(list(returnFrame, bootcorr))
  })
  BootValuesTO <- lapply(BootstrapEstimatesTO, function(i) i[[2]])
  BootstrapEstimatesTO <- lapply(BootstrapEstimatesTO, function(i) i[[1]])
  BootstrapEstimatesTO <- data.table::rbindlist(BootstrapEstimatesTO)
  names(BootstrapEstimatesTO)[names(BootstrapEstimatesTO)%in%'ID'] <-'toID'
  geometryTO <- sf::st_geometry(poly_to)
  sf::st_geometry(poly_to) <- NULL
  poly_to <- dplyr::left_join(poly_to, BootstrapEstimatesTO, by = c('toID'))
  sf::st_geometry(poly_to) <- geometryTO
  poly_to$Predict_lp <- poly_to$Fit
  poly_to$Predict <- linkFunction$linkinv(poly_to$Predict_lp)
  poly_to$AW_SD[is.na(poly_to$AW_SD)] <- 0
  poly_to$Upper_lp <- poly_to$Predict_lp + critical*poly_to$SE
  poly_to$Upper <- linkFunction$linkinv(poly_to$Predict_lp + critical*poly_to$SE)
  poly_to$Upper_AW <- poly_to$AW + critical*poly_to$AW_SD
  poly_to$Lower_lp <- poly_to$Predict_lp - critical*poly_to$SE
  poly_to$Lower <- linkFunction$linkinv(poly_to$Predict_lp - critical*poly_to$SE)
  poly_to$Lower_AW <- poly_to$AW - critical*poly_to$AW_SD
  if(extensive){
    poly_to_ <- poly_to 
    poly_to$PredictDensity <- poly_to$Predict/sum(poly_to$Predict, na.rm = T)
    poly_to$LowerDensity <- poly_to$Lower/sum(poly_to$Lower, na.rm = T)
    poly_to$UpperDensity <- poly_to$Upper/sum(poly_to$Upper, na.rm = T)
    poly_to$Predict <- poly_to$PredictDensity*TotalVar
    poly_to$Lower <- poly_to$LowerDensity*TotalVar
    poly_to$Upper <- poly_to$UpperDensity*TotalVar
  }

  # RETURN RESULTS
  timeStop <- Sys.time()
  if(return_list==TRUE){
    returnObj <- list('poly_from' = poly_from,
                      'poly_to' = poly_to,
                      'Intersection' = Intersection,
                      'Fitted_Model' = TPRS_Model,
                      'Boot_Estimates' = BootValuesTO,
                      'Time' = timeStop - timeStart)
  } else {returnObj <- poly_to}
  return(returnObj)
}





############################################
## Permutation test
############################################



onesamplePermutation <- function(input_data,
                                 iterations = NULL,
                                 statistic = NULL,
                                 signs = FALSE,
                                 type = 'two-way',
                                 seed = 123,
                                 verbose = FALSE){
  # Set seed
  set.seed(seed)

  # Set lapply function
  lfun <- ifelse(verbose==TRUE,pbapply::pblapply(cl=1),lapply)

  # Part A - 
  input_data = input_data 
  iterations =  if(is.null(iterations)){length(input_data)}else{iterations} 
  statistic =  if(is.null(statistic)){input_data - mean(input_data, na.rm = T)}else{statistic}
  
  # Part B - 
  Permutation_Results <- lfun(1:length(input_data), function(i){
    sample_replicates <- sample(input_data, iterations, replace = T)
    returnObj <- c(statistic[i], sample_replicates - mean(input_data, na.rm = T))
    return(returnObj)
  })
  
  # Part C - 
  Permutation_Results_PValue <- lfun(Permutation_Results, function(item){
    if(type%in%'two-way'){
      outVEC <- sum(abs(item[-1]) >= abs(item[1]))/length(item[-1])
    } 
    if(type%in%'less'){
      outVEC <- sum(item[-1] <= item[1])/length(item[-1])
    } 
    if(type%in%'greater'){
      outVEC <- sum(item[-1] >= item[1])/length(item[-1])
    } 
    return(outVEC)
  })
  
  return(unlist(Permutation_Results_PValue))
}



############################################
## CoS diagnostic table
############################################

diag_tab <- function(source_, method_, lat_out_,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest){
  # Set values
  out <- lapply(seq_along(varz_),function(i){
    # Diagnostics table
    data.table::data.table(
      source = source_,
      method = method_,
      n_macro_1 = nrow(votez_source_),
      n_macro_2 = nrow(votez_dest_),
      rn = nest_coef_$rn,
      rs = nest_coef_$rs,
      rn_sym = nest_coef_$rn_sym,
      rs_sym = nest_coef_$rs_sym,
      rn_alt = nest_coef_$rn_alt,
      rs_alt = nest_coef_$rs_alt,
      p_intact = nest_coef_$p_intact,
      full_nest = nest_coef_$full_nest,
      rn_nn = nest_coef_$rn_nn,
      rs_nn = nest_coef_$rs_nn,
      ro = nest_coef_$ro,
      gmi = nest_coef_$gmi,
      varz = varz_[i],
      extensive = extensive_[i],
      diff = lat_out_[,(get(varz_[i])-get(varz2_[i])) %>% mean(na.rm=T)],
      abs_diff = lat_out_[,abs(get(varz_[i])-get(varz2_[i])) %>% mean(na.rm=T)],
      prop_diff = lat_out_[,abs(get(varz_[i])-get(varz2_[i]))/get(varz_[i])]  %>% mean(na.rm=T),
      std_diff = lat_out_[,abs(get(varz_[i])-get(varz2_[i]))/sd(get(varz_[i]),na.rm=T)] %>% mean(na.rm=T),
      rmse = lat_out_[,rmse(get(varz_[i]),get(varz2_[i]))],
      nrmse = lat_out_[,nrmse(get(varz_[i]),get(varz2_[i]))],
      nrmse_m = lat_out_[,nrmse(get(varz_[i]),get(varz2_[i]),basis="mean")],
      moran_p = tryCatch({ifelse(lat_out_[,sd(get(varz_[i])-get(varz2_[i]),na.rm=T)==0],NA_real_,lat_out_[,spdep::moran.test(get(varz_[i])-get(varz2_[i]), listw=lw_dest_,zero.policy = TRUE,na.action=na.omit)]  %>% .["p.value"] %>% as.numeric())},error=function(e){NA}),
      beta_hat = lat_out_ %>% .[,.SD,.SDcols=c(paste0("Y_",varz_a[i]),varz2_a[i])] %>% na.omit() %>% .[,lm(get(paste0("Y_",varz_a[i]))~get(varz2_a[i])) %>% coef(.) %>% .[2] %>% as.numeric()],
      se_hat = lat_out_ %>% .[,.SD,.SDcols=c(paste0("Y_",varz_a[i]),varz2_a[i])] %>% na.omit() %>% .[,lm(get(paste0("Y_",varz_a[i]))~get(varz2_a[i])) %>% summary(.) %>% coef() %>% .[2,2] %>% as.numeric()],
      beta = lat_out_ %>% .[,.SD,.SDcols=c(paste0("Y_",varz_a[i]),varz_a[i])] %>% na.omit() %>% .[,lm(get(paste0("Y_",varz_a[i]))~get(varz_a[i])) %>% coef(.) %>% .[2] %>% as.numeric()],
      se = lat_out_ %>% .[,.SD,.SDcols=c(paste0("Y_",varz_a[i]),varz_a[i])] %>% na.omit() %>% .[,lm(get(paste0("Y_",varz_a[i]))~get(varz_a[i])) %>% summary(.) %>% coef() %>% .[2,2] %>% as.numeric()],
      spcor = lat_out_[,cor.test(get(varz_a[i]), get(varz2_a[i]), method = 'spearman') %>% .["estimate"] %>% as.numeric()],
      missing = lat_out_ %>% .[,mean(is.na(get(varz2_a[i])))]
    )
  }) %>% dplyr::bind_rows()
  return(out)
}



############################################
## CoS diagnostic table (Monte Carlo)
############################################

diag_tab_mc <- function(sim_,source_, method_=NA_character_, lat_out_,n_macro_1_=n_macro_1, n_macro_2_=n_macro_2, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest){
  # Set values
  out <- lapply(seq_along(varz_),function(i){
    # Diagnostics table
    data.table::data.table(
      sim = sim_,
      source = source_,
      method = method_,
      n_macro_1 = get(paste0("n_macro_",n0)),
      n_macro_2 = get(paste0("n_macro_",c(1:2)[!c(1:2)%in%n0])),
      rn = nest_coef_$rn,
      rs = nest_coef_$rs,
      rn_sym = nest_coef_$rn_sym,
      rs_sym = nest_coef_$rs_sym,
      rn_alt = nest_coef_$rn_alt,
      rs_alt = nest_coef_$rs_alt,
      p_intact = nest_coef_$p_intact,
      full_nest = nest_coef_$full_nest,
      rn_nn = nest_coef_$rn_nn,
      rs_nn = nest_coef_$rs_nn,
      ro = nest_coef_$ro,
      gmi = nest_coef_$gmi,
      varz = varz_[i],
      true_mean = lat_out_ %>% .[,mean(get(varz_[i]),na.rm=TRUE)],
      true_se = lat_out_ %>% .[,sd(get(varz_[i]),na.rm=TRUE)],
      diff = lat_out_[,(get(varz_[i])-get(varz2_[i])) %>% mean(na.rm=T)],
      abs_diff = lat_out_[,abs(get(varz_[i])-get(varz2_[i])) %>% mean(na.rm=T)],
      prop_diff = lat_out_[,abs(get(varz_[i])-get(varz2_[i]))/get(varz_[i])]  %>% mean(na.rm=T),
      std_diff = lat_out_[,abs(get(varz_[i])-get(varz2_[i]))/sd(get(varz_[i]),na.rm=T)] %>% mean(na.rm=T),
      rmse = lat_out_[,rmse(get(varz_[i]),get(varz2_[i]))],
      nrmse = lat_out_[,nrmse(get(varz_[i]),get(varz2_[i]))],
      nrmse_m = lat_out_[,nrmse(get(varz_[i]),get(varz2_[i]),basis="mean")],
      moran_p_source = tryCatch({ifelse(macro_votez_source[,sd(get(varz_[i]),na.rm=T)==0],NA_real_,macro_votez_source[,spdep::moran.test(get(varz_[i]), listw=macro_lw_source,na.action=na.exclude, zero.policy = TRUE)]  %>% .["p.value"] %>% as.numeric())},error=function(e){NA}),
      moran_p = tryCatch({ifelse(lat_out_[,sd(X-X_HAT,na.rm=T)==0],NA_real_,lat_out_[,spdep::moran.test(X-X_HAT, listw=macro_lw_dest,na.action=na.exclude, zero.policy = TRUE)]  %>% .["p.value"] %>% as.numeric())},error=function(e){NA}),
      moran_p_w = tryCatch({ifelse(lat_out_[,sd(X_W-X_W_HAT,na.rm=T)==0],NA_real_,lat_out_[,spdep::moran.test(X-X_HAT, listw=macro_lw_dest,na.action=na.exclude, zero.policy = TRUE)]  %>% .["p.value"] %>% as.numeric())},error=function(e){NA}),
      beta_hat = lat_out_ %>% .[,.(Y,X_HAT)] %>% na.omit() %>% .[,lm(Y~X_HAT) %>% coef_ex(2,1)],
      beta_hat_w = lat_out_ %>% .[,.(Y_W,X_W_HAT)] %>% na.omit() %>% .[,lm(Y_W~X_W_HAT) %>% coef_ex(2,1)],
      beta = lat_out_ %>% .[,.(Y,X)] %>% na.omit() %>% .[,lm(Y~X) %>% coef(2,1) %>% .[2]],
      beta_w = lat_out_ %>% .[,.(Y_W,X_W)] %>% na.omit() %>% .[,lm(Y_W~X_W) %>% coef_ex(2,1)],
      se_hat = lat_out_ %>% .[,.(Y,X_HAT)] %>% na.omit() %>% .[,lm(Y~X_HAT) %>% coef_ex(2,2)],
      se_hat_w = lat_out_ %>% .[,.(Y_W,X_W_HAT)] %>% na.omit() %>% .[,lm(Y_W~X_W_HAT) %>% coef_ex(2,2)],
      se = lat_out_ %>% .[,.(Y,X)] %>% na.omit() %>% .[,lm(Y~X) %>% coef_ex(2,2)],
      se_w = lat_out_ %>% .[,.(Y_W,X_W)] %>% na.omit() %>% .[,lm(Y_W~X_W) %>% coef_ex(2,2)],
      spcor = lat_out_[,cor.test(X, X_HAT, method = 'spearman') %>% .["estimate"] %>% as.numeric()],
      spcor_w = lat_out_[,cor.test(X_W, X_W_HAT, method = 'spearman') %>% .["estimate"] %>% as.numeric()],
      rmse_mod = lat_out_ %>% .[,.(Y,X_HAT)] %>% na.omit() %>% .[,rmse(Y,(lm(Y~X_HAT) %>% stats::fitted.values()))],
      rmse_mod_w = lat_out_ %>% .[,.(Y_W,X_W_HAT)] %>% na.omit() %>% .[,rmse(Y_W,(lm(Y_W~X_W_HAT) %>% stats::fitted.values()))],
      missing = lat_out_ %>% .[,mean(is.na(get(varz2_[i])))],
      perm_p05 = mean(onesamplePermutation(lat_out_[,get(varz_[i])])<.05),
      perm_p05_hat = mean(onesamplePermutation(lat_out_[,get(varz2_[i])])<.05),
      est_mean = lat_out_ %>% .[,mean(get(varz2_[i]),na.rm=TRUE)],
      est_se = lat_out_ %>% .[,sd(get(varz2_[i]),na.rm=TRUE)],
      predint_sd025 = NA_real_,
      predint_sd05 = NA_real_,
      predint_sd1 = NA_real_,
      predint_sd2 = NA_real_,
      predint_95ci = NA_real_
    )
  }) %>% dplyr::bind_rows()
  return(out)
}