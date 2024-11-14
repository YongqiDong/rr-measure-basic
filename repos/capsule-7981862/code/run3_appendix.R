##############################################################################
##############################################################################
##############################################################################
##############################################################################
## Replication code for:
## Zhukov, Byers, Davidson, and Kollman,
## "Integrating Data Across Misaligned Spatial Units," 
## Political Analysis (conditionally accepted 10/2022)
## ONLINE APPENDIX
##############################################################################
##############################################################################
##############################################################################
##############################################################################

# Print upon starting
print("**** Starting run3_appendix.R ****")

##############################################################################
##############################################################################
## Table A2.2
## Correlation between alternative nesting metrics.
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Table A2.2")

  # Clear workspace
  rm(list=ls())

  # Source functions
  source("code/functions.R")
  out_dir <- "results/"

  # Labels
  mvarmat <- data.table::data.table(mvar=c("id_match","largest_overlap","simple_overlay","area_int_poly","area_int_sf","area_int_point","pop_int_poly","pop_int_point","tps_default","tps_wresid","tps_mh","tps_mhwresid","tprs_aw","krige_ordinary","krige_universal","rasterization","mean","meta"),mlab=c("ID match","Overlay (polygons)","Overlay (centroids)","Area Weights (polygons)","Area Weights (polygons) (sf)","Area Weights (centroids)","Pop Weights (polygons)","Pop Weights (centroids)","TPRS-Forest","TPRS-Forest (w/ resid)","TPRS-Forest (MH)","TPRS-Forest (MH, w/ resid)","TPRS-Area Weights","Ordinary Kriging","Universal Kriging","Rasterization","Mean","Meta")) %>% .[,mlab := factor(mlab,levels=rev(mlab))]

  # Combine simulation results
  extnz <- TRUE
  filez <- list.files("data/r_output/",pattern=ifelse(extnz,"_ext","_int"),full.names=TRUE) %>% grep("sims",.,value=TRUE) 
  sims_mat_ <- lapply(filez,function(f0){readRDS(f0) %>% dplyr::bind_rows()}) %>% data.table::rbindlist(fill=TRUE) %>% .[,bias := beta_hat-beta] %>% .[,bias_w := beta_hat_w-beta_w] %>% .[,method:=factor(method,levels=mvarmat[,mvar])]  %>% .[,proc_time_sec := NA_real_] %>% .[grep("secs",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric()] %>% .[grep("mins",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*60})] %>% .[grep("hours",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*3600})] %>% .[,n_ratio_21 := n_macro_2/n_macro_1] %>% .[,n_ratio_12 := n_macro_1/n_macro_2] %>% .[!method%in%c("area_int_sf")] %>% .[,diverge:=rs-rn] %>% data.table::setnames(c("rn_full","rs_full"),c("rn_nn","rs_nn"),skip_absent=TRUE)

  # Correlation matrix for nesting metrics
  metrix <- data.table::data.table(
    varz=c("rn","rs","rn_sym","rs_sym","rn_nn","rs_nn","p_intact","full_nest","ro"),
    labz=c("RN","RS","RN-sym","RS-sym","RN-nn","RS-nn","PI","PFN","RO")
    )
  cmatz <- sims_mat_[,.SD%>%cor(use="complete.obs")%>%round(2),.SDcols=metrix[,varz]] %>% data.table::as.data.table() %>% data.table::setnames(metrix[,labz]) %>% .[,Metric := metrix[,labz]] %>% dplyr::select(Metric,dplyr::everything()) %>% xtable::xtable(caption="Correlation between alternative nesting metrics.",label="tab:cor_metrix")

  # Save to file
  cmatz %>% xtable::print.xtable(file=paste0(out_dir,"tabA2_2.tex"),include.rownames=FALSE,booktabs=TRUE,size="footnotesize")

  print("FINISHED: Table A2.2")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})

##############################################################################
##############################################################################
## Table A3.3
## Relative performance of nesting metrics in explaining transformation quality
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Table A3.3")
  
  # Variable labels
  labmat <- data.table::data.table(
    var=c("rs","rs_sym","rn","rn_sym","abs(diff)","log(rmse)","rmse","log(rmse_mod)","log(rmse_mod_w)","rmse_mod","rmse_mod_w","log(nrmse)","nrmse","log(nrmse_m)","nrmse_m","spcor_w","bias_w","abs(bias_w)","spcor","bias","proc_time_sec","diff","n_ratio_12","n_ratio_21","p_intact","full_nest","rn_full","rs_full","rn_nn","rs_nn","mean_splits","moran_p_source","moran_p_w","predint_sd025","predint_sd05","predint_sd1","predint_sd2","predint_95ci","rn_alt","rs_alt","ro","diverge"),
    lab=c("Relative scale (RS)","Relative scale, symmetric (RS-sym)","Relative nesting (RN)","Relative nesting, symmetric (RN-sym)","Absolute residuals","log(RMSE)","RMSE","log(RMSE Y)","log(RMSE Y)","RMSE Y","RMSE Y","log(NRMSE)","NRMSE","log(NRMSE)","NRMSE","Spearman's correlation","OLS estimation bias","OLS estimation bias (absolute)","Spearman's correlation","OLS estimation bias","Processing time (sec.)","Residuals","N source units / N destination units","N destination units / N source units","Proportion intact (PI)","Proportion fully nested (PFN)","RN non-nested (RN-nn)","RS non-nested (RS-nn)","RN non-nested (RN-nn)","RS non-nested (RS-nn)","Average number of splits (ANS)","Source Moran's I p-value","Moran's I p-value, residuals","Coverage 1/4 SD","Coverage 1/2 SD","Coverage 1 SD","Coverage 2 SD","Coverage 95% CI","Relative nesting, alternative (RN-alt)","Relative scale, alternative (RS-alt)","Relative overlap (RO)","Divergence (RS-RN)"))

  # Run GAM models on all other nesting metrics
  X <- data.table::copy(sims_mat_)
  gmodmat <- parallel::mclapply(c("rn","rs","rn_sym","rs_sym","rn_nn","rs_nn","p_intact","full_nest","ro"),function(xvar){
    lapply(c(ifelse(extnz==TRUE,"log(nrmse)","log(rmse)"),"spcor_w","bias_w"),function(yvar){
      print(paste("y=",yvar,", x=",xvar))
      formz <- paste0(yvar," ~ splines::ns(",xvar,", 3) + as.factor(method) ") %>% as.formula() %>% Formula::as.Formula()
      gmod <- mgcv::gam(formz,data=X)
      data.table::data.table(yvar=yvar,ylab=yvar%>%match(labmat[,var])%>%labmat[.,lab],xvar=xvar,xlab=xvar%>%match(labmat[,var])%>%labmat[.,lab]%>%stringr::str_extract_all("\\(([^()]+)\\)") %>% .[[1]]%>%gsub("\\(([^()]+)\\)", "\\1", .),aic=stats::AIC(gmod),bic=stats::BIC(gmod),deviance=gmod$deviance,deviance_null=gmod$null.deviance,ssr=sum(gmod$residuals^2),nsdf=gmod$nsdf,df_resid=gmod$df.residual,edf_min=gmod$min.edf)
    }) %>% data.table::rbindlist(fill=TRUE)
  },mc.cores=parallel::detectCores()-1) %>% data.table::rbindlist(fill=TRUE)

  # Full tournament
  horse_2 <- lapply(c(ifelse(extnz==TRUE,"log(nrmse)","log(rmse)"),"spcor_w","bias_w"),function(y){
    gmodmat %>% .[yvar%in%y,.(ylab,xlab,aic,bic,deviance)] %>% .[order(bic)]
  }) %>% data.table::rbindlist() %>% .[duplicated(ylab),ylab := ""] %>% data.table::setnames(c("ylab","xlab","aic","bic","deviance"),c("Diagnostic","Metric","AIC","BIC","Deviance"))
  horse_2

  # Save to file
  horse_2 %>% xtable::xtable(caption="Relative performance of nesting metrics in explaining transformation quality.",label="tab:horse_2")%>%xtable::print.xtable(booktabs=TRUE,hline.after=c(-1,0,gmodmat[,length(unique(xvar))],gmodmat[,length(unique(xvar))*2],nrow(.)),include.rownames=FALSE,file=paste0(out_dir,"tabA3_3",ifelse(extnz,"_ext","_int"),".tex"),table.placement = "H",size="footnotesize")

  print("FINISHED: Table A3.3")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})


##############################################################################
##############################################################################
## Tables A3.4 and A3.5
## Likelihood ratio tests 
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Table A3.4, A3.5")

  # Include "stars"?
  starz <- FALSE

  # Loop over extensive and intensive variables
  extnz <- FALSE
  for(extnz in c(TRUE,FALSE)){

    # Combine simulation results
    filez <- list.files("data/r_output/",pattern=ifelse(extnz,"_ext","_int"),full.names=TRUE) %>% grep("sims",.,value=TRUE) 
    sims_mat_ <- lapply(filez,function(f0){readRDS(f0) %>% dplyr::bind_rows()}) %>% data.table::rbindlist(fill=TRUE) %>% .[,bias := beta_hat-beta] %>% .[,bias_w := beta_hat_w-beta_w] %>% .[,method:=factor(method,levels=mvarmat[,mvar])]  %>% .[,proc_time_sec := NA_real_] %>% .[grep("secs",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric()] %>% .[grep("mins",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*60})] %>% .[grep("hours",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*3600})] %>% .[,n_ratio_21 := n_macro_2/n_macro_1] %>% .[,n_ratio_12 := n_macro_1/n_macro_2] %>% .[!method%in%c("area_int_sf")] %>% .[,diverge:=rs-rn] %>% data.table::setnames(c("rn_full","rs_full"),c("rn_nn","rs_nn"),skip_absent=TRUE)

    # Likelihood ratio test considering nested models
    X <- data.table::copy(sims_mat_)
    yvar <- "bias_w"
    lrt_mat <- parallel::mclapply(c(ifelse(extnz==TRUE,"log(nrmse)","log(rmse)"),"spcor_w","bias_w"),function(yvar){print(yvar)
      lapply(c("rn","rs"),function(xvar1){
        lapply(c("rn","rs"),function(xvar2){
          if(xvar1!=xvar2){
            if(xvar1%in%c("rn","rs")){xz1 <- seq(0,1,length.out=101)}else if(xvar1%in%c("perm_p05")){xz1 <- seq(0,.25,length.out=101)}else{xz1 <- seq(0,1,length.out=101)}
            if(xvar2%in%c("rn","rs")){xz2 <- seq(0,1,length.out=101)}else if(xvar2%in%c("perm_p05")){xz2 <- seq(0,.25,length.out=101)}else{xz2 <- seq(0,1,length.out=101)}
            formz <- paste0(yvar," ~ s(",xvar1,",",xvar2,") + as.factor(method) ") %>% as.formula() %>% Formula::as.Formula()
            formz_1 <- paste0(yvar," ~ splines::ns(",xvar1,",3) + as.factor(method) ") %>% as.formula() %>% Formula::as.Formula()
            formz_2 <- paste0(yvar," ~ splines::ns(",xvar1,",3) + splines::ns(",xvar2,",3) + as.factor(method) ") %>% as.formula() %>% Formula::as.Formula()
            formz_3 <- paste0(yvar," ~ splines::ns(",xvar1,",3)*splines::ns(",xvar2,",3) + as.factor(method) ") %>% as.formula() %>% Formula::as.Formula()
            gmod_ <- mgcv::gam(formz,data=X[varz=="X_W"] )
            gmod_1 <- mgcv::gam(formz_1,data=X[varz=="X_W"] )
            gmod_2 <- mgcv::gam(formz_2,data=X[varz=="X_W"] )
            gmod_3 <- mgcv::gam(formz_3,data=X[varz=="X_W"] )
            anov_stats <- stats::anova(gmod_1,gmod_2,gmod_3,test="LRT")%>%data.table::as.data.table() %>% .[,Model:=c(toupper(xvar1),paste0(toupper(xvar1),"$+$",toupper(xvar2)),paste0(toupper(xvar1),"$\\times$",toupper(xvar2)))] %>% .[,Outcome:=yvar%>%match(labmat[,var])%>%labmat[.,lab]] %>% .[,BIC:=c(stats::BIC(gmod_1),stats::BIC(gmod_2),stats::BIC(gmod_3))] %>% .[,eval("Pr(>Chi)"):=get("Pr(>Chi)")%>%findInterval(c(0,.001,.01,.05,.1,1))%>%(function(x){c(paste0("$<$0.001","***"),paste0(round(get("Pr(>Chi)"),2),"**"),paste0(round(get("Pr(>Chi)"),2),"*"),paste0(round(get("Pr(>Chi)"),2),"'"),paste0(round(get("Pr(>Chi)"),2),""))[x]})] %>% .[,Df:=round(Df,2)] %>% .[,Deviance:=round(Deviance,2)] %>% dplyr::select("Outcome","Model","BIC",dplyr::everything()) %>% replace(is.na(.),"")%>%data.table::setnames("Pr(>Chi)","Pr($>$Chi)")
            anov_stats
          }
        })%>%data.table::rbindlist()
      })%>%data.table::rbindlist()
    },mc.cores=parallel::detectCores()-1)%>%data.table::rbindlist()%>%.[!grepl("^RS",Model)] %>% .[!grepl(ifelse(extnz==TRUE,"\\(RMSE","\\(NRMSE"),Outcome)] %>% .[duplicated(Outcome,fromLast=FALSE),Outcome := ""]
    if(starz!=TRUE){lrt_mat[,eval("Pr($>$Chi)"):=get("Pr($>$Chi)")%>%gsub("*","",.,fixed=TRUE)]}

    # Save to file
    lrt_mat%>%xtable::xtable(caption=paste0("Likelihood ratio tests ",ifelse(extnz,"(extensive variable).","(intensive variable).")),label=paste0("tab:lrt",ifelse(extnz,"_ext","_int")))%>%xtable::print.xtable(booktabs=TRUE,hline.after=c(-1,0,3,6,nrow(.)),include.rownames=FALSE,table.placement = "H",file=paste0(out_dir,"tabA3_",ifelse(extnz,5,4),".tex"),size="footnotesize",sanitize.text.function = identity)

  # Close extnz loop
  }

  print("FINISHED: Table A3.4, A3.5")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})


##############################################################################
##############################################################################
## Figure A3.1
## How the distributions of RS and RN differ from each other
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Figure A3.1")

  # Combine simulation results
  extnz <- TRUE
  filez <- list.files("data/r_output/",pattern=ifelse(extnz,"_ext","_int"),full.names=TRUE) %>% grep("sims",.,value=TRUE) 
  sims_mat_ <- lapply(filez,function(f0){readRDS(f0) %>% dplyr::bind_rows()}) %>% data.table::rbindlist(fill=TRUE) %>% .[,bias := beta_hat-beta] %>% .[,bias_w := beta_hat_w-beta_w] %>% .[,method:=factor(method,levels=mvarmat[,mvar])]  %>% .[,proc_time_sec := NA_real_] %>% .[grep("secs",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric()] %>% .[grep("mins",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*60})] %>% .[grep("hours",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*3600})] %>% .[,n_ratio_21 := n_macro_2/n_macro_1] %>% .[,n_ratio_12 := n_macro_1/n_macro_2] %>% .[!method%in%c("area_int_sf")] %>% .[,diverge:=rs-rn] %>% data.table::setnames(c("rn_full","rs_full"),c("rn_nn","rs_nn"),skip_absent=TRUE)

  # Estimate logit model, extract predicted values
  X <- data.table::copy(sims_mat_)
  mod <- glm(rs~rn,data=X %>% .[,.(rn,rs)] %>%unique() ,family="binomial"); summary(mod)
  predz <- predict(mod,newdata=data.table::data.table(rn=seq(0,1,length.out=100)),type="response",se.fit=TRUE)

  # Render plots
  h_ <- 3; w_ <- h_#*1.618
  png(paste0(out_dir,"figA3_1a.png"), width = w_, height = h_, units = "in",res=300)
  hist_info <- X[,hist(rs,nclass=50, plot = FALSE)]
  hist_info$density <- hist_info$counts/sum(hist_info$counts) * 100
  par(mar=c(4,2,.5,.5))
  plot(hist_info, freq = FALSE,bty="n",cex=.1,xlab="Relative scale (RS)",ylab="Density (%)",main="",xlim=0:1, yaxt="n")   
  axis(2,at=pretty(hist_info$density),labels=pretty(hist_info$density)%>%paste0("%"))
  dev.off()

  png(paste0(out_dir,"figA3_1b.png"), width = w_, height = h_, units = "in",res=300)
  hist_info <- X[,hist(rn,nclass=50, plot = FALSE)]
  hist_info$density <- hist_info$counts/sum(hist_info$counts) * 100
  par(mar=c(4,2,.5,.5))
  plot(hist_info, freq = FALSE,bty="n",cex=.1,xlab="Relative nesting (RN)",ylab="Density (%)",main="",xlim=0:1, yaxt="n")   
  axis(2,at=pretty(hist_info$density),labels=pretty(hist_info$density)%>%paste0("%"))
  dev.off()

  png(paste0(out_dir,"figA3_1c.png"), width = w_, height = h_, units = "in",res=300)
  par(mar=c(4,4,.5,.5))
  plot(x=X %>% .[,.(rn,rs)] %>%unique() %>% .[,rn],y=X %>% .[,.(rn,rs)] %>%unique() %>% .[,rs],bty="n",cex=.1,xlab="Relative nesting (RN)",ylab="Relative scale (RS)",main="",col=rgb(0.5,0.5,0.5,.33),xlim=0:1,ylim=0:1)
  lines(x=seq(0,1,length.out=100),y=predz$fit,col="black",lwd=1)
  segments(x0=0,x1=1,y0=0,y1=1,lty=3)
  dev.off()

  print("FINISHED: Figure A3.1")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})



##############################################################################
##############################################################################
## Figure A3.2
## Transformation quality when RN and RS diverge
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Figure A3.2")

  # Loop over extensive and intensive variables
  extnz <- TRUE
  for(extnz in c(TRUE,FALSE)){

    # Combine simulation results
    filez <- list.files("data/r_output/",pattern=ifelse(extnz,"_ext","_int"),full.names=TRUE) %>% grep("sims",.,value=TRUE) 
    sims_mat_ <- lapply(filez,function(f0){readRDS(f0) %>% dplyr::bind_rows()}) %>% data.table::rbindlist(fill=TRUE) %>% .[,bias := beta_hat-beta] %>% .[,bias_w := beta_hat_w-beta_w] %>% .[,method:=factor(method,levels=mvarmat[,mvar])]  %>% .[,proc_time_sec := NA_real_] %>% .[grep("secs",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric()] %>% .[grep("mins",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*60})] %>% .[grep("hours",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*3600})] %>% .[,n_ratio_21 := n_macro_2/n_macro_1] %>% .[,n_ratio_12 := n_macro_1/n_macro_2] %>% .[!method%in%c("area_int_sf")] %>% .[,diverge:=rs-rn] %>% data.table::setnames(c("rn_full","rs_full"),c("rn_nn","rs_nn"),skip_absent=TRUE)

    # Loop over diagnostics
    for(x in c(ifelse(extnz==TRUE,"nrmse","rmse"),"bias_w","spcor_w")){print(paste0("x ",x))
      if(x %in% c("rmse","nrmse")){
        rankz <- X[,.(mean(get(x),na.rm=T),
          mean(get(x)[rs==0&rn>=quantile(rn[rs==0],.9)],na.rm=T),
          mean(get(x)[rs>.45&rs<.55&rn>=quantile(rn[rs>.45&rs<.55],.45,na.rm=T)&rn<=quantile(rn[rs>.45&rs<.55],.55,na.rm=T)],na.rm=T),
          mean(get(x)[rs==1&rn<=quantile(rn[rs==1],.1,na.rm=T)],na.rm=T)),by=method] %>% data.table::setnames(c("method","overall","low","med","high")) %>% .[,method := method %>% match(mvarmat[,mvar]) %>% mvarmat[.,mlab] %>% factor(levels=mvarmat[,mlab])] %>% .[order(method)] %>% .[, c("overall_rank") := dplyr::percent_rank(overall)]  %>% .[, c("low","med","high") %>% paste0("_rank") := unlist(.SD) %>% dplyr::percent_rank() %>% matrix(ncol=3,byrow=FALSE) %>% data.table::as.data.table(),.SDcols=c("low","med","high")] %>% .[,row:=.I %>% rev()]
        r_means <- rankz[,lapply(.SD,median,na.rm=TRUE),.SDcols=c("overall","low","med","high")]  %>% .[, c("overall_rank") := .5]  %>% .[, c("low","med","high") %>% paste0("_rank") := unlist(.SD) %>% dplyr::percent_rank() %>% matrix(ncol=3,byrow=FALSE) %>% data.table::as.data.table(),.SDcols=c("low","med","high")]
      }
      if(x %in% c("spcor","spcor_w")){
        rankz <- X[,.(
          mean(get(x),na.rm=T),
          mean(get(x)[rs==0&rn>=quantile(rn[rs==0],.9,na.rm=T)],na.rm=T),
          mean(get(x)[rs>.45&rs<.55&rn>=quantile(rn[rs>.45&rs<.55],.45,na.rm=T)&rn<=quantile(rn[rs>.45&rs<.55],.55,na.rm=T)],na.rm=T),
          mean(get(x)[rs==1&rn<=quantile(rn[rs==1],.1,na.rm=T)],na.rm=T)),by=method] %>% data.table::setnames(c("method","overall","low","med","high")) %>% .[,method := method %>% match(mvarmat[,mvar]) %>% mvarmat[.,mlab] %>% factor(levels=mvarmat[,mlab])] %>% .[order(method)]  %>% .[, c("overall_rank") := 1-dplyr::percent_rank(overall)]   %>% .[, c("low","med","high") %>% paste0("_rank") := 1-(unlist(.SD) %>% dplyr::percent_rank() %>% matrix(ncol=3,byrow=FALSE) %>% data.table::as.data.table()),.SDcols=c("low","med","high")] %>% .[,row:=.I %>% rev()]
        r_means <- rankz[,lapply(.SD,median,na.rm=TRUE),.SDcols=c("overall","low","med","high")]  %>% .[, c("overall_rank") := .5]   %>% .[, c("low","med","high") %>% paste0("_rank") := 1-(unlist(.SD) %>% dplyr::percent_rank() %>% matrix(ncol=3,byrow=FALSE) %>% data.table::as.data.table()),.SDcols=c("low","med","high")]
      } 
      if(x %in% c("bias","bias_w")){
        rankz <- X[,.(mean(get(x),na.rm=T),
          mean(get(x)[rs==0&rn>=quantile(rn[rs==0],.9,na.rm=T)],na.rm=T),
          mean(get(x)[rs>.45&rs<.55&rn>=quantile(rn[rs>.45&rs<.55],.45,na.rm=T)&rn<=quantile(rn[rs>.45&rs<.55],.55,na.rm=T)],na.rm=T),
          mean(get(x)[rs==1&rn<=quantile(rn[rs==1],.1,na.rm=T)],na.rm=T)),by=method] %>% data.table::setnames(c("method","overall","low","med","high")) %>% .[,method := method %>% match(mvarmat[,mvar]) %>% mvarmat[.,mlab] %>% factor(levels=mvarmat[,mlab])] %>% .[order(method)]  %>% .[, c("overall_rank") := dplyr::percent_rank(abs(overall))]  %>% .[, c("low","med","high") %>% paste0("_rank") := unlist(.SD) %>% abs() %>% dplyr::percent_rank() %>% matrix(ncol=3,byrow=FALSE) %>% data.table::as.data.table(),.SDcols=c("low","med","high")] %>% .[,row:=.I %>% rev()]
        r_means <- rankz[,lapply(.SD,median,na.rm=TRUE),.SDcols=c("overall","low","med","high")]  %>% .[, c("overall_rank") := .5]  %>% .[, c("low","med","high") %>% paste0("_rank") := unlist(.SD) %>% abs() %>% dplyr::percent_rank() %>% matrix(ncol=3,byrow=FALSE) %>% data.table::as.data.table(),.SDcols=c("low","med","high")]
      } 
      rankz <- data.table::rbindlist(list(rankz,r_means),fill=TRUE) %>% .[is.na(row),row:=0] %>% .[!is.na(overall)]
      h_ <- 4; w_ <- h_*1.618
      png(paste0(out_dir,"figA3_2",ifelse(extnz,"b_","a_"),x %>% gsub("\\(|\\)","",.) %>% stringr::str_trim(),".png"), width = w_, height = h_, units = "in",res=300)
      par(mar=c(0,11,2,0))
      plot(x=0,y=0,col=NA,ylim=rankz[,range(row)+c(0,1)],xlim=c(0,4),bty="n",axes=FALSE,xlab="",ylab="")
      axis(2,at=rankz[,row+.5],labels=c(rankz[!is.na(method),method%>%as.character()],"Median"),las=2,tick=FALSE,mgp=c(1,0,0),font=c(rep(1,nrow(rankz)),2))
      axis(3,at=0:3 + .5,labels=c("Overall","RS<RN","RS=RN","RS>RN"),tick=FALSE,mgp=c(1,-.5,0))
      for(j in 1:4){  for(i in rankz[,.I]){    rect(xleft=j-1,xright=j,ybottom=rankz[i,row],ytop=rankz[i,row+1],col=gray(rankz[i,c("overall","low","med","high") %>% paste0("_rank") %>% .[j] %>% get()]),border="white")  } }
      rect(xleft=0,xright=1,ybottom=rankz[,min(row)],ytop=rankz[,max(row)+1],col=NA,border="black",lwd=2)
      par(xpd=NA)
      rect(xleft=-1,xright=4,ybottom=0,ytop=1,col=NA,border="black",lwd=2)
      for(j in 1:4){  for(i in rankz[,.I]){    graphics::text(x=j-.5,y=rankz[i,row]+.5,labels=rankz[i,c("overall","low","med","high") %>% .[j] %>% get() %>% smart_round(2)],adj=.5,col=gray(ifelse({rankz[i,c("overall","low","med","high") %>% paste0("_rank") %>% .[j] %>% get()] } <.4,1,0) ))  } }
      title(paste0(x %>% match(labmat[,var]) %>% labmat[.,lab]),line=1)
      dev.off()
      #
    }

  # Close extnz loop
  }

  print("FINISHED: Figure A3.2")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})


##############################################################################
##############################################################################
## Figure A5.3
## Spatial data layers (Sweden)
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Figure A5.3")

  # Clear workspace
  rm(list=ls())
  source("code/functions.R")
  out_dir <- "results/"

  # Load geometries & reproject to planar
  swe_pct <- sf::st_read("data/SWE/VAL_SWE_2010_PCT_wgs.geojson") %>% SUNGEO::utm_select()
  swe_cst <- sf::st_read("data/SWE/GRED_SWE_2010_CST_wgs.geojson") %>% sf::st_transform(crs=sf::st_crs(swe_pct))
  swe_hex <- sf::st_read("data/SWE/HEXGRID_SWE_2020_HEX05_wgs.geojson") %>% sf::st_transform(crs=sf::st_crs(swe_pct))


  # Plot a
  h_ <- 3.5; w_ <- h_*.5
  png(paste0(out_dir,"figA5_3a.png"),width=w_,height=h_,res=300,units="in")
  par(mar=rep(0,4))
  plot(swe_pct["geometry"],lwd=.125)
  dev.off()

  # Plot b
  png(paste0(out_dir,"figA5_3b.png"),width=w_,height=h_,res=300,units="in")
  par(mar=rep(0,4))
  plot(swe_cst["geometry"],lwd=1)
  dev.off()

  # Plot c
  png(paste0(out_dir,"figA5_3c.png"),width=w_,height=h_,res=300,units="in")
  par(mar=rep(0,4))
  plot(swe_hex["geometry"],lwd=.5)
  dev.off()

  # Plot d
  png(paste0(out_dir,"figA5_3d.png"),width=w_,height=h_,res=300,units="in")
  par(mar=rep(0,4))
  plot(swe_cst["geometry"],lwd=1)
  plot(swe_pct["geometry"],lwd=.125,add=TRUE)
  dev.off()

  # Plot e
  png(paste0(out_dir,"figA5_3e.png"),width=w_,height=h_,res=300,units="in")
  par(mar=rep(0,4))
  plot(swe_cst["geometry"],lwd=1)
  plot(swe_hex["geometry"],lwd=.5,add=TRUE)
  dev.off()

  print("FINISHED: Figure A5.3")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})


##############################################################################
##############################################################################
## Table A5.6
## Relative scale and nesting of polygons in Figure A5.3.
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Table A5.6")

  # Calculate nesting coefficients for 3 sets of Georgia polygons
  nest_coef_ab <- SUNGEO::nesting(swe_pct,swe_cst,metrix=c("rs","rn"))
  nest_coef_ac <- SUNGEO::nesting(swe_pct,swe_hex,metrix=c("rs","rn"))
  nest_coef_ba <- SUNGEO::nesting(swe_cst,swe_pct,metrix=c("rs","rn"))
  nest_coef_bc <- SUNGEO::nesting(swe_cst,swe_hex,metrix=c("rs","rn"))
  nest_coef_ca <- SUNGEO::nesting(swe_hex,swe_pct,metrix=c("rs","rn"))
  nest_coef_cb <- SUNGEO::nesting(swe_hex,swe_cst,metrix=c("rs","rn"))

  # Generate tables
  tabA5_6a <- data.table::data.table(
    Layer = c("(a) Precincts","(b) Constituencies","(c) Grid cells"),
    a = c("--",smart_round(nest_coef_ba$rn,rnd=2,force_rnd=TRUE),smart_round(nest_coef_ca$rn,rnd=2,force_rnd=TRUE)),
    b = c(smart_round(nest_coef_ab$rn,rnd=2,force_rnd=TRUE),"--",smart_round(nest_coef_cb$rn,rnd=2,force_rnd=TRUE)),
    c = c(smart_round(nest_coef_ac$rn,rnd=2,force_rnd=TRUE),smart_round(nest_coef_bc$rn,rnd=2,force_rnd=TRUE),"--")
    ) %>% data.table::setnames(c("","(a)","(b)","(c)"))
  tabA5_6b <- data.table::data.table(
    Layer = c("(a) Precincts","(b) Constituencies","(c) Grid cells"),
    a = c("--",smart_round(nest_coef_ba$rs,rnd=2,force_rnd=TRUE),smart_round(nest_coef_ca$rs,rnd=2,force_rnd=TRUE)),
    b = c(smart_round(nest_coef_ab$rs,rnd=2,force_rnd=TRUE),"--",smart_round(nest_coef_cb$rs,rnd=2,force_rnd=TRUE)),
    c = c(smart_round(nest_coef_ac$rs,rnd=2,force_rnd=TRUE),smart_round(nest_coef_bc$rs,rnd=2,force_rnd=TRUE),"--")
    ) %>% data.table::setnames(c("","(a)","(b)","(c)"))

  # Save to disk
  xtable::print.xtable(xtable::xtable(tabA5_6a),file=paste0(out_dir,"tabA5_6a.tex"),floating=FALSE,booktabs=TRUE,add.to.row=list(pos=list(-1),command=c("\\toprule { Source} & \\multicolumn{3}{c}{ Destination}\\\\")), include.rownames=FALSE)
  xtable::print.xtable(xtable::xtable(tabA5_6b),file=paste0(out_dir,"tabA5_6b.tex"),floating=FALSE,booktabs=TRUE,add.to.row=list(pos=list(-1),command=c("\\toprule { Source} & \\multicolumn{3}{c}{ Destination}\\\\")), include.rownames=FALSE)

  print("FINISHED: Table A5.6")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})


##############################################################################
##############################################################################
## Change of support  (for Figures A5.4 and A5.5)
##############################################################################
##############################################################################

tryCatch({

  # Skip if working from intermediate files
  if(read.table("data/r_output/intermz_param.txt")[,"x"]==FALSE){

  print("STARTING: Figure A5.4")
  
  # Clear workspace
  rm(list=ls())

  # Source functions
  source("code/functions.R")
  out_dir <- "results/"
  verbose_warn <- FALSE

  # Labels
  mvarmat <- data.table::data.table(mvar=c("id_match","largest_overlap","simple_overlay","area_int_poly","area_int_point","pop_int_poly","pop_int_point","tps_default","tps_wresid","tps_mh","tps_mhwresid","tprs_aw","krige_ordinary","krige_universal","rasterization"),mlab=c("ID match","Overlay (polygons)","Overlay (centroids)","Area Weights (polygons)","Area Weights (centroids)","Pop Weights (polygons)","Pop Weights (centroids)","TPRS-Forest","TPRS-Forest (w/ resid)","TPRS-Forest (MH)","TPRS-Forest (MH, w/ resid)","TPRS-Area Weights","Ordinary Kriging","Universal Kriging","Rasterization")) %>% .[,mlab := factor(mlab,levels=rev(mlab))]

  # Set parameters
  true_beta <- 2.5

  # Set ID's
  cst_id <- "CST_CODE"
  pct_id <- "LKFV"
  hex_id <- "HEX05_CODE"

  # Load population raster
  r_pop <- raster::raster("data/SWE/GPW_SWE_2015_POP.asc",crs=" +init=epsg:4326")

  # Load geometries & reproject 
  swe_pct <- sf::st_read("data/SWE/VAL_SWE_2010_PCT_wgs.geojson") %>% sf::st_transform(crs=sf::st_crs(r_pop)) %>% data.table::as.data.table() %>% .[,eval(pct_id):=get(pct_id)%>%as.character()] %>% sf::st_as_sf()
  swe_cst <- sf::st_read("data/SWE/GRED_SWE_2010_CST_wgs.geojson") %>% sf::st_transform(crs=sf::st_crs(r_pop))
  swe_hex <- sf::st_read("data/SWE/HEXGRID_SWE_2020_HEX05_wgs.geojson") %>% sf::st_transform(crs=sf::st_crs(r_pop))

  # Neighbor lists
  pct_lw <- spdep::poly2nb(swe_pct,queen=TRUE) %>% spdep::nb2listw(style="W",zero.policy=TRUE)
  cst_lw <- spdep::poly2nb(swe_cst,queen=TRUE) %>% spdep::nb2listw(style="W",zero.policy=TRUE)
  hex_lw <- spdep::poly2nb(swe_hex,queen=TRUE) %>% spdep::nb2listw(style="W",zero.policy=TRUE)

  # Load elections data
  cst_votez <- readRDS("data/SWE/CLEA_SWE_2010_CST.RDS")
  pct_votez <- readRDS("data/SWE/CLEA_SWE_2010_PCT.RDS") %>% .[,eval(pct_id):=get(pct_id)%>%as.character()]

  # Names of numeric CLEA columns
  num_cols <- c("to1","pev1","vot1","vv1","cv1_margin","pv1_margin","v1_margin","cvs1_margin","pvs1_margin","pvs1_margin_incumb","pvs1_margin_nincumb","vs1_margin","pvs1_nincumb","cvs1_incumb","pvs1_incumb","win_pvs1","win_cvs1","contest_p1_nincumb","contest_p1","contest_c1","comptop2_c1","comptop2_p1","comptop1_c1","comptop1_p1","to2","pev2","vot2","vv2","cv2_margin","pv2_margin","v2_margin","cvs2_margin","pvs2_margin","vs2_margin","pv1_pty_180","pv1_pty_583")%>% toupper()

  # Data index
  data_ix <- data.table::data.table(
    index = 1:3,
    id = c(pct_id,cst_id,hex_id),
    shp = c("swe_pct","swe_cst","swe_hex"),
    votez = c("pct_votez","cst_votez","hex_votez"),
    w = c("pct_lw","cst_lw","hex_lw")
    )

  ## Compare CoS methods
  extnz <- TRUE

  # Common variables
  common_varz <- intersect(names(pct_votez), names(cst_votez)) %>% .[!grepl("^CST",.)] %>% data.table(varz = .) %>% .[,INTENSIVE:=grepl("^TO|VS|COMP|CONTEST",varz)] %>% .[,varz2 := paste0(varz,"_HAT")] %>% .[varz%in%{pct_votez %>% .[,apply(.SD,2,function(x){mean(is.na(x))!=TRUE }) %>% which() %>% names()]}] %>% .[varz%in%num_cols] %>% .[varz %in% c("COMPTOP2_P1","TO1","PV1_MARGIN","VV1","PEV1","VOT1")]

  # Transformation functions
  if(extnz){funzo <- function(x){base::sum(x,na.rm=TRUE)}; funzo_t <- base::sum; funzo_pop <- function(x,w){sum(x*w,na.rm=T)};common_varz <- common_varz %>% dplyr::bind_rows(data.table::data.table(varz=c("TO1","COMPTOP2_P1",grep("^PV1_PTY_",names(pct_votez),value=TRUE)),INTENSIVE=FALSE,varz2=c("TO1_HAT","COMPTOP2_P1_HAT",grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")))) %>% .[!duplicated(varz)]; extvar<- common_varz[,varz]}else{funzo <- function(x){base::mean(x,na.rm=TRUE)}; funzo_t <- base::mean; funzo_pop <- function(x,w){weighted.mean(x,w,na.rm=T)}; common_varz <- common_varz %>% dplyr::bind_rows(data.table::data.table(varz=c(grep("^PV1_PTY_",names(pct_votez),value=TRUE)),INTENSIVE=FALSE,varz2=c(paste0(grep("^PV1_PTY_",names(pct_votez),value=TRUE),"_HAT")))) %>% .[!duplicated(varz)];  extvar <- NULL}

  # Estimate grid cell level election results
  hex_votez <- swe_pct %>% sf::st_intersection(swe_hex) %>% data.table::as.data.table() %>% .[,AREA_INT := sf::st_area(geometry) %>% as.numeric()] %>% .[order(get(pct_id)),] %>% .[order(AREA_INT) %>% rev(),.SD,by=pct_id] %>% .[!duplicated(get(pct_id))] %>% .[,.(get(pct_id),get(hex_id))] %>% data.table::setnames(c(pct_id,hex_id)) %>% merge(pct_votez,by=pct_id,all.x=TRUE) %>% .[,lapply(.SD,function(x){unique(x[!is.na(x)]) %>% as.numeric()}),by=c(hex_id,pct_id),.SDcols=common_varz[,varz]] %>% .[,lapply(.SD,funzo),by=hex_id,.SDcols=common_varz[,varz]] %>% .[,PV1_MARGIN := apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),.SDcols=grep("^PV1_PTY_",names(.),value=T)] %>% .[,COMPTOP2_P1 := (VV1-PV1_MARGIN)/VV1] %>% .[,TO1 := VOT1/PEV1*100 ] %>% .[is.infinite(TO1), TO1:= NA_real_]
  rm(common_varz)

  # Loop over transformations
  ix_mat <- lapply(1:nrow(data_ix),function(ix0){

    if((nrow(data_ix)-ix0)>0){
      jx0_mat <- lapply((ix0+1):nrow(data_ix),function(jx0){

        # Fetch data
        id_1 <- data_ix[ix0,id]
        id_2 <- data_ix[jx0,id]
        map_1 <- data_ix[ix0,get(shp)] %>% sf::st_as_sf() %>% sf::st_transform(4326)
        map_2 <- data_ix[jx0,get(shp)] %>% sf::st_as_sf() %>% sf::st_transform(4326)
        votez_1 <- data_ix[ix0,get(votez)] %>% data.table::copy() 
        votez_2 <- data_ix[jx0,get(votez)] %>% data.table::copy() 
        lw_1 <- data_ix[ix0,get(w)]
        lw_2 <- data_ix[jx0,get(w)]

        print("0_1 SUCCESS")

        lat_mat_0 <- readRDS(paste0("data/r_output/mat_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),".RDS"))

        # Transform in both directions
        lat_mat <- lapply(1:2,function(n0){
          
          if(n0==1){
            print(paste0("Processing... source = ",ix0%>%c("(a)","(b)","(c)")[.]," dest = ",jx0%>%c("(a)","(b)","(c)")[.]))
          }
          if(n0==2){
            print(paste0("Processing... source = ",jx0%>%c("(a)","(b)","(c)")[.]," dest = ",ix0%>%c("(a)","(b)","(c)")[.]))
          }

          suppressMessages({
            suppressWarnings({

              # Create data objects
              source_id <- paste0("id_",n0) %>% get()
              dest_id <- paste0("id_",c(1:2)[!c(1:2)%in%n0]) %>% get()
              map_source <- paste0("map_",n0) %>% get() 
              map_dest <- paste0("map_",c(1:2)[!c(1:2)%in%n0]) %>% get() 
              votez_source <- paste0("votez_",n0) %>% get()  %>% .[,eval(dest_id):=NULL]
              votez_dest <- paste0("votez_",c(1:2)[!c(1:2)%in%n0]) %>% get()  %>% .[,eval(source_id):=NULL]
              lw_source <- paste0("lw_",n0) %>% get() 
              lw_dest <- paste0("lw_",c(1:2)[!c(1:2)%in%n0]) %>% get() 
              print("0_2 SUCCESS")
        
              # Nesting
              nest_coef_ <- SUNGEO::nesting(map_source,map_dest)
              print("0_3 SUCCESS")

              # Extract variables
              common_varz <- intersect(names(votez_source)[votez_source %>% .[,apply(.,2,function(x){mean(is.na(x))!=1})]], names(votez_dest)[votez_dest %>% .[,apply(.,2,function(x){mean(is.na(x))!=1})]]) %>% .[!grepl(paste0(cst_id,"|",pct_id,"|geometry|CST_NAME|PTY$|PTY_N$|VS\\d{1}_"),.)] %>% data.table(varz = .) %>% .[,INTENSIVE :=!(extnz==TRUE)] %>% .[,varz2 := paste0(varz,"_HAT")] %>% .[grepl("TO1|COMPTOP",varz),INTENSIVE:=TRUE]
              ext_varz <- intersect(names(votez_source), names(votez_dest)) %>% .[!grepl(paste0(source_id,"|",dest_id,"|geometry|CST_NAME|PTY$|PTY_N$|VS\\d{1}_"),.)] %>% data.table(varz = .) %>% .[,INTENSIVE :=!(extnz==TRUE)] %>% .[,varz2 := paste0(varz,"_HAT")]  %>% .[INTENSIVE==FALSE,.(varz,varz2)] %>% .[varz!="TO1"]
              if(nrow(ext_varz)==0){extvar <- extvar_w <- extvar_now <- NULL; funzo_pop <- function(x,w){weighted.mean(x,w,na.rm=T)}}else{extvar <- ext_varz[,varz]; extvar_now <- ext_varz[!grepl("_W",varz),varz ]; extvar_w <- ext_varz[grepl("_W",varz),varz ]; funzo_pop <- function(x,w){sum(x*w,na.rm=T)}}
              common_varz_ <- common_varz[varz%in%names(votez_dest)[votez_dest %>% .[,apply(.,2,function(x){mean(is.na(x))!=1})]]]
              if(common_varz[INTENSIVE==FALSE,length(varz)>0]){pycnoz <- common_varz[INTENSIVE==FALSE,varz]}else(pycnoz <- NULL)
              print("0_4 SUCCESS")

              # Create empty matrix for results
              lat_error_0 <- lapply(common_varz[,.I],function(i){
                  data.table::data.table(
                    source = n0,
                    method = NA_character_,
                    n_macro_1 = nrow(votez_source),
                    n_macro_2 = nrow(votez_dest),
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
                    varz = common_varz[i,varz],
                    extensive = common_varz[i,!INTENSIVE],
                    diff = NA,
                    abs_diff = NA,
                    prop_diff = NA,
                    std_diff = NA,
                    rmse = NA,
                    nrmse = NA,
                    nrmse_m = NA,
                    moran_p = NA,
                    beta_hat = NA,
                    se_hat = NA,
                    beta = NA,
                    se = NA,
                    spcor = NA,
                    missing = NA
                    )
                }) %>% dplyr::bind_rows()

              # Method 1: match by largest areal overlap 
              {
                lat_error_1 <- lat_error_0 %>% data.table::copy() %>% .[,method := "largest_overlap"]
                lat_error_1 <- lat_mat_0 %>% data.table::copy() %>% .[method == "largest_overlap" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_1 <- map_source %>% sf::st_intersection(map_dest) %>% data.table::as.data.table() %>% .[,AREA_INT := sf::st_area(geometry) %>% as.numeric()] %>% .[order(get(source_id)),] %>% .[order(AREA_INT) %>% rev(),.SD,by=source_id] %>% .[!duplicated(get(source_id))] %>% .[,.(get(source_id),get(dest_id))] %>% data.table::setnames(c(source_id,dest_id)) %>% merge(votez_source,by=source_id) %>% .[,lapply(.SD,function(x){unique(x[!is.na(x)]) %>% as.numeric()}),by=c(dest_id,source_id),.SDcols=common_varz[,varz]] %>% .[,lapply(.SD,funzo),by=dest_id,.SDcols=common_varz[,varz]] %>% data.table::setnames(common_varz[,varz],common_varz[,varz2],skip_absent=TRUE) %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% 
                  .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_1,file=paste0("data/r_output/lat_1_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_1 <- diag_tab(source_=n0, method_="largest_overlap", lat_out_=lat_1,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("1 SUCCESS: simple overlay (poly) complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); #print(format(t2-t1,units="min"))
                lat_error_1[,proc_time := format(t2-t1,units="min")]
              }

              # Method 2: simple overlay (using centroids)
              {
                lat_error_2 <- lat_error_0 %>% data.table::copy() %>% .[,method := "simple_overlay"]
                lat_error_2 <- lat_mat_0 %>% data.table::copy() %>% .[method == "simple_overlay" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_2 <- map_source %>% merge(votez_source,by=source_id) %>% sf::st_centroid()  %>% data.table::as.data.table() %>% .[!duplicated(get(source_id))] %>% sf::st_as_sf() %>% SUNGEO::point2poly_simp(pointz=.,polyz=map_dest,varz=common_varz[,varz],funz=funzo) %>% data.table::as.data.table() %>% .[,.SD,.SDcols=c(dest_id,common_varz[,varz])] %>% data.table::setnames(common_varz[,varz],common_varz[,varz2],skip_absent=TRUE) %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_2,file=paste0("data/r_output/lat_2_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_2 <- diag_tab(source_=n0, method_="simple_overlay", lat_out_=lat_2,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("2 SUCCESS: simple overlay (point) complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); #print(format(t2-t1,units="min"))
                lat_error_2[,proc_time := format(t2-t1,units="min")]
              }

              # Method 3: areal interpolation from poly (SUNGEO)
              {
                lat_error_3 <- lat_error_0 %>% data.table::copy() %>% .[,method := "area_int_poly"]
                lat_error_3 <- lat_mat_0 %>% data.table::copy() %>% .[method == "area_int_poly" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_3 <- merge(map_source,votez_source %>% .[, (common_varz[,varz]) := lapply(.SD, as.numeric), .SDcols = common_varz[,varz]],by=source_id) %>% data.table::as.data.table() %>% .[!duplicated(get(source_id))] %>% sf::st_as_sf() %>% SUNGEO::poly2poly_ap(poly_from=.,poly_to=map_dest,poly_to_id=dest_id,varz=common_varz[,varz],pycno_varz=pycnoz,funz= funzo_pop) %>% data.table::as.data.table() %>% .[,.SD,.SDcols=c(dest_id,common_varz[,varz %>% paste0("_aw")])] %>% data.table::setnames(common_varz[,varz %>% paste0("_aw")],common_varz[,varz2],skip_absent=TRUE) %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_3,file=paste0("data/r_output/lat_3_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_3 <- diag_tab(source_=n0, method_="area_int_poly", lat_out_=lat_3,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("3 SUCCESS: areal interpolation (poly) complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); #print(format(t2-t1,units="min"))
                lat_error_3[,proc_time := format(t2-t1,units="min")]
              }
             

              # Method 4: areal interpolation from centroids (SUNGEO)
              {
                lat_error_4 <- lat_error_0 %>% data.table::copy() %>% .[,method := "area_int_point"]
                lat_error_4 <- lat_mat_0 %>% data.table::copy() %>% .[method == "area_int_point" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_4 <- merge(map_source,votez_source %>% .[, (common_varz[,varz]) := lapply(.SD, as.numeric), .SDcols = common_varz[,varz]],by=source_id) %>% sf::st_centroid()  %>% data.table::as.data.table() %>% .[!duplicated(get(source_id))] %>% sf::st_as_sf()  %>% SUNGEO::point2poly_tess(pointz=.,polyz=map_dest,poly_id=dest_id,varz=common_varz[,varz],pycno_varz=pycnoz,funz= funzo_pop) %>% data.table::as.data.table() %>% .[,.SD,.SDcols=c(dest_id,common_varz[,varz %>% paste0("_aw")])] %>% data.table::setnames(common_varz[,varz %>% paste0("_aw")],common_varz[,varz2],skip_absent=TRUE) %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_4,file=paste0("data/r_output/lat_4_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_4 <- diag_tab(source_=n0, method_="area_int_point", lat_out_=lat_4,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("4 SUCCESS: areal interpolation (point) complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); #print(format(t2-t1,units="min"))
                lat_error_4[,proc_time := format(t2-t1,units="min")]
              }

              # Method 5: pop-weighted interpolation
              {
                lat_error_5 <- lat_error_0 %>% data.table::copy() %>% .[,method := "pop_int_poly"]
                lat_error_5 <- lat_mat_0 %>% data.table::copy() %>% .[method == "pop_int_poly" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_5 <- merge(map_source,votez_source %>% .[, (common_varz[,varz]) := lapply(.SD, as.numeric), .SDcols = common_varz[,varz]],by=source_id) %>% data.table::as.data.table() %>% .[!duplicated(get(source_id))] %>% sf::st_as_sf() %>% sf::st_transform(sf::st_crs(r_pop)) %>% SUNGEO::poly2poly_ap(poly_from=.,poly_to=map_dest %>% sf::st_transform(sf::st_crs(r_pop)),poly_to_id=dest_id,methodz="pw",pop_raster = r_pop,varz=common_varz[,varz ],pycno_varz=pycnoz,funz=funzo_pop) %>% data.table::as.data.table() %>% .[,.SD,.SDcols=c(dest_id,common_varz[,varz %>% paste0("_pw")])] %>% data.table::setnames(common_varz[,varz %>% paste0("_pw")],common_varz[,varz2],skip_absent=TRUE) %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_5,file=paste0("data/r_output/lat_5_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_5 <- diag_tab(source_=n0, method_="pop_int_poly", lat_out_=lat_5,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("5 SUCCESS: pop-weighted interpolation (poly) complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); #print(format(t2-t1,units="min"))
                lat_error_5[,proc_time := format(t2-t1,units="min")]
              }

              # Method 6: pop-weighted interpolation (from centroids)
              {
                lat_error_6 <- lat_error_0 %>% data.table::copy() %>% .[,method := "pop_int_point"]
                lat_error_6 <- lat_mat_0 %>% data.table::copy() %>% .[method == "pop_int_point" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_6 <- merge(map_source,votez_source %>% .[, (common_varz[,varz]) := lapply(.SD, as.numeric), .SDcols = common_varz[,varz]],by=source_id) %>% sf::st_centroid() %>% data.table::as.data.table() %>% .[!duplicated(get(source_id))] %>% sf::st_as_sf() %>% sf::st_transform(sf::st_crs(r_pop)) %>% SUNGEO::point2poly_tess(pointz=.,polyz=map_dest %>% sf::st_transform(sf::st_crs(r_pop)),poly_id=dest_id,methodz="pw",pop_raster = r_pop,varz=common_varz[,varz ],pycno_varz=pycnoz,funz= funzo_pop) %>% data.table::as.data.table() %>% .[,.SD,.SDcols=c(dest_id,common_varz[,varz %>% paste0("_pw")])] %>% data.table::setnames(common_varz[,varz %>% paste0("_pw")],common_varz[,varz2],skip_absent=TRUE) %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE)  %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_6,file=paste0("data/r_output/lat_6_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_6 <- diag_tab(source_=n0, method_="pop_int_point", lat_out_=lat_6,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("6 SUCCESS: pop-weighted interpolation (point) complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); 
                lat_error_6[,proc_time := format(t2-t1,units="min")]
              }

              # Method 7: TPRS-Forest (no resid)
              {
                lat_error_7 <- lat_error_0 %>% data.table::copy() %>% .[,method := "tps_default"]
                lat_error_7 <- lat_mat_0 %>% data.table::copy() %>% .[method == "tps_default" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_7 <- lapply(common_varz[,varz],function(v0){
                     merge(map_source,votez_source %>% .[, (common_varz[,varz]) := lapply(.SD, as.numeric), .SDcols = common_varz[,varz]],by=source_id) %>% tprs_predict(input_poly=.,output_poly=map_dest,funz=funzo_t,resz=25000,varz=v0,pycno=extnz) %>% data.table::as.data.table()
                  }) %>% cbind_dupnames() %>% data.table::setnames(common_varz[,varz] %>% paste0(c("_mean","_l","_u") %>% rep(each=nrow(common_varz))),common_varz[,varz2] %>% paste0(c("","_l","_u") %>% rep(each=nrow(common_varz)))) %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_7,file=paste0("data/r_output/lat_7_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_7 <- diag_tab(source_=n0, method_="tps_default", lat_out_=lat_7,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("7 SUCCESS: TPRS Forest complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); 
                lat_error_7[,proc_time := format(t2-t1,units="min")]
              }


              # Method 8: TPRS (w/ resid)
              {
                lat_error_8 <- lat_error_0 %>% data.table::copy() %>% .[,method := "tps_wresid"]
                lat_error_8 <- lat_mat_0 %>% data.table::copy() %>% .[method == "tps_wresid" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_8 <- lapply(common_varz[,varz],function(v0){
                     merge(map_source,votez_source %>% .[, (common_varz[,varz]) := lapply(.SD, as.numeric), .SDcols = common_varz[,varz]],by=source_id) %>% tprs_predict(input_poly=.,output_poly=map_dest,funz=funzo_t,resz=25000,varz=v0,pycno=extnz,include_resid=TRUE) %>% data.table::as.data.table()
                  }) %>% cbind_dupnames() %>% data.table::setnames(common_varz[,varz] %>% paste0(c("_mean","_l","_u") %>% rep(each=nrow(common_varz))),common_varz[,varz2] %>% paste0(c("","_l","_u") %>% rep(each=nrow(common_varz)))) %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_8,file=paste0("data/r_output/lat_8_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_8 <- diag_tab(source_=n0, method_="tps_wresid", lat_out_=lat_8,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("8 SUCCESS: TPRS Forest (w/ resid) complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); 
                lat_error_8[,proc_time := format(t2-t1,units="min")]
              }


              # Method 9: TPRS AW
              {
                lat_error_9<- lat_error_0 %>% data.table::copy() %>% .[,method := "tprs_aw"]
                t1 <- Sys.time()
                lat_error_9 <- lat_mat_0 %>% data.table::copy() %>% .[method == "tprs_aw" & source == n0] 
                tryCatch({
                  lat_9 <- lapply(common_varz[,varz],function(v0){
                     merge(map_source,votez_source %>% .[, (common_varz[,varz]) := lapply(.SD, as.numeric), .SDcols = common_varz[,varz]],by=source_id) %>% TPRS_AW_2(poly_from=.,poly_to=map_dest,,poly_from_id=source_id,poly_to_id=dest_id,varz=v0,k=3,extensive=extnz) %>% data.table::as.data.table() %>% .[,.SD,.SDcols=c(dest_id,"Predict","Lower","Upper")] %>% data.table::setnames(c(dest_id,paste0(v0,"_pred"),paste0(v0,"_l"),paste0(v0,"_u")))
                  }) %>% cbind_dupnames() %>% data.table::setnames(common_varz[,varz] %>% paste0(c("_pred","_l","_u") %>% rep(each=nrow(common_varz))),common_varz[,varz2] %>% paste0(c("","_l","_u") %>% rep(each=nrow(common_varz)))) %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_9,file=paste0("data/r_output/lat_9_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_9 <- diag_tab(source_=n0, method_="tprs_aw", lat_out_=lat_9,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("9 SUCCESS: TPRS AW complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); 
                lat_error_9[,proc_time := format(t2-t1,units="min")]
              }


              # Method 10: ordinary kriging from centroids (SUNGEO)
              {
                lat_error_10 <- lat_error_0 %>% data.table::copy() %>% .[,method := "krige_ordinary"]
                lat_error_10 <- lat_mat_0 %>% data.table::copy() %>% .[method == "krige_ordinary" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_10 <- parallel::mclapply(seq_along(common_varz[,varz]),function(i0){
                    if(common_varz[i0,!INTENSIVE]){pycno_yvarz_<-common_varz[i0,varz]}else{pycno_yvarz_<-NULL}
                    merge(map_source,votez_source %>% .[, (common_varz[,varz]) := lapply(.SD, as.numeric), .SDcols = common_varz[,varz]],by=source_id) %>% sf::st_centroid() %>% data.table::as.data.table() %>% .[!duplicated(get(source_id))] %>% sf::st_as_sf() %>% SUNGEO::point2poly_krige(pointz=.,polyz=map_dest,yvarz=common_varz[i0,varz],funz=funzo_t,pycno_yvarz=pycno_yvarz_) %>% data.table::as.data.table() %>% data.table::setnames(common_varz[,varz] %>% paste0(c(".pred",".stdev") %>% rep(each=nrow(common_varz))),common_varz[,varz2] %>% paste0(c("","_sd") %>% rep(each=nrow(common_varz))),skip_absent=TRUE)
                  },mc.cores=parallel::detectCores()-1) %>% cbind_dupnames() %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_10,file=paste0("data/r_output/lat_10_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_10 <- diag_tab(source_=n0, method_="krige_ordinary", lat_out_=lat_10,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("10 SUCCESS: ordinary Kriging complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); 
                lat_error_10[,proc_time := format(t2-t1,units="min")]
              }

              # Method 11: universal kriging from centroids (SUNGEO)
              {
                lat_error_11 <- lat_error_0 %>% data.table::copy() %>% .[,method := "krige_universal"]
                lat_error_11 <- lat_mat_0 %>% data.table::copy() %>% .[method == "krige_universal" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_11 <- parallel::mclapply(seq_along(common_varz[,varz]),function(i0){
                    if(common_varz[i0,!INTENSIVE]){pycno_yvarz_<-common_varz[i0,varz]}else{pycno_yvarz_<-NULL}
                    merge(map_source,votez_source %>% .[, (common_varz[,varz]) := lapply(.SD, as.numeric), .SDcols = common_varz[,varz]],by=source_id) %>% sf::st_centroid() %>% data.table::as.data.table() %>% .[!duplicated(get(source_id))] %>% sf::st_as_sf() %>% SUNGEO::point2poly_krige(pointz=.,polyz=map_dest,yvarz=common_varz[i0,varz],rasterz=r_pop,funz=funzo_t,pycno_yvarz=pycno_yvarz_) %>% data.table::as.data.table() %>% data.table::setnames(common_varz[,varz] %>% paste0(c(".pred",".stdev") %>% rep(each=nrow(common_varz))),common_varz[,varz2] %>% paste0(c("","_sd") %>% rep(each=nrow(common_varz))),skip_absent=TRUE) 
                  },mc.cores=parallel::detectCores()-1) %>% cbind_dupnames() %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_11,file=paste0("data/r_output/lat_11_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_11 <- diag_tab(source_=n0, method_="krige_universal", lat_out_=lat_11,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("11 SUCCESS: universal Kriging complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); 
                lat_error_11[,proc_time := format(t2-t1,units="min")]
              }

              # Method 12: poly2poly_ras
              {
                lat_error_12 <- lat_error_0 %>% data.table::copy() %>% .[,method := "rasterization"]
                lat_error_12 <- lat_mat_0 %>% data.table::copy() %>% .[method == "rasterization" & source == n0] 
                t1 <- Sys.time()
                tryCatch({
                  lat_12 <- merge(map_source,votez_source %>% .[, (common_varz[,varz]) := lapply(.SD, as.numeric), .SDcols = common_varz[,varz]],by=source_id) %>% data.table::as.data.table() %>% .[!duplicated(get(source_id))] %>% sf::st_as_sf() %>% poly2poly_ras(input_poly=.,output_poly=map_dest,varz=common_varz[,varz],pycno=extnz) %>% data.table::as.data.table() %>% .[,.SD,.SDcols=c(dest_id,common_varz[,varz])] %>% data.table::setnames(common_varz[,varz],common_varz[,varz2],skip_absent=TRUE) %>% .[,PV1_MARGIN_HAT := ifelse(rep(extnz==TRUE,.N),apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),PV1_MARGIN_HAT),.SDcols=grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")] %>% .[,COMPTOP2_P1_HAT := ifelse(rep(extnz==TRUE,.N),(VV1_HAT-PV1_MARGIN_HAT)/VV1_HAT,COMPTOP2_P1_HAT)] %>% merge(votez_dest[,.SD,.SDcols=c(dest_id,common_varz[varz%in%names(votez_dest),varz])],by=dest_id,all.y=TRUE) %>% .[,paste0("Y_",common_varz[varz%in%names(votez_dest),varz]) := lapply(.SD,function(x){true_beta*x + rnorm(.N,sd=sd(x,na.rm=T))}),.SDcols=common_varz[varz%in%names(votez_dest),varz]] %>% .[order(order(as.data.frame(map_dest)[dest_id])),] %>% .[,TO1_HAT := VOT1_HAT/PEV1_HAT *100] %>% .[is.infinite(TO1_HAT), TO1_HAT:= NA_real_]
                  saveRDS(lat_12,file=paste0("data/r_output/lat_12_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
                  lat_error_12 <- diag_tab(source_=n0, method_="rasterization", lat_out_=lat_12,votez_source_ = votez_source, votez_dest_ = votez_dest, nest_coef_ = nest_coef_, varz_ = common_varz[,varz], varz2_ = common_varz[,varz2], extensive_ = common_varz_[,!INTENSIVE], varz_a = common_varz_[,varz], varz2_a = common_varz_[,varz2], lw_dest_ = lw_dest)
                  print("12 SUCCESS: rasterization complete")
                },error=function(e){if(verbose_warn == TRUE){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))}})
                  t2 <- Sys.time(); 
                lat_error_12[,proc_time := format(t2-t1,units="min")]
              }

              # Output
              lat_error_1 %>% rbind(lat_error_2) %>% rbind(lat_error_3) %>% rbind(lat_error_4) %>% rbind(lat_error_5) %>% rbind(lat_error_6) %>% rbind(lat_error_7) %>% rbind(lat_error_8) %>% rbind(lat_error_9) %>% rbind(lat_error_10) %>% rbind(lat_error_11) %>% rbind(lat_error_12)

            }) 
          }) 
        }) %>% data.table::rbindlist(fill=TRUE)

        # Save
        saveRDS(lat_mat,file=paste0("data/r_output/mat_swe_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),".RDS"))

      })
    }

  })

  print("FINISHED: Figure A5.4")

  # Close if statement
  }

},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})



##############################################################################
##############################################################################
## Figure A5.4
## Output from change-of-support operations (Sweden).
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Figure A5.4")

  # Clear workspace
  rm(list=ls())

  # Source functions
  source("code/functions.R")

  # Set parameters
  out_dir <- "results/"
  cntz <- "swe"
  extnz <- TRUE
  n0 <- 1

  # Set ID's
  cst_id <- "CST_CODE"
  pct_id <- "LKFV"
  hex_id <- "HEX05_CODE"

  # Names of numeric CLEA columns
  num_cols <- c("to1","pev1","vot1","vv1","cv1_margin","pv1_margin","v1_margin","cvs1_margin","pvs1_margin","pvs1_margin_incumb","pvs1_margin_nincumb","vs1_margin","pvs1_nincumb","cvs1_incumb","pvs1_incumb","win_pvs1","win_cvs1","contest_p1_nincumb","contest_p1","contest_c1","comptop2_c1","comptop2_p1","comptop1_c1","comptop1_p1","to2","pev2","vot2","vv2","cv2_margin","pv2_margin","v2_margin","cvs2_margin","pvs2_margin","vs2_margin","pv1_pty_180","pv1_pty_583")%>% toupper()

  # Data index
  data_ix <- data.table::data.table(
    index = 1:3,
    id = c(pct_id,cst_id,hex_id),
    shp = c("swe_pct","swe_cst","swe_hex"),
    votez = c("pct_votez","cst_votez","hex_votez"),
    w = c("pct_lw","cst_lw","hex_lw")
    )

  # Load population raster
  r_pop <- raster::raster("data/SWE/GPW_SWE_2015_POP.asc",crs=" +init=epsg:4326")
  
  # Load geometries & reproject 
  swe_pct <- sf::st_read("data/SWE/VAL_SWE_2010_PCT_wgs.geojson") %>% sf::st_transform(crs=sf::st_crs(r_pop)) %>% data.table::as.data.table() %>% .[,eval(pct_id):=get(pct_id)%>%as.character()] %>% sf::st_as_sf()
  swe_cst <- sf::st_read("data/SWE/GRED_SWE_2010_CST_wgs.geojson") %>% sf::st_transform(crs=sf::st_crs(r_pop))
  swe_hex <- sf::st_read("data/SWE/HEXGRID_SWE_2020_HEX05_wgs.geojson") %>% sf::st_transform(crs=sf::st_crs(r_pop))

  # Load elections data
  cst_votez <- readRDS("data/SWE/CLEA_SWE_2010_CST.RDS")
  pct_votez <- readRDS("data/SWE/CLEA_SWE_2010_PCT.RDS") %>% .[,eval(pct_id):=get(pct_id)%>%as.character()]

  # Common variables
  common_varz <- intersect(names(pct_votez), names(cst_votez)) %>% .[!grepl("^CST",.)] %>% data.table(varz = .) %>% .[,INTENSIVE:=grepl("^TO|VS|COMP|CONTEST",varz)] %>% .[,varz2 := paste0(varz,"_HAT")] %>% .[varz%in%{pct_votez %>% .[,apply(.SD,2,function(x){mean(is.na(x))!=TRUE }) %>% which() %>% names()]}] %>% .[varz%in%num_cols] %>% .[varz %in% c("COMPTOP2_P1","TO1","PV1_MARGIN","VV1","PEV1","VOT1")]

  # Transformation functions
  if(extnz){funzo <- function(x){base::sum(x,na.rm=TRUE)}; funzo_t <- base::sum; funzo_pop <- function(x,w){sum(x*w,na.rm=T)};common_varz <- common_varz %>% dplyr::bind_rows(data.table::data.table(varz=c("TO1","COMPTOP2_P1",grep("^PV1_PTY_",names(pct_votez),value=TRUE)),INTENSIVE=FALSE,varz2=c("TO1_HAT","COMPTOP2_P1_HAT",grep("^PV1_PTY_",names(pct_votez),value=TRUE) %>% paste0("_HAT")))) %>% .[!duplicated(varz)]; extvar<- common_varz[,varz]}else{funzo <- function(x){base::mean(x,na.rm=TRUE)}; funzo_t <- base::mean; funzo_pop <- function(x,w){weighted.mean(x,w,na.rm=T)}; common_varz <- common_varz %>% dplyr::bind_rows(data.table::data.table(varz=c(grep("^PV1_PTY_",names(pct_votez),value=TRUE)),INTENSIVE=FALSE,varz2=c(paste0(grep("^PV1_PTY_",names(pct_votez),value=TRUE),"_HAT")))) %>% .[!duplicated(varz)];  extvar <- NULL}

  # Estimate grid cell level election results
  hex_votez <- swe_pct %>% sf::st_intersection(swe_hex) %>% data.table::as.data.table() %>% .[,AREA_INT := sf::st_area(geometry) %>% as.numeric()] %>% .[order(get(pct_id)),] %>% .[order(AREA_INT) %>% rev(),.SD,by=pct_id] %>% .[!duplicated(get(pct_id))] %>% .[,.(get(pct_id),get(hex_id))] %>% data.table::setnames(c(pct_id,hex_id)) %>% merge(pct_votez,by=pct_id,all.x=TRUE) %>% .[,lapply(.SD,function(x){unique(x[!is.na(x)]) %>% as.numeric()}),by=c(hex_id,pct_id),.SDcols=common_varz[,varz]] %>% .[,lapply(.SD,funzo),by=hex_id,.SDcols=common_varz[,varz]] %>% .[,PV1_MARGIN := apply(.SD,1,function(x){sort(x,decreasing=TRUE) %>% diff() %>% abs() %>% .[1]}),.SDcols=grep("^PV1_PTY_",names(.),value=T)] %>% .[,COMPTOP2_P1 := (VV1-PV1_MARGIN)/VV1] %>% .[,TO1 := VOT1/PEV1*100 ] %>% .[is.infinite(TO1), TO1:= NA_real_]

  # plot(swe_pct["geometry"])

  #####################################
  # Plots for Figure A5.4
  #####################################

  w_ <- 3.5; h_ <- w_*1.618
  # ix0 <- 1 
  for(ix0 in 1:2){
    jx0 <- ix0 + 1
    # Diagnostics
    lat_mat <- readRDS(paste0("data/r_output/mat_",cntz,"_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),".RDS"))
    map_ <- data_ix[,get(votez[jx0])] %>% .[,.(get(data_ix[,id[jx0]]),COMPTOP2_P1)] %>% merge(data_ix[,get(shp[jx0])],.,by.x=data_ix[,id[jx0]],by.y="V1",all.x=T) %>% .["COMPTOP2_P1"]
    png(paste0(out_dir,"figA5_4",ifelse(data_ix[c(ix0,jx0),paste0(index%>%c("a","b","c")[.],collapse="")]=="ab","a","b"),"_xtrue.png"),width=w_,height=h_,res=300,units="in")
    plot(map_,pal=grey(seq(.9,0.2,length.out=101)),breaks=0:101/100,main="",key.length=1,key.pos=4,setParUsrBB=F)
    dev.off()
    iz0 <- paste0("lat_",1:12)
    # i0 <- iz0[1]
    for(i0 in iz0){
      tryCatch({
        lat_ <- readRDS(paste0("data/r_output/",i0,"_",cntz,"_i",ix0,"_j",jx0,ifelse(extnz,"_ext","_int"),"_n0",n0,".RDS"))
        map_ <- lat_ %>% .[,.(get(data_ix[,id[jx0]]),COMPTOP2_P1_HAT)] %>% merge(data_ix[,get(shp[jx0])],.,by.x=data_ix[,id[jx0]],by.y="V1",all.x=T) %>% .["COMPTOP2_P1_HAT"]
        png(paste0(out_dir,"figA5_4",ifelse(data_ix[c(ix0,jx0),paste0(index%>%c("a","b","c")[.],collapse="")]=="ab","a","b"),"_",i0%>%gsub("lat_","",.),"_",lat_mat[,method%>%unique()][i0%>%gsub("lat_","",.)%>%as.numeric()],".png"),width=w_,height=h_,res=300,units="in")
        plot(map_,pal=grey(seq(.9,0.2,length.out=101)),breaks=0:101/100,main="",key.length=1,key.pos=4,setParUsrBB=F)
        dev.off()
      },error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})
    }        
  }

  print("FINISHED: Figure A5.4")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})



##############################################################################
##############################################################################
## Figure A5.5
## Relative nesting, scale, and transformations of election data (Sweden)
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Table A5.5")

  # Clear workspace
  rm(list=ls())

  # Source functions
  source("code/functions.R")

  # Set parameters
  out_dir <- "results/"
  cntz <- "swe"
  extnz <- TRUE

  # Labels
  mvarmat <- data.table::data.table(mvar=c("id_match","largest_overlap","simple_overlay","area_int_poly","area_int_point","pop_int_poly","pop_int_point","tps_default","tps_wresid","tps_mh","tps_mhwresid","tprs_aw","krige_ordinary","krige_universal","rasterization"),mlab=c("ID match","Overlay (polygons)","Overlay (centroids)","Area Weights (polygons)","Area Weights (centroids)","Pop Weights (polygons)","Pop Weights (centroids)","TPRS-Forest","TPRS-Forest (w/ resid)","TPRS-Forest (MH)","TPRS-Forest (MH, w/ resid)","TPRS-Area Weights","Ordinary Kriging","Universal Kriging","Rasterization")) %>% .[,mlab := factor(mlab,levels=rev(mlab))]
  labmat <- data.table::data.table(
    var=c("rs","rs_sym","rn","rn_sym","abs(diff)","log(rmse)","rmse","log(rmse_mod)","log(rmse_mod_w)","rmse_mod","rmse_mod_w","log(nrmse)","nrmse","log(nrmse_m)","nrmse_m","spcor_w","bias_w","abs(bias_w)","spcor","bias","proc_time_sec","diff","n_ratio_12","n_ratio_21","p_intact","full_nest","rn_full","rs_full","rn_nn","rs_nn","mean_splits","moran_p_source","moran_p_w","predint_sd025","predint_sd05","predint_sd1","predint_sd2","predint_95ci","rn_alt","rs_alt","ro","diverge"),
    lab=c("Relative scale (RS)","Relative scale, symmetric (RS-sym)","Relative nesting (RN)","Relative nesting, symmetric (RN-sym)","Absolute residuals","log(RMSE)","RMSE","log(RMSE Y)","log(RMSE Y)","RMSE Y","RMSE Y","log(NRMSE)","NRMSE","log(NRMSE)","NRMSE","Spearman's correlation","OLS estimation bias","OLS estimation bias (absolute)","Spearman's correlation","OLS estimation bias","Processing time (sec.)","Residuals","N source units / N destination units","N destination units / N source units","Proportion intact (PI)","Proportion fully nested (PFN)","RN non-nested (RN-nn)","RS non-nested (RS-nn)","RN non-nested (RN-nn)","RS non-nested (RS-nn)","Average number of splits (ANS)","Source Moran's I p-value","Moran's I p-value, residuals","Coverage 1/4 SD","Coverage 1/2 SD","Coverage 1 SD","Coverage 2 SD","Coverage 95% CI","Relative nesting, alternative (RN-alt)","Relative scale, alternative (RS-alt)","Relative overlap (RO)","Divergence (RS-RN)"))

  # List of files
  filez <- list.files("data/r_output/",pattern=ifelse(extnz,"_ext\\.RDS","_int\\.RDS"),full.names=TRUE) %>% grep(paste0(cntz,"_i"),.,value=T)
  # Combine CoS output into one file
  sims_mat_ <- lapply(filez,function(f0){readRDS(f0) %>% dplyr::bind_rows()}) %>% data.table::rbindlist(fill=TRUE) %>% .[,bias := beta_hat-beta] %>% .[,method:=factor(method,levels=mvarmat[,mvar])]  %>% .[,proc_time_sec := NA_real_] %>% .[grep("secs",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric()] %>% .[grep("mins",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*60})] %>% .[grep("hours",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*3600})] %>% .[,n_ratio_21 := n_macro_2/n_macro_1] %>% .[,n_ratio_12 := n_macro_1/n_macro_2] %>% .[varz=="COMPTOP2_P1",]  %>% .[,c("bias","rmse","nrmse") := lapply(.SD,function(x){x[x<quantile(x,.25,na.rm=TRUE)-1.5*IQR(x,na.rm=TRUE)|x>quantile(x,.75,na.rm=TRUE)+1.5*IQR(x,na.rm=TRUE)] <- NA; return(x)}),.SDcols=c("bias","rmse","nrmse")] %>% .[,rmse := rmse + 1] %>% .[,nrmse := nrmse + 1]

  # Extract RN/RS scores for all CoS's
  if(cntz=="usa"){
    nlabmat <- data.table::data.table(n=c(2572,14,84),lab=c("a","b","c"))
  } else if(cntz=="swe"){
    nlabmat <- data.table::data.table(n=c(6063,29,320),lab=c("a","b","c"))
  }
  N_mat <- sims_mat_ %>% .[,poly_1 := n_macro_1 %>% match(nlabmat[,n]) %>% nlabmat[.,lab]] %>% .[,poly_2 := n_macro_2 %>% match(nlabmat[,n]) %>% nlabmat[.,lab]] %>% .[,.SD,.SDcols=c("poly_1","poly_2","rs","rn")] %>% unique() %>% data.table::setkey(poly_1,poly_2)


  #####################################
  # Figure A5.5 Legend
  #####################################

  h_ <- 1.5; w_ <- h_*(1.618)*3.5
  mz <- mvarmat[mvar%in%sims_mat_[,method],mvar] 
  png(paste0(out_dir,"figA5_5_legend_methods.png"), width = w_, height = h_, units = "in",res=300)
  par(mar=rep(0,4))
  plot.new()
  legend("center",bty="n",pch=mz%>%match(mvarmat[,mvar]),legend=mz%>%match(mvarmat[,mvar])%>%mvarmat[.,mlab],ncol=4,cex=1,title="Change-of-support method")
  dev.off()
  png(paste0(out_dir,"figA5_5_legend_units.png"), width = w_/4, height = h_, units = "in",res=300)
  par(mar=rep(0,4))
  plot.new()
  legend("center",bty="n",pch=c("a","b","c"),legend=c("Electoral precincts","Constituencies","Grid cells"),ncol=1,cex=1,title=paste0("Units",ifelse(cntz=="usa"," (Georgia)"," (Sweden)")))
  dev.off()


  #####################################
  # Rest of Figure A5.5
  #####################################

  # Create copies of dt
  X <- data.table::copy(sims_mat_)
  N_mat_ <- N_mat %>% data.table::copy()

  for(xvar in c("rn","rs")){
    
    #####################################
    # Figure A5.5 x axis labels
    #####################################

    # Plot dimensions
    xz <- seq(0,1,by=.01)
    xlimz <- range(xz)
    h_ <- 3; w_ <- h_*(1.618)
    m_ <- N_mat_ %>% .[order(get(xvar))] %>% .[,ypos := seq(-.1,-1,length.out=.N)]

    # Render
    png(paste0(out_dir,"figA5_5_x_",xvar,".png"), width = w_, height = w_/(1.618*2), units = "in",res=300)
    par(mar=c(0.5,4,2,0.5),xpd="n")
    plot(x=0,y=0,col=NA,ylim=c(-1,0),xlim=xlimz,xlab="",ylab="",bty="n",yaxt="n",xaxt="n")
    axis(3)
    segments(x0=m_[,get(xvar)],x1=m_[,get(xvar)],y0=0,y1=m_[,ypos+.5/.N],lty=3,lwd=.5)
    text(x=m_[,get(xvar)],y=m_[,ypos],labels=expression(intersect()))
    text(x=m_[,get(xvar)],y=m_[,ypos],labels=paste0(m_[,poly_1],"      ",m_[,poly_2]))
    dev.off()

    #####################################
    # Figure A5.5 main
    #####################################

    for(yvar in c("bias","log(rmse)","spcor")){
      print(paste0(yvar," ",xvar))

      # Plot dimensions
      h_ <- 3; w_ <- h_*(1.618)

      # Model specification and estimation
      formz <- paste0(yvar," ~ ",xvar) %>% as.formula() %>% Formula::as.Formula()
      gmod_ <- mgcv::gam(formz,data=X)
      summary(gmod_)
      
      # Model predictions
      predz <- mgcv::predict.gam(gmod_,newdata=data.table::data.table(X=xz)%>%data.table::setnames("X",xvar),se.fit=TRUE,type="response")
      xlimz <- range(xz); ylimz <- range(predz$fit-1.96*predz$se.fit,predz$fit+1.96*predz$se.fit); lylimz <- range(exp(predz$fit-1.96*predz$se.fit) %>% replace(.,is.infinite(.), min(.[!is.infinite(.)])),exp(predz$fit+1.96*predz$se.fit) %>% replace(.,is.infinite(.), max(.[!is.infinite(.)])))
      if(grepl("log\\(",yvar)){
        ylimz <- X[,get(yvar %>% gsub("log\\(|\\)","",.)) %>% log() %>% quantile(c(0,.97),na.rm=TRUE)]; 
        # lylimz <- X[,(get(yvar %>% gsub("log\\(|\\)","",.)) %>% quantile(c(0,.97)))-c_] 
        lylimz <- c(0,min(1,X[,get(yvar %>% gsub("log\\(|\\)","",.)) %>% log()%>%max(na.rm=TRUE)]))
      }

      # Render on Natural scale
      if(!grepl("log",yvar)){
        png(paste0(out_dir,"figA5_5",ifelse(xvar%in%"rs","b","a"),"_",gsub("\\(|\\)","",yvar),".png"), width = w_, height = h_, units = "in",res=300)
        par(mar=c(4,4,0,0.5))
        plot(x=0,y=0,col=NA,ylim=ylimz,xlim=xlimz,xlab=xvar %>% match(labmat[,var]) %>% labmat[.,lab],ylab=yvar %>% match(labmat[,var]) %>% labmat[.,lab],bty="o",yaxt="n")
        axis(2,las=2)
        if(grepl("^bias|^diff",yvar)){segments(x0 = xlimz[1],x1 = xlimz[2],y0 = 0,y1 = 0,lty=3)}
        polygon(x=c(xz,rev(xz)),y=c(predz$fit-1.96*predz$se.fit,rev(predz$fit+1.96*predz$se.fit)),border=NA,col=rgb(.9,.9,.9,.5))
        lines(x=xz,y=predz$fit,lwd=2,lty=1)
        if(grepl("log",yvar)){
          points(x=X[,get(xvar)],y=X[,get(yvar %>% gsub("log\\(|\\)","",.)) %>% log()],pch=X[,method %>% as.numeric()],col=rgb(0,0,0,.5))
        } else if(grepl("abs",yvar)){
          points(x=X[,get(xvar)],y=X[,get(yvar %>% gsub("abs\\(|\\)","",.)) %>% abs()],pch=X[,method %>% as.numeric()],col=rgb(0,0,0,.5))
        } else {
          points(x=X[,get(xvar)],y=X[,get(yvar)],pch=X[,method %>% as.numeric()],col=rgb(0,0,0,.5))
        }
        dev.off()
      }

      # Render with exponentiated y-axis
      if(grepl("log",yvar)){
        png(paste0(out_dir,"figA5_5",ifelse(xvar%in%"rs","b","a"),"_",gsub("log\\(|\\)","",yvar),".png"), width = w_, height = h_, units = "in",res=300)
        par(mar=c(4,4,0,0.5))
        plot(x=0,y=0,col=NA,ylim=lylimz,xlim=xlimz,xlab=xvar %>% match(labmat[,var]) %>% labmat[.,lab],ylab=yvar %>% match(labmat[,var]) %>% labmat[.,lab] %>% gsub("log\\(|\\)","",.),bty="o",yaxt="n")
        axis(2,las=2)
        polygon(x=c(xz,rev(xz)),y=c(exp(predz$fit-1.96*predz$se.fit) %>% replace(.,is.infinite(.), NA),rev(exp(predz$fit+1.96*predz$se.fit) %>% replace(.,is.infinite(.), NA)))-1,border=NA,col=rgb(.9,.9,.9,.5))
        lines(x=xz,y=(exp(predz$fit)%>% replace(.,is.infinite(.), NA))-1,lwd=2,lty=1)
        points(x=X[,get(xvar)],y=X[,get(yvar %>% gsub("log\\(|\\)","",.))-1],pch=X[,method %>% as.numeric()],col=rgb(0,0,0,.5))
        dev.off()
      }

    }
  }

  print("FINISHED: Table A5.5")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})


##############################################################################
##############################################################################
## Table A6.7
## Transformation quality when interpolating component intensive variables
## directly vs. reconstructing them from extensive components 
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Table A6.7")

  # Clear workspace
  rm(list=ls())

  # Source functions
  source("code/functions.R")
  out_dir <- "results/"
  cntz <- "usa"

  # Labels
  mvarmat <- data.table::data.table(mvar=c("id_match","largest_overlap","simple_overlay","area_int_poly","area_int_sf","area_int_point","pop_int_poly","pop_int_point","tps_default","tps_wresid","tps_mh","tps_mhwresid","tprs_aw","krige_ordinary","krige_universal","rasterization","mean","meta"),mlab=c("ID match","Overlay (polygons)","Overlay (centroids)","Area Weights (polygons)","Area Weights (polygons) (sf)","Area Weights (centroids)","Pop Weights (polygons)","Pop Weights (centroids)","TPRS-Forest","TPRS-Forest (w/ resid)","TPRS-Forest (MH)","TPRS-Forest (MH, w/ resid)","TPRS-Area Weights","Ordinary Kriging","Universal Kriging","Rasterization","Mean","Meta")) %>% .[,mlab := factor(mlab,levels=rev(mlab))]
  # Variable labels
  labmat <- data.table::data.table(
    var=c("rs","rs_sym","rn","rn_sym","abs(diff)","log(rmse)","rmse","log(rmse_mod)","log(rmse_mod_w)","rmse_mod","rmse_mod_w","log(nrmse)","nrmse","log(nrmse_m)","nrmse_m","spcor_w","bias_w","abs(bias_w)","spcor","bias","proc_time_sec","diff","n_ratio_12","n_ratio_21","p_intact","full_nest","rn_full","rs_full","rn_nn","rs_nn","mean_splits","moran_p_source","moran_p_w","predint_sd025","predint_sd05","predint_sd1","predint_sd2","predint_95ci","rn_alt","rs_alt","ro","diverge"),
    lab=c("Relative scale (RS)","Relative scale, symmetric (RS-sym)","Relative nesting (RN)","Relative nesting, symmetric (RN-sym)","Absolute residuals","log(RMSE)","RMSE","log(RMSE Y)","log(RMSE Y)","RMSE Y","RMSE Y","log(NRMSE)","NRMSE","log(NRMSE)","NRMSE","Spearman's correlation","OLS estimation bias","OLS estimation bias (absolute)","Spearman's correlation","OLS estimation bias","Processing time (sec.)","Residuals","N source units / N destination units","N destination units / N source units","Proportion intact (PI)","Proportion fully nested (PFN)","RN non-nested (RN-nn)","RS non-nested (RS-nn)","RN non-nested (RN-nn)","RS non-nested (RS-nn)","Average number of splits (ANS)","Source Moran's I p-value","Moran's I p-value, residuals","Coverage 1/4 SD","Coverage 1/2 SD","Coverage 1 SD","Coverage 2 SD","Coverage 95% CI","Relative nesting, alternative (RN-alt)","Relative scale, alternative (RS-alt)","Relative overlap (RO)","Divergence (RS-RN)"))
  if(cntz=="usa"){
    polyz <- data.table::data.table(n=c(2572,14,84),poly=c("a. Precincts","b. Constituencies","c. Grid cells"))
  } else if(cntz=="swe"){
    polyz <- data.table::data.table(n=c(6063,29,320),poly=c("a. Precincts","b. Constituencies","c. Grid cells"))
  }

  # Combine extensive and intensive
  # extnz <- TRUE
  X <- lapply(c(TRUE,FALSE),function(extnz){#print(extnz)
    filez <- list.files("data/r_output/",pattern=ifelse(extnz,"_ext","_int"),full.names=TRUE) %>% grep(paste0(cntz,"_i"),.,value=TRUE) 
    sims_mat_ <- lapply(filez,function(f0){readRDS(f0) %>% data.table::copy() %>% .[,extensive := extnz] %>% .[,geometry := NULL] %>% dplyr::bind_rows()}) %>% data.table::rbindlist(fill=TRUE) %>% .[,bias := beta_hat-beta] %>% .[,method:=factor(method,levels=mvarmat[,mvar])]  %>% .[,proc_time_sec := NA_real_] %>% .[grep("secs",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric()] %>% .[grep("mins",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*60})] %>% .[grep("hours",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*3600})] %>% .[,n_ratio_21 := n_macro_2/n_macro_1] %>% .[,n_ratio_12 := n_macro_1/n_macro_2] %>% .[method!="area_int_sf"] %>% .[,rmse := rmse + 1] %>% .[varz%in%"COMPTOP2_P1"]
    sims_mat_
  }) %>% data.table::rbindlist(fill=TRUE)
X[,nrmse,by=extensive]

  # Create table
varz_ <- "rmse"
  extint <- lapply(c("rmse","spcor","bias"),function(varz_){print(varz_)
    extint_ <- X[extensive==FALSE,mean(get(varz_)),by=c("n_macro_1","n_macro_2")] %>% merge(X[extensive==TRUE,mean(get(varz_)),by=c("n_macro_1","n_macro_2")],by=c("n_macro_1","n_macro_2")) %>% merge(X[,lapply(.SD,mean),.SDcols=c("rs","rn"),by=c("n_macro_1","n_macro_2")],by=c("n_macro_1","n_macro_2")) %>% data.table::setnames(c("Source","Destination","Direct","Indirect","RS","RN")) %>% .[,Diagnostic:=varz_%>%match(labmat[,var])%>%labmat[.,lab]] %>% .[,Source := Source %>% match(polyz[,n]) %>% polyz[.,poly]] %>% .[,Destination := Destination %>% match(polyz[,n]) %>% polyz[.,poly]] %>% dplyr::select("Diagnostic","Source","Destination","RS","RN",dplyr::everything()) %>% .[order(Source,Destination)] %>% .[duplicated(Diagnostic,fromLast=FALSE),Diagnostic:=""]
    extint_
  }) %>% data.table::rbindlist()

  # Save to file
  extint%>%xtable::xtable(caption=paste0("Transformation quality when interpolating component intensive variables directly (``Direct'') vs. reconstructing them from extensive components (``Indirect'')."),label=paste0("tab:extint_2"))%>%xtable::print.xtable(booktabs=TRUE,hline.after=c(-1,0,6,12,nrow(.)),include.rownames=FALSE,table.placement = "H",file=paste0(out_dir,"tabA6_7.tex"),size="footnotesize",sanitize.text.function = identity)

  print("FINISHED: Table A6.7")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})


##############################################################################
##############################################################################
## Figure A7.6
## Two examples of Gaussian Random Fields used in simulations
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Figure A7.6")

  # Clear workspace
  rm(list=ls())

  # Source functions
  source("code/functions.R")
  out_dir <- "results/"

  # Set parameters
  macro_id <- "mac_id"
  n_macro_1 <- 200
  n_macro_2 <- 10

  # Create bounding box
  bbx <-  sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_point(c(-71.06,42.36)), sf::st_point(c(-83.05,42.33)),sf::st_point(c(-80.85,35.5))), crs = 4326) %>% sf::st_bbox() %>% sf::st_as_sfc()

  # Predict values from random field
  empty_raster <- make_empty_raster(nrowz=50, ncolz=50, resz=NULL, extentInsert=bbx %>% sf::st_transform(sf::st_crs("+proj=merc +units=km")) %>% sf::st_as_sf())  
  x_mean <- 0; x_sd <- 1; ext_varz <- "Z"; funzo <- base::mean
  raster_xy <- raster::as.data.frame(empty_raster, xy = TRUE) %>% data.table::as.data.table()
  set.seed(0);raster_xy$pred <- make_gaussian_field(newdataz= raster_xy, rasterz=empty_raster,betaz=x_mean,psillz=x_sd^2,rangez=1) %>% data.table::as.data.table() %>% .[,sim1]
  set.seed(0);raster_xy$pred_w <- make_gaussian_field(newdataz= raster_xy, rasterz=empty_raster,betaz=x_mean,psillz=x_sd^2,rangez=2000) %>% data.table::as.data.table() %>% .[,sim1]
  raster_sim <- raster_sim_w <- empty_raster; 
  raster::values(raster_sim) <- raster_xy$pred
  raster::values(raster_sim_w) <- raster_xy$pred_w

  # Render Figure A7.6a
  h_ <- 3.5; w_ <- h_*1.618
  png(paste0(out_dir,"figA7_6a.png"),width=w_,height=h_,res=300,units="in")
  par(mar=c(0,0,0,0))
  plot(raster_sim_w,col=grey.colors(255), bty="n",box=FALSE)
  dev.off()
  # Render Figure A7.6b
  png(paste0(out_dir,"figA7_6b.png"),width=w_,height=h_,res=300,units="in")
  par(mar=c(0,0,0,0))
  plot(raster_sim,col=grey.colors(255), bty="n",box=FALSE)
  dev.off()

  print("FINISHED: Figure A7.6")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})


##############################################################################
##############################################################################
## Figure A7.7
## Two examples of Poisson Point Processes used in simulations
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Figure A7.7")
         
  # Inhomogeneous Poisson Process
  winz <- spatstat.geom::owin(xrange=bbx %>% sf::st_coordinates() %>% .[,"X"] %>% range(),yrange=bbx %>% sf::st_coordinates() %>% .[,"Y"] %>% range())
  lamb_0 <- 10
  lmax <- lamb_0*10
  bbcent <- sf::st_centroid(bbx) %>% sf::st_coordinates() %>% as.numeric()
  set.seed(0)
  bbsd <- bbx %>% sf::st_coordinates() %>% data.table::as.data.table() %>% .[,lapply(.SD,function(k){range(k) %>% diff() %>% abs() %>% (function(k0){k0/3}) %>% rnorm(1,0,.)}),.SDcols=c("X","Y")] %>% as.numeric() 
  ipp_fun <- function(x,y){lmax-lamb_0*sqrt((x-bbcent[1]+bbsd[1])^2 + (y-bbcent[2]+bbsd[2])^2)} 
  if("rpoispp"%in%ls("package:spatstat.core")){
    ppz_w_ <- spatstat.core::rpoispp(lambda=ipp_fun,lmax=lmax, win=winz) %>% maptools::as.SpatialPoints.ppp() %>% sf::st_as_sf() %>% sf::st_set_crs(sf::st_crs(bbx))
  } else {
    ppz_w_ <- spatstat.random::rpoispp(lambda=ipp_fun,lmax=lmax, win=winz) %>% maptools::as.SpatialPoints.ppp() %>% sf::st_as_sf() %>% sf::st_set_crs(sf::st_crs(bbx))
  }

  # Homogeneous pp
  if("rpoispp"%in%ls("package:spatstat.core")){
    ppz <- spatstat.core::rpoispp(lambda=lmax, lmax=lmax, win=winz) %>% maptools::as.SpatialPoints.ppp() %>% sf::st_as_sf() %>% sf::st_set_crs(sf::st_crs(bbx)) 
  } else {
    ppz <- spatstat.random::rpoispp(lambda=lmax, lmax=lmax, win=winz) %>% maptools::as.SpatialPoints.ppp() %>% sf::st_as_sf() %>% sf::st_set_crs(sf::st_crs(bbx)) 
  }

  # Render Figure A7.7a
  h_ <- 3.5; w_ <- h_*1.618*.85
  png(paste0(out_dir,"figA7_7a.png"),width=w_,height=h_,res=300,units="in")
  par(mar=c(0,0,0,0))
  plot(ppz_w_,main="", pch = 1, cex=.5)
  plot(bbx,add=T)
  dev.off()
  # Render Figure A7.7a
  png(paste0(out_dir,"figA7_7b.png"),width=w_,height=h_,res=300,units="in")
  par(mar=c(0,0,0,0))
  plot(ppz,main="", pch = 1, cex=.5)
  plot(bbx,add=T)
  dev.off()

  print("FINISHED: Figure A7.7")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})



##############################################################################
##############################################################################
## Figure A7.8
## Geographic distribution of electoral and violence data in India
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Figure A7.8")

  # Clear workspace
  rm(list=ls())

  # Source functions
  source("code/functions.R")
  out_dir <- "results/"

  # Set parameters
  iso3 <- "IND"
  geomz <- "PRIOGRID"
  geomz_yr <- 2018
  geomz_unit <- "PRIO"
  unitvar <- "PRIO_CODE"

  # Load xSub data
  xSub_file <- data.table::fread(paste0("data/",iso3,"/xSub_MELTT1km1dB_Events_",iso3,".csv")) %>% sf::st_as_sf(coords=c("LONG","LAT"))

  # Basemaps
  map0 <- paste0("data/",iso3,"/GADM_",iso3,"_2018_ADM0_wgs.geojson")%>%sf::st_read()
  map2 <- paste0("data/",iso3,"/PRIOGRID_",iso3,"_2018_PRIO_wgs.geojson")%>%sf::st_read() %>% data.table::as.data.table() %>% .[,POLYGON_ID := paste0(iso3,"_",geomz,geomz_yr,"_",geomz_unit,"_",get(unitvar))] 

  # Load Elections
  clea <- paste0("data/",iso3,"/CLEA_Lower_",iso3,"_",geomz,geomz_yr,"_",geomz_unit,"_YEAR.RDS") %>% readRDS() %>% merge(map2,by="POLYGON_ID") %>% .[YEAR==1998] %>% sf::st_as_sf()

  # Render Figure A7.8a
  h_ <- 8; w_ <- h_+1
  map_x <- clea %>% .["COMPTOP2_P1_AW_MR"]
  png(paste0(out_dir,"figA7_8a.png"),width=w_,height=h_,res=300,units="in")
  par(mar=c(0,0,0,0))
  plot(map_x,pal=grey(seq(.8,0,length.out=100))%>%rev(),breaks=range(map_x[[1]],na.rm=T)%>%(function(x){seq(x[1],x[2],length.out=101)}),main="",lwd=.1,border=NA)
  dev.off()

  # Render Figure A7.8b
  w_ <- 8; h_ <- w_
  png(paste0(out_dir,"figA7_8b.png"),width=w_,height=h_,res=300,units="in")
  par(mar=c(0,0,0,0))
  plot(map0["geometry"],key.pos=NULL)
  plot(xSub_file["geometry"],add=TRUE,pch=1,cex=.8,col=rgb(0,0,0,.5))
  dev.off()

  print("FINISHED: Figure A7.8")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})


##############################################################################
##############################################################################
## Figures A8.9 and A8.10
## Relative scale and transformations of synthetic data
## Transformation quality at different percentiles of relative scale
##############################################################################
##############################################################################

## - these figures are generated by run2_main.R

##############################################################################
##############################################################################
## Table A9.8
## Rosner’s outlier tests ($\hat{X}$ estimates in Monte Carlo simulations).
##############################################################################
##############################################################################

tryCatch({
  print("STARTING: Table A9.8")

  # Clear workspace
  rm(list=ls())

  # Source functions
  source("code/functions.R")
  out_dir <- "results/"

  # Labels
  mvarmat <- data.table::data.table(mvar=c("id_match","largest_overlap","simple_overlay","area_int_poly","area_int_sf","area_int_point","pop_int_poly","pop_int_point","tps_default","tps_wresid","tps_mh","tps_mhwresid","tprs_aw","krige_ordinary","krige_universal","rasterization","mean","meta"),mlab=c("ID match","Overlay (polygons)","Overlay (centroids)","Area Weights (polygons)","Area Weights (polygons) (sf)","Area Weights (centroids)","Pop Weights (polygons)","Pop Weights (centroids)","TPRS-Forest","TPRS-Forest (w/ resid)","TPRS-Forest (MH)","TPRS-Forest (MH, w/ resid)","TPRS-Area Weights","Ordinary Kriging","Universal Kriging","Rasterization","Mean","Meta")) %>% .[,mlab := factor(mlab,levels=rev(mlab))]

  # Combine extensive and intensive
  X <- lapply(c(TRUE,FALSE),function(extnz){
    filez <- list.files("data/r_output/",pattern=ifelse(extnz,"_ext","_int"),full.names=TRUE) %>% grep("_sims_",.,value=TRUE) 
    sims_mat_ <- lapply(filez,function(f0){readRDS(f0) %>% dplyr::bind_rows()}) %>% data.table::rbindlist(fill=TRUE) %>% .[,bias := beta_hat-beta] %>% .[,bias_w := beta_hat_w-beta_w] %>% .[,method:=factor(method,levels=mvarmat[,mvar])]  %>% .[,proc_time_sec := NA_real_] %>% .[grep("secs",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric()] %>% .[grep("mins",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*60})] %>% .[grep("hours",proc_time),proc_time_sec := proc_time %>% stringr::str_split(" ",simplify=T) %>% .[,1] %>% as.numeric() %>% (function(x){x*3600})] %>% .[,n_ratio_21 := n_macro_2/n_macro_1] %>% .[,n_ratio_12 := n_macro_1/n_macro_2] %>% .[!method%in%c("area_int_sf")] %>% .[,diverge:=rs-rn] %>% data.table::setnames(c("rn_full","rs_full"),c("rn_nn","rs_nn"),skip_absent=TRUE) %>% .[,extensive := extnz]
    sims_mat_
  }) %>% data.table::rbindlist(fill=TRUE)

  # Rosner outlier tests
  varz_ <- "est_mean"
  ix <- X %>% .[,.SD,.SDcols=c("sim","source","n_macro_1","n_macro_2","varz","extensive")] %>% unique()  %>% .[varz%in%"X_W"]
  out_mat <- parallel::mclapply(1:nrow(ix),function(i0){#print(i0)
    X_ <- X %>% .[sim%in%ix[i0,sim]&source%in%ix[i0,source]&n_macro_1%in%ix[i0,n_macro_1]&n_macro_2%in%ix[i0,n_macro_2]&varz%in%ix[i0,varz]&extensive%in%ix[i0,extensive]] %>% .[,.SD,.SDcols=c("method",varz_)] %>% unique() 
    if(X_[,sd(get(varz_))]<1e-10){
      out_mat_ <- data.table::data.table(method=NA_character_,i=i0,kmax=floor(nrow(X_)/2))  
    } else {
      out_list <- lapply(1:floor(nrow(X_)/2),function(k0){#print(k0)
        suppressWarnings({
        outz <- X_ %>% .[,EnvStats::rosnerTest(get(varz_), k = k0)] %>% .["all.stats"] %>% data.table::as.data.table() %>% .[all.stats.Outlier==TRUE,all.stats.Obs.Num]
        })
        list(i=i0,k=k0,outz=outz,method_outz=X_[outz,as.character(method)])
      })
      out_mat_ <- data.table::data.table(method=NA_character_,i=i0,kmax=length(out_list))
      if(length(out_list %>% sapply("[","method_outz") %>% unlist())>0){
        out_mat_ <- out_list %>% sapply("[","method_outz") %>% unlist() %>% as.character() %>% data.table::data.table(method=.) %>% .[,i:=i0] %>% .[,kmax := length(out_list)]
      }
    }
    out_mat_
  },mc.cores=parallel::detectCores()-1) %>% data.table::rbindlist()
  out_tab <- out_mat %>% .[,method %>% .[!is.na(.)] %>% table()] %>% data.table::as.data.table() %>% data.table::setnames(c("Method","Frequency")) %>% .[,Method := Method %>% match(mvarmat[,mvar]) %>% mvarmat[.,mlab]]  %>% dplyr::bind_rows(.,.[,{!mvarmat[,mlab] %in% Method} %>% mvarmat[.,mlab] %>% as.character()] %>% data.table::data.table(Method=.,Frequency=0)) %>% .[order(Frequency)] %>% .[Method %in% X[,method %>% match(mvarmat[,mvar]) %>% mvarmat[.,mlab]]] %>% .[,Frequency := {Frequency/(nrow(ix)*out_mat[,max(kmax)])} %>% smart_round(rnd=2)]

  # Save to file
  out_tab%>%data.table::setnames("Frequency","$\\omega_k$")%>%xtable::xtable(caption=paste0("Rosner's outlier tests ($\\hat{X}$ estimates in Monte Carlo simulations)."),label=paste0("tab:rosner_ext"))%>%xtable::print.xtable(booktabs=TRUE,hline.after=c(-1,0,nrow(.)),include.rownames=FALSE,table.placement = "H",file=paste0(out_dir,"tabA9_8.tex"),size="footnotesize",sanitize.text.function = identity)

  print("FINISHED: Table A9.8")
},error=function(e){print(paste0(e%>%gsub("Error","Warning",.),"... proceeding to next"))})


# Print upon finishing
print("**** Finished run3_appendix.R ****")