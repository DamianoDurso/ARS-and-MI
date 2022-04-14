#Code for making plot to look at goodness-of-fit measures distribution


Mean.Res_1fac_ARS$seed-200801

layout(matrix(c(1:6), 2, 3, byrow = TRUE))


#for(k in c(13,15,17,25,27,29)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
   print(hist(NoARS_Conf_RMSEA, 
              main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                             ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
              xaxt = "n", xlim = c(0, 0.2)))
   axis(1, at = seq(0,.2,.01))
   
#   print(hist(NoARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
#   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
#   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
   
  
}

#for(k in c(19,21,23,31,33,35)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_RMSEA, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(0, 0.2)))
  axis(1, at = seq(0,.2,.01))
  #   print(hist(NoARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}

#for(k in c(14,16,18,26,28,30)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_RMSEA, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(0, 0.2)))
  axis(1, at = seq(0,.2,.01))
  
  #   print(hist(NoARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}

#for(k in c(20,22,24,32,34,36)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_RMSEA, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(0, 0.2)))
  axis(1, at = seq(0,.2,.01))
  #   print(hist(NoARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}

#2Factors conditions

#RMSEA
for(k in c(49,51,53,61,63,65)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_RMSEA, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(0, 0.2)))
  axis(1, at = seq(0,.2,.01))
  #   print(hist(NoARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}

for(k in c(55,57,59,67,69,71)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_RMSEA, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(0, 0.2)))
  axis(1, at = seq(0,.2,.01))
  #   print(hist(NoARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}

for(k in c(50,52,54,62,64,66)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_RMSEA, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(0, 0.2)))
  axis(1, at = seq(0,.2,.01))
  #   print(hist(NoARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}

for(k in c(56,58,60,68,70,72)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_RMSEA, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(0, 0.2)))
  axis(1, at = seq(0,.2,.01))
  #   print(hist(NoARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}

#CFI
for(k in c(49,51,53,61,63,65)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_cfi, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(.80, 1)))
  axis(1, at = seq(0.80,1,.02))
  #   print(hist(NoARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}

for(k in c(55,57,59,67,69,71)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_cfi, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(.80, 1)))
  axis(1, at = seq(0.80,1,.02))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}

for(k in c(50,52,54,62,64,66)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_cfi, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(.80, 1)))
  axis(1, at = seq(0.80,1,.02))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}

for(k in c(56,58,60,68,70,72)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  for(i in 1:100){
    #NOARS results
    NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
    NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
    NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
    
    NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
    NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
    NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
    NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
    
    NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
    NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
    NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
    NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))
    
    #NOARS results absolute fit
    Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
    Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
    Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01
    
    Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
    Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
    Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01
    
    
    #ARS RESULTS
    ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
    ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
    ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_chi[i] <- NA# < .05
    } else {
      ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
    }
    ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
    ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Thr_comb[i] <- NA# < .05
    } else {
      ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                               (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                    Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
    }
    
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_chi[i] <- NA# < .05
    } else {
      ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
    }
    ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
    ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_Load_comb[i] <- NA# < .05
    } else {
      ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                                (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                     Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
    }
    
    
    
    # ARS Absolute fit
    Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
    
    
    Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
    Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
    Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01
    
    
    #ARS VS NO ars
    if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
      ARS_vs_noARS[i] <- NA# < .05
    } else {
      ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
    }
    
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
      Conv.Conf.ARS[i] <- "Yes"
    } else {
      Conv.Conf.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
      Conv.Thr.ARS[i] <- "Yes"
    } else {
      Conv.Thr.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
      Conv.Load.ARS[i] <- "Yes"
    } else {
      Conv.Load.ARS[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
      Conv.Conf[i] <- "Yes"
    } else {
      Conv.Conf[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
      Conv.Thr[i] <- "Yes"
    } else {
      Conv.Thr[i] <- "No"
    }
    
    if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
      Conv.Load[i] <- "Yes"
    } else {
      Conv.Load[i] <- "No"
    }
    
  } 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
  
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
  #Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
  print(hist(NoARS_Conf_cfi, 
             main = paste0 ("N = ", simdes$sample[k], ", ARS = ", simdes$ARS[k], 
                            ", Nfac = ", simdes$Nfac[k], ", scale = ", simdes$scale[k]), 
             xaxt = "n", xlim = c(.80, 1)))
  axis(1, at = seq(0.80,1,.02))
  #   print(hist(ARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
  #   print(hist(ARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
  
}
