
suppressMessages(library("plyr", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("data.table", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("crayon", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("withr", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("ggplot2", lib.loc = "/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("farver", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("labeling", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("optparse", lib.loc = "/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("dplyr", lib.loc = "/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("withr", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("backports", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("broom", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("rstudioapi", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("cli", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("tzdb", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("tidyverse", lib.loc="/nfs/team151/software/manuel_R_libs_4_1//"))
suppressMessages(library("svglite", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("ggeasy", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("sandwich", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("digest", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
suppressMessages(library("ggforce", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))



opt = NULL

#options(warn = 1)

Stats_function_non_compositional_data = function(option_list)
{
  library("lme4", lib.loc = "/nfs/team151/software/manuel_R_libs_4_1/")
  library("afex", lib.loc = "/nfs/team151/software/manuel_R_libs_4_1/")
  
  
  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")
  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("OUT_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
 
 
  
  #### Read Flow_cyt_data ----
  
  #
  
  Flow_cyt_data<-as.data.frame(fread(file=opt$Flow_cyt_data, sep=",") , stringsAsFactors=F)[,(1:11)]
  
  Flow_cyt_data$log_CD41_MFI<-log(Flow_cyt_data[,10])
  Flow_cyt_data$log_CD41_GeoMFI<-log(Flow_cyt_data[,11])
  
  
  cat("Flow_cyt_data_0\n")
  cat(str(Flow_cyt_data))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data$Condition)))))
  cat("\n")
  
  # ##################################
  # quit(status = 1)
  
  
  Basal.df<-Flow_cyt_data[which(Flow_cyt_data$Condition == 'unst' &
                                  Flow_cyt_data$Time%in%c("16","24")),]
  
  cat("Basal.df_0\n")
  cat(str(Basal.df))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df$Condition)))))
  cat("\n")
  
  
  
  cols<-c(which(colnames(Basal.df) == 'log_CD41_MFI'),which(colnames(Basal.df) == 'log_CD41_GeoMFI'))
  
  cat("cols_0\n")
  cat(str(cols))
  cat("\n")
  
  Basal.df.dt<-data.table(Basal.df, key=c("Genotype", "Condition"))
  
  
  
  Basal.df.addition <- as.data.frame(Basal.df.dt[, sapply(.SD, function(x) list(mean = mean(x))), .SDcols = cols, by = key(Basal.df.dt)], stringsAsFactors=F)
  
  colnames(Basal.df.addition)<-gsub("\\.mean","",colnames(Basal.df.addition))
  
  Basal.df.addition$Time<-'Basal'
  
  
  
  cat("Basal.df.addition_0\n")
  cat(str(Basal.df.addition))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition$Condition)))))
  cat("\n")
  
  
  
  
  Basal.df.addition_new_1 <- Basal.df.addition[rep(seq_len(nrow(Basal.df.addition)), each = 3), ]
  
  Basal.df.addition_new_1$Sample<-unique(Basal.df$Sample)
  
  cat("Basal.df.addition_new_1_0\n")
  cat(str(Basal.df.addition_new_1))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1$Condition)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1$Sample))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1$Sample)))))
  cat("\n")
  
  Basal.df.addition_new_1_unst<-Basal.df.addition_new_1
  
  Basal.df.addition_new_1_PMA<-Basal.df.addition_new_1
  
  Basal.df.addition_new_1_PMA$Condition<-'PMA'
  
  Basal.df.addition_new_1_DEF<-rbind(Basal.df.addition_new_1_unst,Basal.df.addition_new_1_PMA)
 
  cat("Basal.df.addition_new_1_DEF_0\n")
  cat(str(Basal.df.addition_new_1_DEF))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1_DEF$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1_DEF$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1_DEF$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1_DEF$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1_DEF$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1_DEF$Condition)))))
  cat("\n")
  
 
  indx.int<-c(which(colnames(Flow_cyt_data) == "Sample"),which(colnames(Flow_cyt_data) == "Genotype"),
              which(colnames(Flow_cyt_data) == "Time"),which(colnames(Flow_cyt_data) == "Condition"),
              which(colnames(Flow_cyt_data) == "log_CD41_MFI"),which(colnames(Flow_cyt_data) == "log_CD41_GeoMFI"))
  
  
  
  Flow_cyt_data_no_compositional<-unique(Flow_cyt_data[,indx.int])
  
  cat("Flow_cyt_data_no_compositional_0\n")
  cat(str(Flow_cyt_data_no_compositional))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data_no_compositional$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data_no_compositional$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data_no_compositional$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data_no_compositional$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data_no_compositional$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data_no_compositional$Condition)))))
  cat("\n")
  
  Flow_cyt_data_no_compositional<-rbind(Flow_cyt_data_no_compositional,Basal.df.addition_new_1_DEF)
  ##### Transform into ordered factors ----
  
  Flow_cyt_data_no_compositional$Genotype<-factor(Flow_cyt_data_no_compositional$Genotype,
                                 levels=c("WT","KI","Del"),
                                 ordered=T)
  
  Flow_cyt_data_no_compositional$Time<-factor(Flow_cyt_data_no_compositional$Time,
                                 levels=c("Basal","16","24","48","72"),
                                 ordered=T)
  
  Flow_cyt_data_no_compositional$Condition<-factor(Flow_cyt_data_no_compositional$Condition,
                                 levels=c("unst","PMA"),
                                 ordered=T)
  
  
  cat("Flow_cyt_data_no_compositional_1\n")
  cat(str(Flow_cyt_data_no_compositional))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_no_compositional$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_no_compositional$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_no_compositional$Time)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_no_compositional$Time))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_no_compositional$Condition)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_no_compositional$Condition))))
  cat("\n")
  
  # #####################
  # quit(status = 1)
 
  
  #### Summary table parameters----
  
  cols<-c(which(colnames(Flow_cyt_data_no_compositional) == 'log_CD41_MFI'),which(colnames(Flow_cyt_data_no_compositional) == 'log_CD41_GeoMFI'))
  
  cat("cols_0\n")
  cat(str(cols))
  cat("\n")
  
  
  Flow_cyt_data_no_compositional.dt<-data.table(Flow_cyt_data_no_compositional, key=c("Genotype","Condition","Time"))
  
  
  
  Summary_table_parameters_mean <- as.data.frame(Flow_cyt_data_no_compositional.dt[, sapply(.SD, function(x) list(mean = mean(x))), .SDcols = cols, by = key(Flow_cyt_data_no_compositional.dt)], stringsAsFactors=F)
  
 
 
  
  cat("Summary_table_parameters_mean_0\n")
  cat(str(Summary_table_parameters_mean))
  cat("\n")
  
  
  Summary_table_parameters_mean.m<-melt(Summary_table_parameters_mean, id.vars=c("Genotype","Condition","Time"),value.name = "Mean",variable.name = "Parameter")
  
  Summary_table_parameters_mean.m$Parameter<-gsub("\\.mean","",Summary_table_parameters_mean.m$Parameter)
  
  cat("Summary_table_parameters_mean.m_0\n")
  cat(str(Summary_table_parameters_mean.m))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Summary_table_parameters_mean.m$Parameter))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Summary_table_parameters_mean.m$Parameter)))))
  cat("\n")
  
  Summary_table_parameters_sd <- as.data.frame(Flow_cyt_data_no_compositional.dt[, sapply(.SD, function(x) list(sd = sd(x))), .SDcols = cols, by = key(Flow_cyt_data_no_compositional.dt)], stringsAsFactors=F)
  
  
  
  
  cat("Summary_table_parameters_sd_0\n")
  cat(str(Summary_table_parameters_sd))
  cat("\n")
  
  
  Summary_table_parameters_sd.m<-melt(Summary_table_parameters_sd, id.vars=c("Genotype","Condition","Time"),value.name = "sd",variable.name = "Parameter")
  
  Summary_table_parameters_sd.m$Parameter<-gsub("\\.sd","",Summary_table_parameters_sd.m$Parameter)
  
  cat("Summary_table_parameters_sd.m_0\n")
  cat(str(Summary_table_parameters_sd.m))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Summary_table_parameters_sd.m$Parameter))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Summary_table_parameters_sd.m$Parameter)))))
  cat("\n")
  
  
  Summary_table_DEF<-merge(Summary_table_parameters_mean.m,
                           Summary_table_parameters_sd.m,
                           by=c("Genotype","Condition","Time","Parameter"))
  
  Summary_table_DEF$Parameter<-factor(Summary_table_DEF$Parameter,
                                      levels=c('log_CD41_MFI','log_CD41_GeoMFI'),
                                      ordered=T)
  
 
  
  cat("Summary_table_DEF_0\n")
  cat(str(Summary_table_DEF))
  cat("\n")
  cat(sprintf(as.character(names(summary(Summary_table_DEF$Parameter)))))
  cat("\n")
  cat(sprintf(as.character(summary(Summary_table_DEF$Parameter))))
  cat("\n")
  
  
  Summary_table_DEF<-Summary_table_DEF[order(Summary_table_DEF$Parameter,Summary_table_DEF$Genotype,Summary_table_DEF$Condition,Summary_table_DEF$Time),]
  
  cat("Summary_table_DEF_1\n")
  cat(str(Summary_table_DEF))
  cat("\n")
  cat(sprintf(as.character(names(summary(Summary_table_DEF$Parameter)))))
  cat("\n")
  cat(sprintf(as.character(summary(Summary_table_DEF$Parameter))))
  cat("\n")
  
  #### NEW PATH ----
  
  path5<-paste(out,'EXP_20062023','/', sep='')
  
  if (file.exists(path5)){
    
    
    
    
  } else {
    dir.create(file.path(path5))
    
  }
  
  setwd(path5)
  
  saveRDS(Summary_table_DEF,file="Summary_table_non_compositional_data.rds")
  saveRDS(Flow_cyt_data_no_compositional,file="Flow_cyt_data_no_compositional_plus_BASAL_non_compositional_data.rds")
  
  
 
  #### LOOP to ITERATE the GENOTYPES ----
  
  
  Genotypes_array<-levels(Flow_cyt_data_no_compositional$Genotype)
  
  Condition_DEBUG <- 1
  
  comparison<-data.frame()
  
  for(i in 1:length(Genotypes_array))
  {
    Genotype_sel<-Genotypes_array[i]
    
    cat("------------------>\t")
    cat(sprintf(as.character(Genotype_sel)))
    cat("\n")
    
    
    
    if(i > 1 & i<length(Genotypes_array))
    {
      comparison_terms_1<-c(Genotypes_array[1],Genotypes_array[i])
      comparison_string_1<-paste(comparison_terms_1, collapse="_")
      comparison_terms_2<-c(Genotypes_array[i],Genotypes_array[i+1])
      comparison_string_2<-paste(comparison_terms_2, collapse="_")
      
      
      comparison_DEF<-as.data.frame(rbind(cbind('1',comparison_string_1),
                            cbind('2',comparison_string_2)), stringsAsFactors=F)
      
     
      
      colnames(comparison_DEF)<-c("iteration","comparison")
      
      cat("comparison_DEF_1\n")
      cat(str(comparison_DEF))
      cat("\n") 
                            
      comparison<-rbind(comparison_DEF,comparison)    
      
      cat("comparison_0\n")
      cat(str(comparison))
      cat("\n")  
      
    }# i > 1 & i<length(Genotypes_array)
    if(i == length(Genotypes_array))
    {
      comparison_terms_3<-c(Genotypes_array[1],Genotypes_array[i])
      comparison_string_3<-paste(comparison_terms_3, collapse="_")
      
      comparison_DEF<-as.data.frame(cbind('3',comparison_string_3), stringsAsFactors=F)
     
      colnames(comparison_DEF)<-c("iteration","comparison")
      
      cat("comparison_DEF_0\n")
      cat(str(comparison_DEF))
      cat("\n") 
      
      comparison<-rbind(comparison_DEF,comparison)     
      
      cat("comparison_0\n")
      cat(str(comparison))
      cat("\n")  
      
      
    }#i == length(Genotypes_array)
    
  }#i in 1:length(Genotypes_array)
    
  cat("comparison_1\n")
  cat(str(comparison))
  cat("\n")  
   
  for(i in 1:dim(comparison)[1])  
  {
    comparison_string_sel<-comparison$comparison[i]
    
    cat("----------------comparison_string_sel---------------------->\t")
    cat(sprintf(as.character(comparison_string_sel)))
    cat("\n")  
    
    comparison_pair<-unlist(strsplit(comparison_string_sel, split="_"))   
    
    # cat("comparison_pair_0\n")
    # cat(str(comparison_pair))
    # cat("\n") 

      Flow_cyt_data_no_compositional_sel<-droplevels(Flow_cyt_data_no_compositional[which(Flow_cyt_data_no_compositional$Genotype%in%comparison_pair),])

      # cat("Flow_cyt_data_no_compositional_sel_0\n")
      # cat(str(Flow_cyt_data_no_compositional_sel))
      # cat("\n")


      # cat(sprintf(as.character(names(summary(Flow_cyt_data_no_compositional_sel$Genotype)))))
      # cat("\n")
      # cat(sprintf(as.character(summary(Flow_cyt_data_no_compositional_sel$Genotype))))
      # cat("\n")
      # cat(sprintf(as.character(names(summary(Flow_cyt_data_no_compositional_sel$Time)))))
      # cat("\n")
      # cat(sprintf(as.character(summary(Flow_cyt_data_no_compositional_sel$Time))))
      # cat("\n")
      # cat(sprintf(as.character(names(summary(Flow_cyt_data_no_compositional_sel$Condition)))))
      # cat("\n")
      # cat(sprintf(as.character(summary(Flow_cyt_data_no_compositional_sel$Condition))))
      # cat("\n")


      Condition_array<-levels(Flow_cyt_data_no_compositional_sel$Condition)

      # cat("Condition_array\n")
      # cat(str(Condition_array))
      # cat("\n")

      for(j in 1:length(Condition_array))
      {

        Condition_array_sel<-Condition_array[j]

        cat("-condition-->\t")
        cat(sprintf(as.character(Condition_array_sel)))
        cat("\n")

        Flow_cyt_data_no_compositional_sel_condition_sel<-droplevels(Flow_cyt_data_no_compositional_sel[which(Flow_cyt_data_no_compositional_sel$Condition == Condition_array_sel),])

        # cat("Flow_cyt_data_no_compositional_sel_condition_sel_0\n")
        # cat(str(Flow_cyt_data_no_compositional_sel_condition_sel))
        # cat("\n")
        # 
        # 
        # cat(sprintf(as.character(names(summary(Flow_cyt_data_no_compositional_sel_condition_sel$Genotype)))))
        # cat("\n")
        # cat(sprintf(as.character(summary(Flow_cyt_data_no_compositional_sel_condition_sel$Genotype))))
        # cat("\n")
        # cat(sprintf(as.character(names(summary(Flow_cyt_data_no_compositional_sel_condition_sel$Time)))))
        # cat("\n")
        # cat(sprintf(as.character(summary(Flow_cyt_data_no_compositional_sel_condition_sel$Time))))
        # cat("\n")
        # cat(sprintf(as.character(names(summary(Flow_cyt_data_no_compositional_sel_condition_sel$Condition)))))
        # cat("\n")
        # cat(sprintf(as.character(summary(Flow_cyt_data_no_compositional_sel_condition_sel$Condition))))
        # cat("\n")
        
        # #################################
        # quit(status = 1)

        for(k in 5:dim(Flow_cyt_data_no_compositional_sel_condition_sel)[2])
        {
          continuous_variable_sel<-colnames(Flow_cyt_data_no_compositional_sel_condition_sel)[k]

          cat("--->\t")
          cat(sprintf(as.character(continuous_variable_sel)))
          cat("\n")

          path6<-paste(path5,continuous_variable_sel,'/', sep='')

          if (file.exists(path6)){




          } else {
            dir.create(file.path(path6))

          }



          # indx.parameter_sel<-which(colnames(Flow_cyt_data_no_compositional_sel_condition_sel) == continuous_variable_sel)
          #
          #
          # cat("indx.parameter_sel\n")
          # cat(str(indx.parameter_sel))
          # cat("\n")


          Summary_table_DEF_sel<-droplevels(Summary_table_DEF[which(Summary_table_DEF$Parameter == continuous_variable_sel &
                                                                      Summary_table_DEF$Genotype%in%comparison &
                                                                      Summary_table_DEF$Condition == Condition_array_sel),])

          # cat("Summary_table_DEF_sel_0\n")
          # cat(str(Summary_table_DEF_sel))
          # cat("\n")



          #### full model ----

          full_model<-lmer(Flow_cyt_data_no_compositional_sel_condition_sel[,k] ~ 1 + Genotype:as.numeric(Time) +
                             Genotype + as.numeric(Time) +
                             (1 + 1|Sample), data= Flow_cyt_data_no_compositional_sel_condition_sel)

          check_full_model<-isSingular(full_model,  tol = 1e-4)

          # cat("check_full_model\n")
          # cat(str(check_full_model))
          # cat("\n")

          Summary_model<-summary(full_model)

          intermediate_model<-lmer(Flow_cyt_data_no_compositional_sel_condition_sel[,k] ~ 1 +
                                     Genotype + as.numeric(Time) +
                                     (1 + 1|Sample) , data= Flow_cyt_data_no_compositional_sel_condition_sel)

          check_intermediate_model<-isSingular(intermediate_model,  tol = 1e-4)

          # cat("check_intermediate_model\n")
          # cat(str(check_intermediate_model))
          # cat("\n")

          Summary_model<-summary(intermediate_model)

          # cat("Summary_model\n")
          # cat(str(Summary_model))
          # cat("\n")

          reduced_model<-lmer(Flow_cyt_data_no_compositional_sel_condition_sel[,k] ~ 1 +
                                as.numeric(Time) +
                                (1 + 1|Sample), data= Flow_cyt_data_no_compositional_sel_condition_sel)

          check_reduced_model<-isSingular(reduced_model,  tol = 1e-4)

          # cat("check_reduced_model\n")
          # cat(str(check_reduced_model))
          # cat("\n")

          LRT_model_intermediate<-anova(reduced_model,intermediate_model)

          cat("LRT_model_intermediate\n")
          cat(str(LRT_model_intermediate))
          cat("\n")

          LRT_model_full<-anova(intermediate_model,full_model)

          cat("LRT_model_full\n")
          cat(str(LRT_model_full))
          cat("\n")

          setwd(path6)

          write.table(LRT_model_full, file=paste("LRT_model_full_",Condition_array_sel,'_',comparison_string_sel,".tsv",sep=''), sep="\t", quote=F)


          write.table(LRT_model_intermediate, file=paste("LRT_model_intermediate_",Condition_array_sel,'_',comparison_string_sel,".tsv",sep=''), sep="\t", quote=F)

          coefficients<-Summary_model$coefficients

          # cat("coefficients\n")
          # cat(str(coefficients))
          # cat("\n")

          write.table(coefficients, file=paste("coefficients_",Condition_array_sel,'_',comparison_string_sel,".tsv",sep=''), sep="\t", quote=F)


          # #######################################
          # quit(status = 1)


        }# k in 5:dim(Flow_cyt_data_no_compositional_sel_condition_sel)[2]
      }# j in 1:length(Condition_array)

  }#i in 1:dim(comparison_DEF)[1]
  

 
 
}

graph_non_compositional_data = function(option_list)
{
  suppressMessages(library("cowplot", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
  suppressMessages(library("ggforce", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
  suppressMessages(library("ggnewscale", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
  suppressMessages(library("scales", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
  
  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")
  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("OUT_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  
  #### NEW PATH ----
  
  path5<-paste(out,'EXP_20062023','/', sep='')
  
  if (file.exists(path5)){
    
    
    
    
  } else {
    dir.create(file.path(path5))
    
  }
  
  setwd(path5)
  
  Summary_table_DEF<-readRDS(file="Summary_table_non_compositional_data.rds")
  
  
  cat("Summary_table_DEF_0\n")
  cat(str(Summary_table_DEF))
  cat("\n")
  cat(sprintf(as.character(names(summary(Summary_table_DEF$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(Summary_table_DEF$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Summary_table_DEF$Time)))))
  cat("\n")
  cat(sprintf(as.character(summary(Summary_table_DEF$Time))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Summary_table_DEF$Condition)))))
  cat("\n")
  cat(sprintf(as.character(summary(Summary_table_DEF$Condition))))
  cat("\n")
  
  Flow_cyt_data<-readRDS(file="Flow_cyt_data_no_compositional_plus_BASAL_non_compositional_data.rds")
  
  cat("Flow_cyt_data_0\n")
  cat(str(Flow_cyt_data))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data$Time)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data$Time))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data$Condition)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data$Condition))))
  cat("\n")
  
  #### sina plot ----
  
  
  parameter_array<-levels(Summary_table_DEF$Parameter)
  
  CONDITION_DEBUG<-1
  
  for(i in 1:length(parameter_array))
  {
    parameter_array_sel<-parameter_array[i]
    
    cat("---------------Parameter------------>\t")
    cat(sprintf(as.character(parameter_array_sel)))
    cat("\n")
    
    path6<-paste(path5,parameter_array_sel,'/', sep='')
    
    if (file.exists(path6)){
      
      
      
      
    } else {
      dir.create(file.path(path6))
      
    }
    
    Summary_table_DEF_sel<-droplevels(Summary_table_DEF[which(Summary_table_DEF$Parameter == parameter_array_sel),])
    
    cat("Summary_table_DEF_sel_0\n")
    cat(str(Summary_table_DEF_sel))
    cat("\n")
    cat(sprintf(as.character(names(summary(Summary_table_DEF_sel$Genotype)))))
    cat("\n")
    cat(sprintf(as.character(summary(Summary_table_DEF_sel$Genotype))))
    cat("\n")
    cat(sprintf(as.character(names(summary(Summary_table_DEF_sel$Time)))))
    cat("\n")
    cat(sprintf(as.character(summary(Summary_table_DEF_sel$Time))))
    cat("\n")
    cat(sprintf(as.character(names(summary(Summary_table_DEF_sel$Condition)))))
    cat("\n")
    cat(sprintf(as.character(summary(Summary_table_DEF_sel$Condition))))
    cat("\n")
    
    indx.Parameter<-which(colnames(Flow_cyt_data) == parameter_array_sel)
    
    cat("indx.Parameter_0\n")
    cat(str(indx.Parameter))
    cat("\n")
    
    A<-round(summary(Flow_cyt_data[,indx.Parameter]),2)
    
    
    cat("summary_parameter\n")
    cat(sprintf(as.character(names(A))))
    cat("\n")
    cat(sprintf(as.character(A)))
    cat("\n")
    
    step<-abs(A[6]-A[1])/10
    
    if(step == 0)
    {
      
      step<-0.25
    }
    
    breaks.Rank<-seq(from= A[1], to=A[6]+step,by=step)
    labels.Rank<-as.character(round(breaks.Rank,2))
    
    if(CONDITION_DEBUG == 1)
    {
      cat("labels.Rank_0\n")
      cat(sprintf(as.character(labels.Rank)))
      cat("\n")
    }
    
    # 
    # aes(shape=Genotype,color=Genotype)
    
    name_for_yaxis<-NULL
    
    if(parameter_array_sel == 'log_CD41_MFI')
    {
      
      name_for_yaxis<-'log(CD41 MFI)'
      
    }
    if(parameter_array_sel == 'log_CD41_GeoMFI')
    {
      name_for_yaxis<-'log(CD41 GeoMFI)'
    }
    
    graph<-ggplot(data=Flow_cyt_data, 
                  aes(x=Time, 
                      y=Flow_cyt_data[,indx.Parameter],
                      color=Genotype,
                      shape=Genotype)) +
      geom_jitter(size = 4,height = 0.05, width=0.2)+
      geom_line(aes(group=Sample))+
      scale_shape_manual(name = paste("Genotype","of","Clone",sep="\n"), values = c("WT"=15,"KI"=16,"Del"=17), breaks=c("WT","KI","Del"), drop=F)+
      theme(plot.title = element_text(size=11)) +
      theme(plot.title=element_text(size=11))+
      scale_color_manual(values = c('#32A852','#1877C9','#553B68','#D45E85','#6DB2EE','#62D07F','#C9244B','#87447B','#D6B8E6'),
                         drop=F)+
      ggeasy::easy_center_title()
    
    graph<-graph+
      theme_cowplot(font_size = 12)+
      facet_grid(. ~ Condition)+
      theme(axis.text.x=element_text(angle=0,size=14,colour="black",family="sans"),
            axis.title.y=element_text(size=16,family="sans"),
            legend.title=element_text(size=16,family="sans"),
            legend.text=element_text(size=12,family="sans",colour="black"),
            axis.text.y=element_text(colour="black",size=10,family="sans"))+
      scale_x_discrete(name=NULL, drop=F)+
      scale_y_continuous(name=name_for_yaxis,breaks=breaks.Rank,labels=labels.Rank, limits=c(breaks.Rank[1],breaks.Rank[length(breaks.Rank)]))+
      theme(legend.position="right",legend.title=element_blank(), legend.text = element_text(size=10))+
      guides(fill=guide_legend(nrow=3,byrow=TRUE))
    
    setwd(path6)
    
    svglite(paste('Sina_plot_',paste(parameter_array_sel, collapse="_"),'.svg',sep=''), width = 8, height = 8)
    print(graph)
    dev.off()
    
    # ####################################
    # quit(status = 1)
    
    
  }#i in 1:length(parameter_array)
 
  
}

Stats_function_compositional_data = function(option_list)
{
  library("carData", lib.loc = "/nfs/team151/software/manuel_R_libs_4_1/")
  library("car", lib.loc = "/nfs/team151/software/manuel_R_libs_4_1/")
  
  
  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")
  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("OUT_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  
  
  
  #### Read Flow_cyt_data ----
  
  #
  
  Flow_cyt_data<-as.data.frame(fread(file=opt$Flow_cyt_data, sep=",") , stringsAsFactors=F)[,(1:8)]
  
  
  
  cat("Flow_cyt_data_0\n")
  cat(str(Flow_cyt_data))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data$Condition)))))
  cat("\n")
  
  # ##################################
  # quit(status = 1)
  
  
  Basal.df<-Flow_cyt_data[which(Flow_cyt_data$Condition == 'unst' &
                                  Flow_cyt_data$Time%in%c("16","24")),]
  
  cat("Basal.df_0\n")
  cat(str(Basal.df))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df$Condition)))))
  cat("\n")
  
  
  
  cols<-colnames(Basal.df)[5:dim(Basal.df)[2]]
  
  cat("cols_0\n")
  cat(str(cols))
  cat("\n")
  
  Basal.df.dt<-data.table(Basal.df, key=c("Genotype", "Condition"))
  
  
  
  Basal.df.addition <- as.data.frame(Basal.df.dt[, sapply(.SD, function(x) list(mean = mean(x))), .SDcols = cols, by = key(Basal.df.dt)], stringsAsFactors=F)
  
  colnames(Basal.df.addition)<-gsub("\\.mean","",colnames(Basal.df.addition))
  
  Basal.df.addition$Time<-'Basal'
  
  
  
  cat("Basal.df.addition_0\n")
  cat(str(Basal.df.addition))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition$Condition)))))
  cat("\n")
  
  
  
  
  Basal.df.addition_new_1 <- Basal.df.addition[rep(seq_len(nrow(Basal.df.addition)), each = 3), ]
  
  Basal.df.addition_new_1$Sample<-unique(Basal.df$Sample)
  
  cat("Basal.df.addition_new_1_0\n")
  cat(str(Basal.df.addition_new_1))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1$Condition)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1$Sample))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1$Sample)))))
  cat("\n")
  
  Basal.df.addition_new_1_unst<-Basal.df.addition_new_1
  
  Basal.df.addition_new_1_PMA<-Basal.df.addition_new_1
  
  Basal.df.addition_new_1_PMA$Condition<-'PMA'
  
  Basal.df.addition_new_1_DEF<-rbind(Basal.df.addition_new_1_unst,Basal.df.addition_new_1_PMA)
  
  cat("Basal.df.addition_new_1_DEF_0\n")
  cat(str(Basal.df.addition_new_1_DEF))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1_DEF$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1_DEF$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1_DEF$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1_DEF$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Basal.df.addition_new_1_DEF$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Basal.df.addition_new_1_DEF$Condition)))))
  cat("\n")
  
  
 
  
  
  Flow_cyt_data_compositional<-Flow_cyt_data
  
  cat("Flow_cyt_data_compositional_0\n")
  cat(str(Flow_cyt_data_compositional))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data_compositional$Genotype))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data_compositional$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data_compositional$Time))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data_compositional$Time)))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(Flow_cyt_data_compositional$Condition))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(Flow_cyt_data_compositional$Condition)))))
  cat("\n")
  
  Flow_cyt_data_compositional<-rbind(Flow_cyt_data_compositional,Basal.df.addition_new_1_DEF)
  ##### Transform into ordered factors ----
  
  Flow_cyt_data_compositional$Genotype<-factor(Flow_cyt_data_compositional$Genotype,
                                                  levels=c("WT","KI","Del"),
                                                  ordered=T)
  
  Flow_cyt_data_compositional$Time<-factor(Flow_cyt_data_compositional$Time,
                                              levels=c("Basal","16","24","48","72"),
                                              ordered=T)
  
  Flow_cyt_data_compositional$Condition<-factor(Flow_cyt_data_compositional$Condition,
                                                   levels=c("unst","PMA"),
                                                   ordered=T)
  
  
  cat("Flow_cyt_data_compositional_1\n")
  cat(str(Flow_cyt_data_compositional))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_compositional$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional$Time)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_compositional$Time))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional$Condition)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_compositional$Condition))))
  cat("\n")
  
  
  #### Scale per sample by the geometric mean ----
  
  Flow_cyt_data_compositional.m<-melt(Flow_cyt_data_compositional, id.variables=c("Sample","Genotype","Condition","Time"), value.name="value", variable.name="parameter")
  
  Flow_cyt_data_compositional.m$parameter<-factor(Flow_cyt_data_compositional.m$parameter,
                                                  levels=c('CD41-CD235-','CD41-CD235+','CD41+CD235+','CD41+CD235-'),
                                                  ordered=T)
  
  
  cat("Flow_cyt_data_compositional.m_0\n")
  cat(str(Flow_cyt_data_compositional.m))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m$parameter)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m$parameter))))
  cat("\n")
  
  
  
  
  Flow_cyt_data_compositional.m.dt<-data.table(Flow_cyt_data_compositional.m, key=c("Sample","Genotype","Condition","Time"))
  
  Geometric_mean_Table<-as.data.frame(Flow_cyt_data_compositional.m.dt[,.(geom_mean=exp(mean(log(value)))),
                                                                     by=key(Flow_cyt_data_compositional.m.dt)], stringsAsFactors=F)
  
  cat("Geometric_mean_Table_0\n")
  cat(str(Geometric_mean_Table))
  cat("\n")
  
  Flow_cyt_data_compositional.m<-merge(Flow_cyt_data_compositional.m,
                                       Geometric_mean_Table,
                                       by=c("Sample","Genotype","Condition","Time"))
  
  Flow_cyt_data_compositional.m$scaled_value<-Flow_cyt_data_compositional.m$value/Flow_cyt_data_compositional.m$geom_mean
  
  cat("Flow_cyt_data_compositional.m_1\n")
  cat(str(Flow_cyt_data_compositional.m))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m$parameter)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m$parameter))))
  cat("\n")
  
  #### NEW PATH ----
  
  path5<-paste(out,'EXP_20062023','/', sep='')
  
  if (file.exists(path5)){
    
    
    
    
  } else {
    dir.create(file.path(path5))
    
  }
  
  setwd(path5)
  
  
  
  #### LOOP to ITERATE the GENOTYPES ----
  
  
  Genotypes_array<-levels(Flow_cyt_data_compositional.m$Genotype)
  
  Condition_DEBUG <- 1
  
  comparison<-data.frame()
  
  for(i in 1:length(Genotypes_array))
  {
    Genotype_sel<-Genotypes_array[i]
    
    cat("------------------>\t")
    cat(sprintf(as.character(Genotype_sel)))
    cat("\n")
    
    
    
    if(i > 1 & i<length(Genotypes_array))
    {
      comparison_terms_1<-c(Genotypes_array[1],Genotypes_array[i])
      comparison_string_1<-paste(comparison_terms_1, collapse="_")
      comparison_terms_2<-c(Genotypes_array[i],Genotypes_array[i+1])
      comparison_string_2<-paste(comparison_terms_2, collapse="_")
      
      
      comparison_DEF<-as.data.frame(rbind(cbind('1',comparison_string_1),
                                          cbind('2',comparison_string_2)), stringsAsFactors=F)
      
      
      
      colnames(comparison_DEF)<-c("iteration","comparison")
      
      cat("comparison_DEF_1\n")
      cat(str(comparison_DEF))
      cat("\n") 
      
      comparison<-rbind(comparison_DEF,comparison)    
      
      cat("comparison_0\n")
      cat(str(comparison))
      cat("\n")  
      
    }# i > 1 & i<length(Genotypes_array)
    if(i == length(Genotypes_array))
    {
      comparison_terms_3<-c(Genotypes_array[1],Genotypes_array[i])
      comparison_string_3<-paste(comparison_terms_3, collapse="_")
      
      comparison_DEF<-as.data.frame(cbind('3',comparison_string_3), stringsAsFactors=F)
      
      colnames(comparison_DEF)<-c("iteration","comparison")
      
      cat("comparison_DEF_0\n")
      cat(str(comparison_DEF))
      cat("\n") 
      
      comparison<-rbind(comparison_DEF,comparison)     
      
      cat("comparison_0\n")
      cat(str(comparison))
      cat("\n")  
      
      
    }#i == length(Genotypes_array)
    
  }#i in 1:length(Genotypes_array)
  
  cat("comparison_1\n")
  cat(str(comparison))
  cat("\n")  
  
  for(i in 1:dim(comparison)[1])  
  {
    comparison_string_sel<-comparison$comparison[i]
    
    cat("----------------comparison_string_sel---------------------->\t")
    cat(sprintf(as.character(comparison_string_sel)))
    cat("\n")  
    
    comparison_pair<-unlist(strsplit(comparison_string_sel, split="_"))   
    
    # cat("comparison_pair_0\n")
    # cat(str(comparison_pair))
    # cat("\n") 
    
    Flow_cyt_data_compositional.m_sel<-droplevels(Flow_cyt_data_compositional.m[which(Flow_cyt_data_compositional.m$Genotype%in%comparison_pair),])
    
    # cat("Flow_cyt_data_compositional.m_sel_0\n")
    # cat(str(Flow_cyt_data_compositional.m_sel))
    # cat("\n")
    
    
    # cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m_sel$Genotype)))))
    # cat("\n")
    # cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m_sel$Genotype))))
    # cat("\n")
    # cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m_sel$Time)))))
    # cat("\n")
    # cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m_sel$Time))))
    # cat("\n")
    # cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m_sel$Condition)))))
    # cat("\n")
    # cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m_sel$Condition))))
    # cat("\n")
    
    
    Condition_array<-levels(Flow_cyt_data_compositional.m_sel$Condition)
    
    # cat("Condition_array\n")
    # cat(str(Condition_array))
    # cat("\n")
    
    for(j in 1:length(Condition_array))
    {
      
      Condition_array_sel<-Condition_array[j]
      
      cat("-condition-->\t")
      cat(sprintf(as.character(Condition_array_sel)))
      cat("\n")
      
      Flow_cyt_data_compositional.m_sel_condition_sel<-droplevels(Flow_cyt_data_compositional.m_sel[which(Flow_cyt_data_compositional.m_sel$Condition == Condition_array_sel),])

     
      
      Flow_cyt_data_compositional.m_sel_condition_sel$parameter_2<-revalue(Flow_cyt_data_compositional.m_sel_condition_sel$parameter, 
                                    c('CD41-CD235-'='Double_negative',
                                      'CD41-CD235+'='CD235_Pos',
                                      'CD41+CD235+'='Double_positive',
                                      'CD41+CD235-'='CD41_Pos'))
      
      Flow_cyt_data_compositional.m_sel_condition_sel$parameter_2<-factor(Flow_cyt_data_compositional.m_sel_condition_sel$parameter_2,
                                                      levels=c('Double_negative','CD235_Pos','Double_positive','CD41_Pos'),
                                                      ordered=T)
      
      cat("Flow_cyt_data_compositional.m_sel_condition_sel_0\n")
      cat(str(Flow_cyt_data_compositional.m_sel_condition_sel))
      cat("\n")
      cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m_sel_condition_sel$Genotype)))))
      cat("\n")
      cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m_sel_condition_sel$Genotype))))
      cat("\n")
      cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m_sel_condition_sel$Time)))))
      cat("\n")
      cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m_sel_condition_sel$Time))))
      cat("\n")
      cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m_sel_condition_sel$Condition)))))
      cat("\n")
      cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m_sel_condition_sel$Condition))))
      cat("\n")
      cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m_sel_condition_sel$parameter_2)))))
      cat("\n")
      cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m_sel_condition_sel$parameter_2))))
      cat("\n")
      
      
      Flow_cyt_data_compositional.m_sel_condition_sel_wide<-as.data.frame(pivot_wider(Flow_cyt_data_compositional.m_sel_condition_sel,
                                                                               id_cols=c(Sample,Genotype,Condition,Time),
                                                                               names_from=parameter_2,
                                                                               names_sep = "_",
                                                                               values_from=scaled_value), stringsAsFactors=F)
      cat("Flow_cyt_data_compositional.m_sel_condition_sel_wide_0\n")
      cat(str(Flow_cyt_data_compositional.m_sel_condition_sel_wide))
      cat("\n")
      cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m_sel_condition_sel_wide$Genotype)))))
      cat("\n")
      cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m_sel_condition_sel_wide$Genotype))))
      cat("\n")
      cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m_sel_condition_sel_wide$Time)))))
      cat("\n")
      cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m_sel_condition_sel_wide$Time))))
      cat("\n")
      cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m_sel_condition_sel_wide$Condition)))))
      cat("\n")
      cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m_sel_condition_sel_wide$Condition))))
      cat("\n")
      
      path6<-paste(path5,"CD41_CD235_compositional_analysis",'/', sep='')
      
      if (file.exists(path6)){
        
        
        
        
      } else {
        dir.create(file.path(path6))
        
      }
      
      #### full model ----
      
      full_lm<-lm(cbind(Double_negative,CD235_Pos,Double_positive,CD41_Pos) ~ Genotype + as.numeric(Time) + Genotype:as.numeric(Time), data=Flow_cyt_data_compositional.m_sel_condition_sel_wide)
      
      
      Summary_full_model<-summary(full_lm)
      
      Anova_full_model<-Anova(full_lm)
      
      cat("Anova_full_model\n")
      cat(str(Anova_full_model))
      cat("\n")
      
      
      
      
      # quit(status = 1)
      
      
      # coefficients_full<-Summary_full_model$coefficients
      # 
      # cat("coefficients_full\n")
      # cat(str(coefficients_full))
      # cat("\n")
      
      intermediate_lm<-lm(cbind(Double_negative,CD235_Pos,Double_positive,CD41_Pos) ~ Genotype + as.numeric(Time), data=Flow_cyt_data_compositional.m_sel_condition_sel_wide)
      
      
      Summary_intermediate_model<-summary(intermediate_lm)
      
      # cat("Summary_intermediate_model\n")
      # cat(str(Summary_intermediate_model))
      # cat("\n")
      
      # coefficients_intermediate<-Summary_intermediate_model$coefficients
      # 
      # cat("coefficients_intermediate\n")
      # cat(str(coefficients_intermediate))
      # cat("\n")
      # 
      reduced_lm<-lm(cbind(Double_negative,CD235_Pos,Double_positive,CD41_Pos) ~ as.numeric(Time), data=Flow_cyt_data_compositional.m_sel_condition_sel_wide)
      
      
      Summary_reduced_model<-summary(reduced_lm)
      
      # cat("Summary_reduced_model\n")
      # cat(str(Summary_reduced_model))
      # cat("\n")
      
      
     
      
      LRT_model_full<-anova(intermediate_lm,full_lm)
      
      cat("LRT_model_full\n")
      cat(str(LRT_model_full))
      cat("\n")
      
      
      LRT_model_intermediate<-anova(reduced_lm,intermediate_lm)
      
      cat("LRT_model_intermediate\n")
      cat(str(LRT_model_intermediate))
      cat("\n")
      
     
      setwd(path6)
      
      write.table(LRT_model_full, file=paste("LRT_model_full_",Condition_array_sel,'_',comparison_string_sel,".tsv",sep=''), sep="\t", quote=F)
      
      
      write.table(LRT_model_intermediate, file=paste("LRT_model_intermediate_",Condition_array_sel,'_',comparison_string_sel,".tsv",sep=''), sep="\t", quote=F)
      
      
      
      saveRDS(Anova_full_model, file=paste("Anova_full_model_",Condition_array_sel,'_',comparison_string_sel,".rds",sep=''))
      
      
      
      
    
    }# j in 1:length(Condition_array)
    
  }#i in 1:dim(comparison_DEF)[1]
  
  #### Save the long matrix to do the stack barplot in a separate function----
  
  setwd(path6)
  
  write.table(Flow_cyt_data_compositional.m, 
              file=paste("Long_matrix_with_scaled_values",".tsv",sep=''), sep="\t", quote=F, row.names=F)
  saveRDS(Flow_cyt_data_compositional.m,file=paste("Long_matrix_with_scaled_values",".rds",sep=''))
}

graph_compositional_data = function(option_list)
{
  suppressMessages(library("cowplot", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
  suppressMessages(library("ggforce", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
  suppressMessages(library("ggnewscale", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
  suppressMessages(library("scales", lib.loc="/nfs/team151/software/manuel_R_libs_4_1/"))
  library("RColorBrewer",lib.loc="/nfs/team151/software/manuel_R_libs_4_1/")
  
  
  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")
  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("OUT_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  
  #### Read tje long matrix ----
  
  path5<-paste(out,'EXP_20062023','/', sep='')
  
  if (file.exists(path5)){
    
    
    
    
  } else {
    dir.create(file.path(path5))
    
  }
  
  path6<-paste(path5,"CD41_CD235_compositional_analysis",'/', sep='')
  
  if (file.exists(path6)){
    
    
    
    
  } else {
    dir.create(file.path(path6))
    
  }
  
  setwd(path6)
  
  Flow_cyt_data_compositional.m<-readRDS(file=paste("Long_matrix_with_scaled_values",".rds",sep=''))
  
  cat("Flow_cyt_data_compositional.m_0\n")
  cat(str(Flow_cyt_data_compositional.m))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m$Genotype)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m$Genotype))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m$Time)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m$Time))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Flow_cyt_data_compositional.m$Condition)))))
  cat("\n")
  cat(sprintf(as.character(summary(Flow_cyt_data_compositional.m$Condition))))
  cat("\n")
  
  #### Graph
  
  breaks.Rank<-(seq(0,100,by=10))
  labels.Rank<-as.character(breaks.Rank)
  
  
  cat(sprintf(as.character(labels.Rank)))
  cat("\n")
  
  vector_colors<-brewer.pal(8,"Reds")[c(1,3,5,7)]
  
  cat("vector_colors_0\n")
  cat(str(vector_colors))
  cat("\n")
  
  
  

  
  compositional_graph<-ggplot(data=Flow_cyt_data_compositional.m,
                      aes(x=fct_reorder(Sample,as.numeric(Genotype)), y=value, fill=parameter)) +
    geom_bar(stat="identity",colour='black')+
    theme_classic()+
    theme(axis.title.y=element_text(size=18, family="sans"),
          axis.text.y=element_text(angle=0,size=14, color="black", family="sans"))+
    scale_y_continuous(name=paste("Percentage of cells",sep=" "),breaks=breaks.Rank,labels=labels.Rank,
                       limits=c(breaks.Rank[1],breaks.Rank[length(breaks.Rank)]+1))+
    scale_x_discrete(name=NULL, drop=F)+
    scale_fill_manual(values=vector_colors,drop=F)+
    ggeasy::easy_center_title()
  

  
  compositional_graph<-compositional_graph+
    theme_cowplot(font_size = 12)+
    facet_grid(Condition ~ Time)+
    theme(legend.position="bottom",legend.title=element_blank(), legend.text = element_text(size=10))+
    guides(fill=guide_legend(nrow=1,byrow=TRUE))+
    theme(axis.text.x=element_text(angle=90,size=8,hjust=1,color="black", family="sans"))
  
  setwd(path6)
  
  svglite(paste('CD41_CD235_stacked_barplot','.svg',sep=''), width = 8, height = 8)
  print(compositional_graph)
  dev.off()
  
}

printList = function(l, prefix = "    ") {
  list.df = data.frame(val_name = names(l), value = as.character(l))
  list_strs = apply(list.df, MARGIN = 1, FUN = function(x) { paste(x, collapse = " = ")})
  cat(paste(paste(paste0(prefix, list_strs), collapse = "\n"), "\n"))
}


#### main script ----

main = function() {
  cmd_line = commandArgs()
  cat("Command line:\n")
  cat(paste(gsub("--file=", "", cmd_line[4], fixed=T),
            paste(cmd_line[6:length(cmd_line)], collapse = " "),
            "\n\n"))
  option_list <- list(
    make_option(c("--Table_of_labels"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Flow_cyt_data"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--tracking_variants"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
       make_option(c("--type"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
        make_option(c("--out"), type="character", default=NULL, 
                metavar="filename", 
                help="Path to tab-separated input file listing regions to analyze. Required.")
  )
  parser = OptionParser(usage = "140__Rscript_v106.R
                        --subset type
                        --TranscriptEXP FILE.txt
                        --cadd FILE.txt
                        --ncboost FILE.txt
                        --type type
                        --out filename",
                        option_list = option_list)
  opt <<- parse_args(parser)
  
  Stats_function_non_compositional_data(opt)
  graph_non_compositional_data(opt)
  Stats_function_compositional_data(opt)
  graph_compositional_data(opt)
  
}


###########################################################################

system.time( main() )
