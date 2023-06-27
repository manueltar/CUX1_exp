
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
library("lme4", lib.loc = "/nfs/team151/software/manuel_R_libs_4_1/")
library("afex", lib.loc = "/nfs/team151/software/manuel_R_libs_4_1/")



opt = NULL

#options(warn = 1)

Stats_function = function(option_list)
{
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
  
  Flow_cyt_data<-as.data.frame(fread(file=opt$Flow_cyt_data, sep=",") , stringsAsFactors=F)[,(1:9)]
  
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
  
 
  
  Flow_cyt_data<-rbind(Flow_cyt_data,Basal.df.addition_new_1_DEF)
  ##### Transform into ordered factors ----
  
  Flow_cyt_data$Genotype<-factor(Flow_cyt_data$Genotype,
                                 levels=c("WT","KI","Del"),
                                 ordered=T)
  
  Flow_cyt_data$Time<-factor(Flow_cyt_data$Time,
                                 levels=c("Basal","16","24","48","72"),
                                 ordered=T)
  
  Flow_cyt_data$Condition<-factor(Flow_cyt_data$Condition,
                                 levels=c("unst","PMA"),
                                 ordered=T)
  
  
  cat("Flow_cyt_data_1\n")
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
  
  # #####################
  # quit(status = 1)
 
  
  #### Summary table parameters----
  
  cols<-colnames(Flow_cyt_data)[5:dim(Flow_cyt_data)[2]]
  
  cat("cols_0\n")
  cat(str(cols))
  cat("\n")
  
  
  Flow_cyt_data.dt<-data.table(Flow_cyt_data, key=c("Genotype","Condition","Time"))
  
  
  
  Summary_table_parameters_mean <- as.data.frame(Flow_cyt_data.dt[, sapply(.SD, function(x) list(mean = mean(x))), .SDcols = cols, by = key(Flow_cyt_data.dt)], stringsAsFactors=F)
  
 
 
  
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
  
  Summary_table_parameters_sd <- as.data.frame(Flow_cyt_data.dt[, sapply(.SD, function(x) list(sd = sd(x))), .SDcols = cols, by = key(Flow_cyt_data.dt)], stringsAsFactors=F)
  
  
  
  
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
                                      levels=c('CD41+','CD41+CD235-','CD41+CD235+','CD41 MFI','CD41 GeoMFI'),
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
  
  saveRDS(Summary_table_DEF,file="Summary_table.rds")
  saveRDS(Flow_cyt_data,file="Flow_cyt_data_plus_BASAL.rds")
  
  
 
  #### LOOP to ITERATE the GENOTYPES ----
  
  
  Genotypes_array<-levels(Flow_cyt_data$Genotype)
  
  Condition_DEBUG <- 1
  
  
  for(i in 1:length(Genotypes_array))
  {
    Genotype_sel<-Genotypes_array[i]
    
    cat("------------------>\t")
    cat(sprintf(as.character(Genotype_sel)))
    cat("\n")
    
    if(i > 1)
    {
      comparison<-c(Genotypes_array[1],Genotypes_array[i])
      
      cat("--------------------------------------------------------comparison-------->\t")
      cat(sprintf(as.character(comparison)))
      cat("\n")
      
      comparison_string<-paste(comparison, collapse="_")
      
      Flow_cyt_data_sel<-droplevels(Flow_cyt_data[which(Flow_cyt_data$Genotype%in%comparison),])
      
      cat("Flow_cyt_data_sel_0\n")
      cat(str(Flow_cyt_data_sel))
      cat("\n")
      
      
      # cat(sprintf(as.character(names(summary(Flow_cyt_data_sel$Genotype)))))
      # cat("\n")
      # cat(sprintf(as.character(summary(Flow_cyt_data_sel$Genotype))))
      # cat("\n")
      # cat(sprintf(as.character(names(summary(Flow_cyt_data_sel$Time)))))
      # cat("\n")
      # cat(sprintf(as.character(summary(Flow_cyt_data_sel$Time))))
      # cat("\n")
      # cat(sprintf(as.character(names(summary(Flow_cyt_data_sel$Condition)))))
      # cat("\n")
      # cat(sprintf(as.character(summary(Flow_cyt_data_sel$Condition))))
      # cat("\n")
      
      
      Condition_array<-levels(Flow_cyt_data_sel$Condition)
      
      cat("Condition_array\n")
      cat(str(Condition_array))
      cat("\n")
      
      for(j in 1:length(Condition_array))
      {
        
        Condition_array_sel<-Condition_array[j]
        
        cat("-condition-->\t")
        cat(sprintf(as.character(Condition_array_sel)))
        cat("\n")
        
        Flow_cyt_data_sel_condition_sel<-droplevels(Flow_cyt_data_sel[which(Flow_cyt_data_sel$Condition == Condition_array_sel),])
        
        cat("Flow_cyt_data_sel_condition_sel_0\n")
        cat(str(Flow_cyt_data_sel_condition_sel))
        cat("\n")
        
        
        cat(sprintf(as.character(names(summary(Flow_cyt_data_sel_condition_sel$Genotype)))))
        cat("\n")
        cat(sprintf(as.character(summary(Flow_cyt_data_sel_condition_sel$Genotype))))
        cat("\n")
        cat(sprintf(as.character(names(summary(Flow_cyt_data_sel_condition_sel$Time)))))
        cat("\n")
        cat(sprintf(as.character(summary(Flow_cyt_data_sel_condition_sel$Time))))
        cat("\n")
        cat(sprintf(as.character(names(summary(Flow_cyt_data_sel_condition_sel$Condition)))))
        cat("\n")
        cat(sprintf(as.character(summary(Flow_cyt_data_sel_condition_sel$Condition))))
        cat("\n")
        
        for(k in 5:dim(Flow_cyt_data_sel_condition_sel)[2])
        {
          continuous_variable_sel<-colnames(Flow_cyt_data_sel_condition_sel)[k]
          
          cat("--->\t")
          cat(sprintf(as.character(continuous_variable_sel)))
          cat("\n")
          
          path6<-paste(path5,continuous_variable_sel,'/', sep='')
          
          if (file.exists(path6)){
            
            
            
            
          } else {
            dir.create(file.path(path6))
            
          }
          
          
          
          # indx.parameter_sel<-which(colnames(Flow_cyt_data_sel_condition_sel) == continuous_variable_sel)
          # 
          # 
          # cat("indx.parameter_sel\n")
          # cat(str(indx.parameter_sel))
          # cat("\n")
          
          
          Summary_table_DEF_sel<-droplevels(Summary_table_DEF[which(Summary_table_DEF$Parameter == continuous_variable_sel &
                                                                      Summary_table_DEF$Genotype%in%comparison &
                                                                      Summary_table_DEF$Condition == Condition_array_sel),])
          
          cat("Summary_table_DEF_sel_0\n")
          cat(str(Summary_table_DEF_sel))
          cat("\n")
          
          
          
          #### model CD41+CD235- ----
          
          full_model<-lmer(Flow_cyt_data_sel_condition_sel[,k] ~ 1 + Genotype:Time +
                             Genotype + Time +
                             (1 + Genotype|Sample), data= Flow_cyt_data_sel_condition_sel)
          
          check_full_model<-isSingular(full_model,  tol = 1e-4)
          
          # cat("check_full_model\n")
          # cat(str(check_full_model))
          # cat("\n")
          
          Summary_model<-summary(full_model)
          
          intermediate_model<-lmer(Flow_cyt_data_sel_condition_sel[,k] ~ 1 +
                                     Genotype + Time +
                                     (1 + Genotype|Sample) , data= Flow_cyt_data_sel_condition_sel)
          
          check_intermediate_model<-isSingular(intermediate_model,  tol = 1e-4)
          
          # cat("check_intermediate_model\n")
          # cat(str(check_intermediate_model))
          # cat("\n")
          
          Summary_model<-summary(intermediate_model)
          
          # cat("Summary_model\n")
          # cat(str(Summary_model))
          # cat("\n")
          
          reduced_model<-lmer(Flow_cyt_data_sel_condition_sel[,k] ~ 1 +
                                Time + 
                                (1 + Genotype|Sample), data= Flow_cyt_data_sel_condition_sel)
          
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
          
          write.table(LRT_model_full, file=paste("LRT_model_full_",Condition_array_sel,'_',comparison_string,".tsv",sep=''), sep="\t", quote=F)
          
          
          write.table(LRT_model_intermediate, file=paste("LRT_model_intermediate_",Condition_array_sel,'_',comparison_string,".tsv",sep=''), sep="\t", quote=F)
          
          coefficients<-Summary_model$coefficients
          
          # cat("coefficients\n")
          # cat(str(coefficients))
          # cat("\n")
          
          write.table(coefficients, file=paste("coefficients_",Condition_array_sel,'_',comparison_string,".tsv",sep=''), sep="\t", quote=F)
          
          
          # #######################################
          # quit(status = 1)
          
         
        }# k in 5:dim(Flow_cyt_data_sel_condition_sel)[2]
      }# j in 1:length(Condition_array)
    }# don't select 'WT' genotype is the reference
  }#i in 1:length(Genotypes_array)
  

 
 
}

graph = function(option_list)
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
  
  Summary_table_DEF<-readRDS(file="Summary_table.rds")
  
  
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
  
  Flow_cyt_data<-readRDS(file="Flow_cyt_data_plus_BASAL.rds")
  
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
    
    graph<-ggplot(data=Flow_cyt_data, 
                  aes(x=Time, 
                      y=Flow_cyt_data[,indx.Parameter],
                      group=Genotype,
                      color=Genotype,
                      shape=Genotype)) +
      geom_jitter(size = 3,height = 0.05, width=0.1)+
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
      scale_y_continuous(name=paste(parameter_array_sel, collapse="_"),breaks=breaks.Rank,labels=labels.Rank, limits=c(breaks.Rank[1],breaks.Rank[length(breaks.Rank)]))+
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
  
  Stats_function(opt)
  graph(opt)
  
}


###########################################################################

system.time( main() )
