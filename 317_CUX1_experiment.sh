#!/bin/bash>
 
MASTER_ROUTE=$1
mem=$2
pc=$3
queue=$4






Log_files_path=$(echo "$MASTER_ROUTE""Log_files""/")

 
#rm -rf $Log_files_path
#mkdir -p $Log_files_path


#### Rscript
 
Rscript=/software/R-4.1.0/bin/Rscript
  
output="/nfs/users/nfs_m/mt19/Scripts/Wraper_scripts/317_CREATION.sh"

touch $output
echo -n "" > $output

echo "#!/bin/bash"  >> $output
 
# #   ### stats_linear_mix_model

# Rscript_stats_linear_mix_model=/nfs/users/nfs_m/mt19/Scripts/R/358_stats_on_flow_cyt.R

# type=$(echo "stats_linear_mix_model")
# outfile_stats_linear_mix_model=$(echo "$Log_files_path""outfile""_""$type"".out")
# touch $outfile_stats_linear_mix_model
# echo -n "" > $outfile_stats_linear_mix_model
# name_stats_linear_mix_model=$(echo "$type""_job")

# Flow_cyt_data=$(echo "/lustre/scratch126/humgen/teams/soranzo/users/mt19/CUX1_experiment/Stats_flow_CUX1.csv")


# step_mem=$(expr $mem \* 1)
# step_pc=$(expr $pc \* 1)


# #	    echo "$mem""->""$step_mem"
# #	    echo "$pc""->""$step_pc"



# echo "bsub -G team151 -o $outfile_stats_linear_mix_model -M $step_mem -J $name_stats_linear_mix_model -R\"select[model==Intel_Platinum]\"  -R\"select[mem>=$step_mem] rusage[mem=$step_mem] span[hosts=1]\" -n$step_pc -q $queue -- \\" >> $output
# echo "\"$Rscript $Rscript_stats_linear_mix_model \\" >> $output
# echo "--Flow_cyt_data $Flow_cyt_data \\" >> $output
# echo "--type $type --out $MASTER_ROUTE\"" >> $output

# #   ### qPCR_data

# Rscript_qPCR_data=/nfs/users/nfs_m/mt19/Scripts/R/359_qPCR.R

# type=$(echo "qPCR_data")
# outfile_qPCR_data=$(echo "$Log_files_path""outfile""_""$type"".out")
# touch $outfile_qPCR_data
# echo -n "" > $outfile_qPCR_data
# name_qPCR_data=$(echo "$type""_job")

# qPCR_data=$(echo "/lustre/scratch126/humgen/teams/soranzo/users/mt19/CUX1_experiment/Stats_qPCR.csv")
# REF_genes=$(echo "B2M,GAPDH")
# Interest_genes=$(echo 'CUXRI,CUX1p75,CUX1_siRNa_spe,CUX1p200,ITGB3,Betaglobin_A1')

# step_mem=$(expr $mem \* 1)
# step_pc=$(expr $pc \* 1)


# #	    echo "$mem""->""$step_mem"
# #	    echo "$pc""->""$step_pc"



# echo "bsub -G team151 -o $outfile_qPCR_data -M $step_mem -J $name_qPCR_data -R\"select[model==Intel_Platinum]\"  -R\"select[mem>=$step_mem] rusage[mem=$step_mem] span[hosts=1]\" -n$step_pc -q $queue -- \\" >> $output
# echo "\"$Rscript $Rscript_qPCR_data \\" >> $output
# echo "--REF_genes $REF_genes \\" >> $output
# echo "--Interest_genes $Interest_genes \\" >> $output
# echo "--qPCR_data $qPCR_data \\" >> $output
# echo "--type $type --out $MASTER_ROUTE\"" >> $output

#   ### stats_linear_mix_model_exp_20062023

Rscript_stats_linear_mix_model_exp_20062023=/nfs/users/nfs_m/mt19/Scripts/R/363_stats_flow_cyte_exp_20062023.R

type=$(echo "stats_linear_mix_model_exp_20062023")
outfile_stats_linear_mix_model_exp_20062023=$(echo "$Log_files_path""outfile""_""$type"".out")
touch $outfile_stats_linear_mix_model_exp_20062023
echo -n "" > $outfile_stats_linear_mix_model_exp_20062023
name_stats_linear_mix_model_exp_20062023=$(echo "$type""_job")

Flow_cyt_data=$(echo "/lustre/scratch126/humgen/teams/soranzo/users/mt19/CUX1_experiment/EXP_20062023/FlowCyt_results_for_model_EXPERIMENT_2006_2023.csv")


step_mem=$(expr $mem \* 1)
step_pc=$(expr $pc \* 1)

 
#	    echo "$mem""->""$step_mem"
#	    echo "$pc""->""$step_pc"



echo "bsub -G team151 -o $outfile_stats_linear_mix_model_exp_20062023 -M $step_mem -J $name_stats_linear_mix_model_exp_20062023 -R\"select[model==Intel_Platinum]\"  -R\"select[mem>=$step_mem] rusage[mem=$step_mem] span[hosts=1]\" -n$step_pc -q $queue -- \\" >> $output
echo "\"$Rscript $Rscript_stats_linear_mix_model_exp_20062023 \\" >> $output
echo "--Flow_cyt_data $Flow_cyt_data \\" >> $output
echo "--type $type --out $MASTER_ROUTE\"" >> $output


bash $output
exit


