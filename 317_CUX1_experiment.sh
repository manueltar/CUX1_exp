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
  
output="/nfs/users/nfs_m/mt19/Scripts/Wraper_scripts/316_CREATION.sh"

touch $output
echo -n "" > $output

echo "#!/bin/bash"  >> $output

#   ### stats_linear_mix_model

Rscript_stats_linear_mix_model=/nfs/users/nfs_m/mt19/Scripts/R/358_stats_on_flow_cyt.R

type=$(echo "stats_linear_mix_model")
outfile_stats_linear_mix_model=$(echo "$Log_files_path""outfile""_""$type"".out")
touch $outfile_stats_linear_mix_model
echo -n "" > $outfile_stats_linear_mix_model
name_stats_linear_mix_model=$(echo "$type""_job")

Flow_cyt_data=$(echo "/lustre/scratch126/humgen/teams/soranzo/users/mt19/CUX1_experiment/Stats_flow_CUX1.csv")


step_mem=$(expr $mem \* 1)
step_pc=$(expr $pc \* 1)


#	    echo "$mem""->""$step_mem"
#	    echo "$pc""->""$step_pc"



echo "bsub -G team151 -o $outfile_stats_linear_mix_model -M $step_mem -J $name_stats_linear_mix_model -R\"select[model==Intel_Platinum]\"  -R\"select[mem>=$step_mem] rusage[mem=$step_mem] span[hosts=1]\" -n$step_pc -q $queue -- \\" >> $output
echo "\"$Rscript $Rscript_stats_linear_mix_model \\" >> $output
echo "--Flow_cyt_data $Flow_cyt_data \\" >> $output
echo "--type $type --out $MASTER_ROUTE\"" >> $output



bash $output
exit



#####################################################LOOP CT ###################################################################################################################################################################################
#####################################################LOOP CT ###################################################################################################################################################################################
#####################################################LOOP CT ###################################################################################################################################################################################





b=($(echo "$SELECTED_CT" | tr "," '\n'))

declare -a arr1

for j  in "${b[@]}"
    do
        CT_sel=${j}

	echo "$CT_sel"

	CT_ROUTE=$(echo "$MASTER_ROUTE""$CT_sel""/")

#	echo "$CT_ROUTE"

	SELECTED_FDR=$(echo 'FDR0.1,FDR0.01,FDR0.001')	

	a=($(echo "$SELECTED_FDR" | tr "," '\n'))


        declare -a arr2
	
	for i  in "${a[@]}"
	do
            FDR_sel=${i}
	    

	    
    	    echo "$FDR_sel"

	    FDR_ROUTE=$(echo "$CT_ROUTE""$FDR_sel""/")

	    tag=$(echo "$CT_sel""_""$FDR_sel")

#	    echo "$tag"

	    #   ### Intersecting_variants_and_HiC_regions

	    Rscript_Intersecting_variants_and_HiC_regions="/nfs/users/nfs_m/mt19/Scripts/R/355_overlapper.R"

	    type=$(echo "Intersecting_variants_and_HiC_regions""_""$tag")
	    outfile_Intersecting_variants_and_HiC_regions=$(echo "$Log_files_path""outfile""_""$type"".out")
	    touch $outfile_Intersecting_variants_and_HiC_regions
	    echo -n "" > $outfile_Intersecting_variants_and_HiC_regions
            name_Intersecting_variants_and_HiC_regions=$(echo "$type""_job")


	    Input_rds=$(echo "/lustre/scratch126/humgen/teams/soranzo/users/mt19/HiC_Sahlen/conversion.rds")
	    


	    step_mem=$(expr $mem \* 1)
	    step_pc=$(expr $pc \* 1)


#	    echo "$mem""->""$step_mem"
#	    echo "$pc""->""$step_pc"



	    echo "bsub -G team151 -o $outfile_Intersecting_variants_and_HiC_regions -w\"done($name_stats_linear_mix_model)\" -M $step_mem -J $name_Intersecting_variants_and_HiC_regions -R\"select[model==Intel_Platinum]\"  -R\"select[mem>=$step_mem] rusage[mem=$step_mem] span[hosts=1]\" -n$step_pc -q $queue -- \\" >> $output
#	    echo "bsub -G team151 -o $outfile_Intersecting_variants_and_HiC_regions -M $step_mem -J $name_Intersecting_variants_and_HiC_regions -R\"select[model==Intel_Platinum]\"  -R\"select[mem>=$step_mem] rusage[mem=$step_mem] span[hosts=1]\" -n$step_pc -q $queue -- \\" >> $output
	    echo "\"$Rscript $Rscript_Intersecting_variants_and_HiC_regions \\" >> $output
	    echo "--Input_rds $Input_rds \\" >> $output
	    echo "--input_gene_names $input_gene_names \\" >> $output
    	    echo "--tag $tag \\" >> $output
	    echo "--type $type --out $MASTER_ROUTE\"" >> $output




	    if [ $FDR_sel == "FDR0.1" ]; then
		Intersecting_variants_and_HiC_regions_string=$(echo "done($name_Intersecting_variants_and_HiC_regions)")
	    else
		Intersecting_variants_and_HiC_regions_string=$(echo "&& done($name_Intersecting_variants_and_HiC_regions)")

	    fi


#	    echo "->>>$Intersecting_variants_and_HiC_regions_string"
	    arr2[${#arr2[@]}]="$Intersecting_variants_and_HiC_regions_string"


	done


	
	done_string=$(echo "\""""${arr2[@]}"""\"")
	echo "INTERMEDIATE_1:$done_string"

	arr2=()

	# # #### Rscript_PUT_TOGETHER ----

	Rscript_PUT_TOGETHER="/nfs/users/nfs_m/mt19/Scripts/R/356_Per_CT_summary.R"

	type=$(echo "PUT_TOGETHER""_""$CT_sel")

	outfile_PUT_TOGETHER=$(echo "$Log_files_path""outfile""_""$type"".out")
	touch $outfile_PUT_TOGETHER
	echo -n "" > $outfile_PUT_TOGETHER
	
	name_PUT_TOGETHER=$(echo "$type""_job")

	step_mem=$(expr $mem \* 1)
	step_pc=$(expr $pc \* 1)


#	echo "$mem""->""$step_mem"
#	echo "$pc""->""$step_pc"


	echo "bsub -G team151 -o $outfile_PUT_TOGETHER -M $step_mem -w$done_string -J $name_PUT_TOGETHER -R\"select[model==Intel_Platinum]\" -R\"select[mem>=$step_mem] rusage[mem=$step_mem] span[hosts=1]\" -n$step_pc -q $queue -- \\" >> $output
#	echo "bsub -G team151 -o $outfile_PUT_TOGETHER -M $step_mem -J $name_PUT_TOGETHER -R\"select[model==Intel_Platinum]\" -R\"select[mem>=$step_mem] rusage[mem=$step_mem] span[hosts=1]\" -n$step_pc -q $queue -- \\" >> $output
	echo "\"$Rscript $Rscript_PUT_TOGETHER \\" >> $output
	echo "--Cell_Type $CT_sel \\" >> $output
	echo "--type $type --out $MASTER_ROUTE\"" >> $output


        if [ $CT_sel == "K562" ]; then
		name_PUT_TOGETHER_string=$(echo "done($name_PUT_TOGETHER)")
        else
		name_PUT_TOGETHER_string=$(echo "&& done($name_PUT_TOGETHER)")

        fi

        echo "->>>$name_PUT_TOGETHER_string"
        arr1[${#arr1[@]}]="$name_PUT_TOGETHER_string"


done

done_string_DEF=$(echo "\""""${arr1[@]}"""\"")
echo "FINAL_DEF:------------------->$done_string_DEF"

	arr1=()
 
	# #### Rscript_FINAL_COALESCENCE ----

	Rscript_FINAL_COALESCENCE="/nfs/users/nfs_m/mt19/Scripts/R/357_ALL_CT_summary.R"

	type=$(echo "FINAL_COALESCENCE")

	outfile_FINAL_COALESCENCE=$(echo "$Log_files_path""outfile""_""$type"".out")
	touch $outfile_FINAL_COALESCENCE
	echo -n "" > $outfile_FINAL_COALESCENCE

	Input_rds=$(echo "/lustre/scratch126/humgen/teams/soranzo/users/mt19/HiC_Sahlen/conversion.rds")
	Supp4_Table_CURATED_PLUS_PHENOTYPES=$(echo "/lustre/scratch123/hgi/mdt1/teams/soranzo/projects/VARIANT_INTERPRETATION/OVERHAUL_INTERVAL/FINAL_RESULTS/Fig4_pannels/""Supp_Table_4_CURATED_Plus_phenotypes.rds")
	Fedes_variants=$(echo "/lustre/scratch126/humgen/teams/soranzo/users/mt19/HiC_Sahlen/Federicas_variants.csv")
	

	name_FINAL_COALESCENCE=$(echo "$type""_job")

	step_mem=$(expr $mem \* 1)
	step_pc=$(expr $pc \* 1)


	echo "$mem""->""$step_mem"
	echo "$pc""->""$step_pc"


	echo "bsub -G team151 -o $outfile_FINAL_COALESCENCE -M $step_mem -w$done_string_DEF -J $name_FINAL_COALESCENCE -R\"select[model==Intel_Platinum]\" -R\"select[mem>=$step_mem] rusage[mem=$step_mem] span[hosts=1]\" -n$step_pc -q $queue -- \\" >> $output
#	echo "bsub -G team151 -o $outfile_FINAL_COALESCENCE -M $step_mem -J $name_FINAL_COALESCENCE -R\"select[model==Intel_Platinum]\" -R\"select[mem>=$step_mem] rusage[mem=$step_mem] span[hosts=1]\" -n$step_pc -q $queue -- \\" >> $output
	echo "\"$Rscript $Rscript_FINAL_COALESCENCE \\" >> $output
	echo "--Supp4_Table_CURATED_PLUS_PHENOTYPES $Supp4_Table_CURATED_PLUS_PHENOTYPES \\" >> $output
	echo "--Fedes_variants $Fedes_variants \\" >> $output	
	echo "--Input_rds $Input_rds \\" >> $output
	echo "--SELECTED_CT $SELECTED_CT \\" >> $output
	echo "--type $type --out $MASTER_ROUTE\"" >> $output



bash $output

