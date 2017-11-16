#!/bin/bash

cp lib/templatesubmit.htc lib/condorsubmit.htc

modelstring="${model_asc}${model}${model2}${model_aamatrix}${model_MLestimate}${model_finaltree}"

files="${phy_file} ${colweight_file} ${quartet_group_file} ${bipart_file} ${exclude_file} ${multi_constraint_tree} ${aa_submodel_file} ${mmodel_part_file} ${binary_tree_file} ${binmodel_file} ${secstructure_file} ${starttree_file}" 
files_noargs=`echo $files | sed 's/-[a-Z] //g'`
files_noargs=`echo $files_noargs | sed 's/ /,/g'`

echo arguments = -m $modelstring ${phy_file} ${colweight_file} ${quartet_group_file} ${bipart_file} ${exclude_file} ${multi_constraint_tree} ${aa_submodel_file} ${mmodel_part_file} ${binary_tree_file} ${binmodel_file} ${secstructure_file} ${starttree_file} ${algorithm} ${sec_structure_model} ${boot_seed} ${mrboot_tresh} ${num_rate_categories} ${tcic_verbose} ${random_starttree} ${ml_conv_crit} ${opt_prec} ${ML_tree_largemem} ${evo_placement_tresh} ${disable_pattern_comp} ${rearr_topo_changes} ${post_boot} ${intermediate_trees} ${maj_consensus} ${print_blength} ${multi_model} ${ictc_consensus} ${est_perpart} ${outgroup} ${disable_undertermined} ${random_seed} ${median_gamma} ${rate_het_disable} ${slid_win_size} ${rboot_seed} ${superf_pars} ${pars} ${num_alt_runs} ${mesquite} ${noseqcheck} ${no_bfgs} ${asc_corr} ${aut_prot_model} ${epa_keep_placement} ${epa_prob_tresh} ${epa_acc_tresh} ${jc69} ${k80} ${hky85} ${bootstop_perms} ${quart_no_replace} ${print_ident_seqs} >> lib/condorsubmit.htc
echo transfer_input_files = $files_noargs >> lib/condorsubmit.htc
echo queue >> lib/condorsubmit.htc

jobid=`condor_submit lib/condorsubmit.htc`
jobid=`echo $jobid | sed -e 's/Sub.*uster //'`
jobid=`echo $jobid | sed -e 's/\.//'`

#echo $jobid

#echo going to monitor job $jobid
condor_tail -f $jobid

exit 0
