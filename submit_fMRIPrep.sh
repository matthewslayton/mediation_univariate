#!/bin/sh

#the executable line at the top was repeated here so I just deleted it.

EXPERIMENT=NetTMS.01

# this is the path to all subjects whose data have been sorted into BIDS (i.e., fMRIprep-ready)
subjectPath=/mnt/munin2/Simon/NetTMS.01/Analysis/fMRIprep/BIDS/NetTMS01
subID=($(ls $subjectPath))
for del in ${subID[@]} # remove the items in the list that do not look like BIDS folders
do
	if ! [[ $del =~ sub-.* ]]; then
		subID=("${subID[@]/$del}")
		echo "$del is not a BIDS folder and will be ignored"
	fi
done

echo -e '\n'
echo ${subID[*]}
echo -e '\n'

# this is the path to all subjects whose data have been preprocessed
processedData=/mnt/munin2/Simon/NetTMS.01/Data/Processed_Data/fmriprep_out
processedID=($(ls $processedData))
for del in ${processedID[@]} # remove the items in the list that do not look like fMRIprep output folders
do
	if ! [[ $del =~ sub-.* ]]; then
		processedID=("${processedID[@]/$del}")
	fi
done
for del in ${processedID[@]} 
do
	if [[ $del =~ .*html ]]; then
		processedID=("${processedID[@]/$del}")
	fi
done

# remove the preprocessed subjects from the subject list
for del in ${processedID[@]}
do
	subID=("${subID[@]/$del}")
	echo -e "${del} is already preprocessed"
done

echo -e '\n'
echo "begin job submission"

# for subjects who have BIDS-formatted data but have not been preprocessed yet:
# submit them to fMRIprep pipeline
for i in ${subID[@]}
do
	SUB=$i
	echo $SUB
		
	timeStamp="$(date)"
	qsub -v EXPERIMENT=${EXPERIMENT} z
	echo -e "\n processed ${SUB} ${timeStamp}" >> processing_record.txt

done
