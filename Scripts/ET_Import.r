
# This script is used to read in all the csv files in a folder.

library(doBy)

ET_Import.NoExpand = function(path_name,Timing,PlaceCodes){
list.files(path = path_name,full.names = T, pattern = ".xlsx") -> file_list

ET = c()
for (x in file_list){
	print(x)
	FullSubj = proc_subj.NoExpand(x,Timing,PlaceCodes)
	ET = rbind(ET,FullSubj)
	print(x)
	}
	return(ET)
}
