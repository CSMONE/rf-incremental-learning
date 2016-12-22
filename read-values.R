fReadValues = function(fileName)
{
	ldfInputParameters = read.table(fileName, header = F, sep = "", comment.char = "#", stringsAsFactor = F)

	ldfInputParametersOrg = data.frame(matrix(nrow = 1, ncol = length(ldfInputParameters[,1])))
	ldfInputParametersOrg[1, ] = ldfInputParameters[,2]
	colnames(ldfInputParametersOrg) = ldfInputParameters[,1]

	assign("inputParameters", ldfInputParametersOrg, envir=.GlobalEnv) 
}
fReadValues(fileName = "info.txt")