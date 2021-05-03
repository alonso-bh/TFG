print("holaaaa")

# test if there is at least one argument: if not, return an error
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n")
} else if (length(args)==1) {
  # default output file
  sprintf("El parámetro introducido es: %s", args[1])
}
