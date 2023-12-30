replace_by_NA <- function(df,string){
    df[df == string] <- NA
}