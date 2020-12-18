##This contains various stats functions


st.err <- function(x){
  x.noNA <- x[!(is.na(x))]
  if(length(x.noNA) < length(x) ){warning("There were NAs")}
  y <- sd(x.noNA)/sqrt(length(x.noNA))
  return(y)
}

