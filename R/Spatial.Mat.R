Spatial.Mat <-
function (DM , inverse =FALSE, decay=1, rowstand = FALSE, bound = FALSE, bandlow = 0, bandupper, binary = FALSE, sqrt = FALSE) {
DM <- ifelse(DM==0, NA, DM) # Convert zero to NA such that the function can handle it
if (bound == TRUE){
DM <- ifelse(DM!= is.na(DM) & DM>=bandlow & DM<bandupper,DM,NA) # Create bound of size band
if (binary == TRUE){
DM <- ifelse(DM!= is.na(DM),1,DM) # Additionally convert bounded matrix into binary matrix
}
}
if (sqrt == TRUE) {
DM <- ifelse(DM!= is.na(DM), sqrt(DM), DM) # rowstandardized, apply: Take matrix Wf, 1= function will be applied over rows, sum = take the sum, ignore NA's
}
if (inverse == TRUE){
DM <- ifelse(DM!= is.na(DM), DM^decay,DM)  # Takes decay function form each non-NA element
DM <- ifelse(DM!= is.na(DM), 1/DM,DM) # inversts non-NA elements
}
if (rowstand == TRUE) {
DM <- DM/apply(DM,1,sum, na.rm= TRUE) # rowstandardized, apply: Take matrix Wf, 1= function will be applied over rows, sum = take the sum, ignore NA's
}
DM<- ifelse(is.na(DM), 0, DM) # converting NA back into zeros!
W <<- DM
return(W)
}
