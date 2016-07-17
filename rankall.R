rankall <- function(outcome, num = "best") {
        out3.1 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        cin <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        if (!(outcome %in% names(cin))) {
                stop("invalid outcome")
        }
        out3.2 <- out3.1[c(2,7, cin)]
        names(out3.2) <- c("Name", "State", "Outcome")
        out3.3 <- out3.2[order(out3.2$Outcome, out3.2$Name),]
        out3.3 <- out3.3[complete.cases(out3.3),]
        out3.4 <- split(out3.3, out3.3$State)
        out3.5 <- lapply(out3.4, function(elt) elt[1,])
        as.data.frame(lapply(out3.5, '[', num))
}