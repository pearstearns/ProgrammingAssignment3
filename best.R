best <- function(state, outcome) {
        out1.1 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        if(state %in% unique(out1.1$State)) {              
                if(outcome == "heart attack") {
                        cn <- 11
                } else if(outcome == "heart failure") {
                        cn <- 17
                } else if(outcome == "pneumonia") {
                        cn <- 23
                } else {
                        stop("invalid outcome")
                }  
                out1.2 <- out1.1[, c(2, 7, cn)]
                names(out1.2) <- c("Name", "State", "Outcome")
                na.omit(out1.2)
                out1.3 <- as.numeric(out1.2[[3]])
                out1.4 <- out1.2[ which(out1.2$State == state), ]
                out1.5 <- out1.4[order(out1.4$Outcome, out1.4$Name), ]
                as.character(out1.5[1,1])
        } else {
                stop("incorrect state")
        }
}