rankHospital <- function(state, outcome, num = "best") {
  
  out2.1 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  column_index <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        if (!(outcome %in% names(column_index))) {
          stop("invalid outcome")
        } else if (!(state %in% out2.1$State)) {
          stop("invalid state")
        }
  out2.2 <- out2.1[c(2,7, column_index[outcome])]
        names(out2.2) <- c("Name", "State", "Outcome")
  out2.2 <- na.omit(out2.2)
  out2.3 <- out2.2[ which(out2.2$State == state), ]
  out2.4 <- out2.3[order(out2.3$Outcome, out2.3$Name), ]
       if (num == "best") {
               num = 1
       } else if (num == "worst") {
               worst <- num <- nrow(out2.4)
       }
        Rank <- 1:nrow(out2.4)
  out2.5 <- cbind(out2.4, Rank)
        as.character(out2.5[num, 1])
}
