addLetLab <- function(let, letAsTitle = TRUE) {
     if (!is.null(let)) {
        
        if (letAsTitle) mtext(side = 3, adj = 0.05, let)
            else  mtext(side = 3, line = -1, adj = 0.1, paste0(let, ')'))
    }
} 
   
