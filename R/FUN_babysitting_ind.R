# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# loop function retrieves group babysitting status database #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

bbstg.func_individuals = function(bbstg = NULL,  input = NULL) {

  # make new column for the group:
  input$bs_Y_N <- 0


  for(i in 1:length(input$date)) {

    print(i)
    #i = 30

    input_dat <- input[i, ]

    match1 <- which(as.character(bbstg$date) == as.character(input_dat$date))       # bbstg[which(1:nrow(bbstg) %in% match1), ]

    match2 <- which(bbstg$bs == input_dat$indiv)                                    # bbstg[which(1:nrow(bbstg) %in% match2), ]

    allmatches <- c(match1, match2)

    which_match1 <- allmatches[which(match1 %in% match2)]

    if(length(which_match1) > 0) {

      both_match_recs <- bbstg[which(1:nrow(bbstg) %in% which_match1), ]           # pups[which(1:nrow(pups) %in% which_match1),]

      for(j in 1:length(unique(both_match_recs$date))) {

        input$bs_Y_N [i] <- "Y"
      }
    }
    if(input$bs_Y_N[i] == 0) {

      input$bs_Y_N[i] <- "N"

    }
  }
  return(list(output = input))
}
