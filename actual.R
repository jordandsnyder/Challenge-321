#Description
  #No more hiding from your alarm clock! You've decided you want your computer to keep you updated on 
  #the time so you're never late again. A talking clock takes a 24-hour time and translates it into words.
#Input Description
  #An hour (0-23) followed by a colon followed by the minute (0-59).
#Output Description
#The time in words, using 12-hour format followed by am or pm.

#Solution
 #Input
  UIinput <- function(){
    #Prompt
    x <- readline(prompt = "What time is it? Format hour (0-23): minute (0-59).")
    #Split String
    x <- as.numeric(unlist(strsplit(x, ":")))
    #Make it into a data frame
    x <- as.data.frame(t(as.data.frame(x)))
    colnames(x) <- c("Hour", "Minute")
    #Test to see if they are valid entries
    x$MinuteTest <- as.numeric(x$Minute)
    x$HourTest <- as.numeric(x$Hour)
    if(x$Hour<00 | x$Hour>23){
      stop("The value provided is not valid. Please try again")
      
    }
    if(x$Minute<00 | x$Minute>59){
      stop("The value provided is not valid. Please try again")
    }
    #Split up Minute
    x$Minute <- as.character(x$Minute)
    y <- t(as.numeric(unlist(strsplit(x$Minute, ""))))
    y <- as.data.frame(y)
    x$Minute <- as.numeric(x$Minute)
    #Create AM/PM
    x$AMPM <-ifelse(x$Hour<12, "AM", "PM")
    #Make hour language
    x$HourLang <-ifelse(x$Hour == 00, "twelve",
                        ifelse(x$Hour == 01 | x$Hour == 13, "one",
                               ifelse(x$Hour == 02 | x$Hour == 14, "two",
                                      ifelse(x$Hour == 03 | x$Hour == 15, "three",
                                             ifelse(x$Hour == 04 | x$Hour == 16, "four",
                                                    ifelse(x$Hour == 05 | x$Hour == 17, "five",
                                                           ifelse(x$Hour == 06 | x$Hour == 18, "six",
                                                                  ifelse(x$Hour == 07 | x$Hour == 19, "seven",
                                                                         ifelse(x$Hour == 08 | x$Hour == 20, "eight",
                                                                                ifelse(x$Hour == 09 | x$Hour == 21, "nine",
                                                                                       ifelse(x$Hour == 10 | x$Hour == 22, "ten",
                                                                                              ifelse(x$Hour == 11 | x$Hour == 23, "eleven",NA))))))))))))
    #Make minute under 19 Language!
    x$MinulteLang19 <- ifelse(x$Minute == 00, "",
                              ifelse(x$Minute == 01, "oh one",
                                     ifelse(x$Minute == 02, "oh two",
                                            ifelse(x$Minute == 03, "oh three",
                                                   ifelse(x$Minute == 04, "oh four",
                                                          ifelse(x$Minute == 05, "oh five",
                                                                 ifelse(x$Minute == 06, "oh six",
                                                                        ifelse(x$Minute == 07, "oh seven",
                                                                               ifelse(x$Minute == 08, "oh eight",
                                                                                      ifelse(x$Minute == 09, "oh nine",
                                                                                             ifelse(x$Minute == 10, "ten",
                                                                                                    ifelse(x$Minute == 11, "eleven",
                                                                                                           ifelse(x$Minute == 12, "twelve",
                                                                                                                  ifelse(x$Minute == 13, "thirteen",
                                                                                                                         ifelse(x$Minute == 14, "fourteen",
                                                                                                                                ifelse(x$Minute == 15, "fifteen",
                                                                                                                                       ifelse(x$Minute == 16, "sixteen",
                                                                                                                                              ifelse(x$Minute == 17, "seventeen",
                                                                                                                                                     ifelse(x$Minute == 18, "eightteen",
                                                                                                                                                            ifelse(x$Minute == 19, "nineteen",
                                                                                                                                                              NA))))))))))))))))))))
    #Make Minute over Ten Lang
    x$MinuteTen <- y$V1
    x$MinuteSingle <- ifelse(is.numeric(y$V2), y$V2, 0)
    x$MinuteTenLang <- ifelse(x$MinuteTen == 0, NA,
                              ifelse(x$MinuteTen == 1, NA,
                                     ifelse(x$MinuteTen == 2, "twenty",
                                            ifelse(x$MinuteTen == 3, "thirty",
                                                   ifelse(x$MinuteTen == 4, "forty",
                                                          ifelse(x$MinuteTen == 5, "fifty", NA))))))
    x$MinuteSingleLang <- ifelse(x$MinuteSingle == 0, "",
                                 ifelse(x$MinuteSingle == 1, "one",
                                        ifelse(x$MinuteSingle == 2, "two",
                                               ifelse(x$MinuteSingle == 3, "three",
                                                      ifelse(x$MinuteSingle == 4, "four",
                                                             ifelse(x$MinuteSingle == 5, "five", 
                                                                    ifelse(x$MinuteSingle == 6, "six",
                                                                           ifelse(x$MinuteSingle == 7, "seven",
                                                                                  ifelse(x$MinuteSingle == 8, "eight",
                                                                                         ifelse(x$MinuteSingle == 9, "nine",NA))))))))))
    #Combine them
    x$combo <- paste(x$MinuteTenLang,x$MinuteSingleLang, sep = " ")
    output <- paste("It's ", x$HourLang, ifelse(x$Minute<20, x$MinulteLang19, x$combo), x$AMPM)
    return(output)
           }
UIinput() 

