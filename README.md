ProgAssignment3-data
====================

dd/mm/yyyy: 04/05/2014 Author: Josie.Tao R Programming on Coursera -- Programming Assignment 3

rankall function hint by myself

My function is doing well on part 8, however presents incorrect on part 9 & 10, because when num == "best" or num == numbers, it can get correct index on sorted data.frame, and at the beginning, I ignore this situation while num == "worst".

1. the wrong code at first :

if(num == "worst"){
                        num <- nrow(mydataSplit)
                  }

It is totally wrong for ignoring the "NA" value. Thus, when running at
as.character(mydataSplit[order(mydataSplit$heartAttack, mydataSplit$hospital)[num], "hospital"]), the results are totally different because the incorrect index of num, at the same time, the value of "num" is modified at 1st and then is fixed at the same value.

2. correct code as modified now:

if(num == "worst"){
                                boolVal <- is.na(mydataSplit$heartAttack)
                                # id is excluded NA
                                id <- nrow(mydataSplit[!boolVal,])
                                # rankhospital by "heart attack"
                                .......
                   }

So I finally got the correct index "id". 
