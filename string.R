#String Basics
string1 = "Don't let what you can't do interfere with what you can do."
string2 = "\"Don't let what you can't do interfere with what you can do.\" - John Wooden"
writeLines(string1)

string3 = c("IF","YOU","GET","GIVE,", "IF","YOU","LEARN","TEAcH", "-MAYA", "ANGELOU")
string3

#stringr functions
str_length(string3)

#Collapse Strings... put them together
str_c(string3)
#Collapse Strings... put them together
str_c(string3, collapse = "")
#Collapse Strings... put them together
str_c(string3, collapse = ",")
#Collapse Strings... put them together
str_c(string3, collapse = " ")

string4 = str_c(string3, collapse = " ")
string4
str_sub(string4,1,1)
str_sub(string4,1,7)
str_sub(string4,-1,-1)
str_sub(string4,-14,-1)
str_sub(string4,3,7)

MSDS= c("M","S","D","S")
str_c(MSDS,collapse = "")