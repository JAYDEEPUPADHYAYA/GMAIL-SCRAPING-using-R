
# GMAIL SCRAPING using R


# SCRIPT

```
#install.packages("dplyr")
#install.packages("rvest")
#install.packages("RSelenium")
#install.packages("stringr")
#install.packages("zeallot")

library("dplyr")#pipe operator
library("rvest")#static webSCRAPING
library("RSelenium")#dynamic webSCRAPING
library("stringr")#str_trim ,str_to_title
library("zeallot")#multiple assignment of variables %<-%


g_s <- paste0("https://www.google.com/search?q=","gmail.com")

r_g_s <- read_html(g_s)


ext =
  r_g_s %>%
  html_nodes(".jfp3ef > a") %>%     
  html_attr(.,"href")


((grep("servicelogin",ext,value=T) %>% strsplit("="))[[1]][2] %>% strsplit("&sa"))[[1]][1] -> extract_loginpage 



rD <- rsDriver(port=4444L,browser="firefox")

remDr <- rD[["client"]]

remDr$navigate(extract_loginpage)

func_Web <- function(u_n,p_w){
  
  ##USERNAME
  uname_l  <-
    list("tag name" = "input"
         ,"name" = "identifier"
         ,"id"   = "identifierId"
         ,"css1"  = "#identifierId"
         ,"css2"  = "input[class  = 'whsOnd zHQkBf']")
  
  email <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "css",  value = uname_l[["css1"]]))
    }, error = function(e) stop("NOT ABLE TO FIND email Element") )
  
  email$sendKeysToElement(list(u_n))
  
  ##NEXT BUTTON
  Next_l <-
    list("id" = "identifierNext" 
         ,"css1" = "#identifierNext"
         ,"css2" =  "div[class = 'U26fgb O0WRkf zZhnYe e3Duub C0oVfc nDKKZc DL0QTb M9Bg4d']")
  
  Next <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "id",  value  = Next_l[["id"]]))
    }, error = function(e) stop("NOT ABLE TO FIND NEXT BUTTON ELEMENT OF USERNAME PAGE"))
  
  Next$sendKeysToElement(list(key="enter"))
  
  #......................................................
  
  Sys.sleep(5)
  
  ##PASSWORD
  password_l <- 
    list( "name"   = "password"
          ,"css1"  = ".I0VJ4d > div:nth-child(1) > input:nth-child(1)"
          ,"css2"  = ".zHQkBf"
          ,"css3"  = "#password > div.aCsJod.oJeWuf > div > div.Xb9hP > input"
          ,"tag"   = "input")
  
  password <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "name",  value  = password_l[["name"]]))
    }, error = function(e) stop("NOT ABLE TO FIND PASSWORD ELEMENT") )
  
  password$sendKeysToElement(list(p_w))
  
  ##NEXT BUTTON
  Next2_l <- 
    list("id" = "passwordNext" 
         ,"css" = "#passwordNext")
  
  Next2 <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "css",  value = Next2_l[["css"]]))
    }, error = function(e) stop("NOT ABLE TO FIND NEXT BUTTON ELEMENT OF PASSWORD PAGE") )
  
  Next2$sendKeysToElement(list(key="enter"))
  
  #......................................................
  
  Sys.sleep(5)
  
  ##NAME OF USERS
  name <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "tag",  value = "h1"))
    }, error = function(e) stop("NOT ABLE TO FIND ELEMENT FOR EXTRACTING USER NAME") )
  
  ((name$getElementText())[[1]][1] %>% 
      strsplit(","))[[1]][2] %>% 
    str_to_title(locale = "en") %>%
    str_trim() -> "Person_name"
    
  writeLines(paste0("NAME OF USER ",Person_name,"\n"))
  
  Sys.sleep(5)
  
  ##CLICK ON GOOGLE APP BUTTON
  g_app <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "css",  value  = "#gbwa > div:nth-child(1) > a:nth-child(1)"))
    }, error = function(e) stop("NOT ABLE TO FIND ELEMENT OF GOOGLE APP") )
  
  g_app$clickElement()
  
  Sys.sleep(5)
  
  ##CLICK ON GMAIL
  g_mail_l <- list("id" = "gb23","css1" = "#gb23")
  g_mail <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "css",  value = g_mail_l[["css1"]]))
    }, error = function(e) stop("NOT ABLE TO FIND ELEMENT OF GMAIL IN GOOGLE APP") )
  
  g_mail$clickElement()
  
  Sys.sleep(5)
  
  ##SWITCH TO NEXT WINDOW OF GMAIL INBOX
  next_window <- remDr$getWindowHandles()[[2]][1]
  remDr$switchToWindow(next_window)
  
  #.......................................................
  
  ##INBOX PAGE
  
  Sys.sleep(5)
  
  ##MOVE NAIVIGATE / SCROLL BAR DOWN BY 100 PIXELS OF LEFT MENU PANE
  nav <- tryCatch(
    { suppressMessages(
      remDr$findElement("css",'.ajl'))
    }, error = function(e) stop("NOT ABLE TO FIND SCROLL BAR ELEMENT OF LEFT MENU PANE"))
  
  remDr$executeScript("arguments[0].scrollTo(0,100)",args = list(nav))
  
  Sys.sleep(5)
  
  ##NUMBER OF DRAFT MAILS
  get_draft_value <- ""
  val <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "css",value = ".aHS-bnq > div:nth-child(2)"))
    }, error = function(e) stop("NOT ABLE TO FIND ELEMENT OF DRAFT"))
  
  (val$getElementText()[[1]][1] %>% strsplit(.,"\n") %>% unlist(.))[2] -> val
  get_draft_value <- ifelse(is.na(val),0,as.integer(val))
  
  writeLines(paste0("NUMBER OF DRAFT MAILS ",get_draft_value,"\n"))
  
  Sys.sleep(5)
  
  ##MOVE MOVE NAIVIGATE / SCROLL BAR DOWN BY 1000 PIXELS
  nav <- tryCatch(
    { suppressMessages(
      remDr$findElement("css",'.ajl'))
    }, error = function(e) stop("NOT ABLE TO FIND SCROLL BAR ELEMENT OF LEFT MENU PANE"))
  
  remDr$executeScript("arguments[0].scrollTo(0,1000)",args = list(nav))
  remDr$getTitle()
  
  Sys.sleep(5)
  
  ##CLICK ON MORE BUTTON
  more <- tryCatch(
    { suppressMessages(
      remDr$findElement("css",".CJ"))
    }, error = function(e) stop("NOT ABLE TO FIND ELEMENT OF MORE BUTTON"))
  
  more$clickElement()
  
  Sys.sleep(5)
  
  ##NUMBER OF SPAM MAIL
  get_spam_value <- ""
  val <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "css",value = ".aHS-bnv > div:nth-child(2)"))
    }, error = function(e) stop("NOT ABLE TO FIND ELEMENT OF SPAM MAILS"))
  
  (val$getElementText()[[1]][1] %>% strsplit(.,"\n") %>% unlist(.))[2] -> val
  get_spam_value = ifelse(is.na(val),0,as.integer(val))
  
  writeLines(paste0("NUMBER OF SPAM MAILS ",get_spam_value,"\n"))
  
  Sys.sleep(5)
  
  ##GET OUT OF INBOX PAGE (CLOSE WINDOW)
  googleAccount_button <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "css" , value = ".gb_Ea")) 
    }, error = function(e) stop("NOT ABLE TO FIND ELEMENT OF GOOGLE account button"))
  
  googleAccount_button $clickElement()
  
  signout_button <- remDr$findElement(using = "css" , value = "#gb_71")
  signout_button $clickElement()
  signout_button $closeWindow()
  
  ##COMING BACK TO PREVIOUS PAGE
  previous_window <- remDr$getWindowHandles()[[1]][1]
  remDr$switchToWindow(previous_window)
  remDr$getCurrentUrl()
  
  ##CLICK ON Google Account BUTTON
  googleAccount_button <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "css" , value = ".gb_Ea"))
    }, error = function(e) stop("NOT ABLE TO FIND GOOGLE ACCOUNT BUTTON ON GOOGLE APP PAGE"))
  
  googleAccount_button $clickElement()
  
  ##CLICK ON SIGNOUT BUTTON
  signout_button <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "css" , value = "#gb_71"))
    }, error = function(e) stop("NOT ABLE TO FIND ELEMENT OF SIGN-OUT BUTTON"))
  
  signout_button $clickElement()
}

func_Close <- function(){
  Sys.sleep(5)
  remDr$close()
}

func_Web("email id","password")
func_Close()
```
