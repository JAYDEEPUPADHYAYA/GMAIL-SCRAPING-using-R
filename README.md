# GMAIL SCRAPING USING R

## **TOOLS / LIBRARIES**-  
1)Programming Language used for **WEB SCRAPING** is **R**.  
2)**LIBRARIES** used are **rvest** for **STATIC WEB SCRAPING** & **RSelenium** for **DYNAMIC WEB SCRAPING**.  
3)**BROWSER** used for WEB SCRAPING is **Firefox Developer Edition**, version is **69.0b1 (64-bit)**.   
4)**SELECTOR GADGET** of **Google Chrome** browser is used to find **CSS selector** of a desired element.  

## **DESCRIPTION**- 
1)Understaning **STATIC WEB SCRAPING** by scraping **URL** of **gmail service login** from **GOOGLE SEARCH WEB PAGE**.  
2)Understanding **DYNAMIC WEB SCRAPING** by scraping **USERNAME,NUMBER OF DRAFT MAILS & NUMBER OF SPAM MAILS** of **gmail account**.  

## **CODE**

 ```
library("xlsx")#read excel
library("zeallot")#multiple assignment of variables %<-%
library("rvest")#static webSCRAPING
library("dplyr")#pipe operator
library("RSelenium")#dynamic webSCRAPING
library("stringr")#str_trim ,str_to_title
library("rlist")#list.append
library("tidyr")#remove na from df

options(stringsAsFactors = FALSE)#set option  at the start to prevent string being converted to factor in dataframe
person_name_l <- list()
num_draft_mail <- list()
num_spam_mail <- list()
res <- data.frame(read.xlsx("Book1email.xlsx",1))
n_row <- nrow(res)#number of rows
locate_na <- which(is.na(res),arr.ind = T)
replace_na <- locate_na[,"row"]#locate row index where na is present
res1 <- res
res %>% drop_na() -> res
c(e,f) %<-% res 

#_________________________________________________________________

####static web SCRAPING

##gmail search url for gmail.com
l_g2 <- paste0("https://www.google.com/search?q=","gmail.com")

##read html page of that google search page
b <- read_html(l_g2)

##use nodes to get class
##use attr to extract attribute from the class
ext <-
  b %>%
  html_nodes(".jfp3ef > a") %>%    
  html_attr(.,"href")

##extract gmail service login url from attribute
((grep("servicelogin",ext,value=T) %>% strsplit("="))[[1]][2] %>% strsplit("&sa"))[[1]][1] ->extract_loginpage 

#______________________________________________________________

####DYNAMIC WEB SCRAPING

rD <- rsDriver(port=4567L,browser="firefox")
remDr <- rD[["client"]]
remDr$navigate(extract_loginpage)

func_Web <- function(i){
  
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
  
  email$sendKeysToElement(list(e[i]))
  
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
  
  password$sendKeysToElement(list(f[i]))
  
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
  person_name_l[[i]] <<- Person_name
  
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
  num_draft_mail[[i]] <<- as.integer(get_draft_value)
  
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
  num_spam_mail[[i]] <<- as.integer(get_spam_value)
  
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
    }, error = function(e) stop("ERROR NOT ABLE TO FIND GOOGLE ACCOUNT BUTTON ON GOOGLE APP PAGE"))
  
  googleAccount_button $clickElement()
  
  ##CLICK ON SIGNOUT BUTTON
  signout_button <- tryCatch(
    { suppressMessages(
      remDr$findElement(using = "css" , value = "#gb_71"))
    }, error = function(e) stop("ERROR NOT ABLE TO FIND ELEMENT OF SIGN-OUT BUTTON"))
  
  signout_button $clickElement()
}

str = "li.M8HEDc:nth-child(2) > div:nth-child(1)"
a  = for(i in seq(1,length(e))){
  if(i==1){
    func_Web(i)  
  }else{
    ##CLICK ON 2ND ,3RD ... USE ANOTHER ACCOUNT
    use_anoter_account <- tryCatch(
      { suppressMessages(
        remDr$findElement("css",str))
      },error = function(e) stop("NOTA BLE TO FIND OF USE ANOTHER ACCOUNT"))
    use_anoter_account$clickElement()
    func_Web(i)
    str = sub(as.character(i),as.character(i+1),str)
    Sys.sleep(5)
  }
}

##CLOSE CURRENT WINDOW

func_Web_Close <- function(){
  remDr$close()
}

##FORMATTING THE VALUES , APPENDING NA'S & VALUES TO EXCEL FILE

func_Change <- function(){
  t <- table(replace_na) %>%  names(.) %>% sapply(.,as.integer,USE.NAMES = F)
  
  l <- list(person_name_l , num_draft_mail , num_spam_mail)
  
  v <- c(n = list() , m = list() , p = list())
  
  for(i in seq(1,length(l))){
    temp <- 0
    v[[i]] <-
      unlist(list(rep(1,length(t)+length(person_name_l)))) %>% 
      replace(.,t,NA) %>% 
      sapply(., function(x) if(is.na(x)) {x} else {temp <<- temp+1 ;l[[i]][temp]})
  }
  
  c(person_name_l , num_draft_mail , num_spam_mail) %<-% v
  
  ##convert l1 to df & column bind with res1
  df_combine = data.frame(USERNAMES  = unlist(person_name_l)
                          ,NUMBER_DRAFT_MAILS = unlist(num_draft_mail)
                          ,NUMBER_SPAM_MAILS  = unlist( num_spam_mail)
  )%>% cbind(res1,.)
  
  ##FOR SECURITY REASONS & BACKUP WRITE THESE
  #write.xlsx(res1,file = "Book1email.xlsx",sheetName = "Sheet1",row.names = F,showNA = T)
  
  ##writing data into excel
  write.xlsx(df_combine,row.names =  F , file = "Book1email.xlsx",sheetName = "Sheet1")
}

func_Web_Close()
func_Change()
```
