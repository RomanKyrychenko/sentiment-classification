Sys.setlocale(,"UK_ua")

library(shiny)
library(stringr)
library(dplyr)
library(tidytext)
library(RCurl)
library(text2vec)
library(gtools)
library(shinydashboard)
library(wordcloud)
library(readr)
library(xml2)
library(httr)
library(reticulate)
library(magrittr)
library(tidyverse)
library(reshape2)
library(widyr)
library(igraph)
library(ggraph)
library(FactoMineR)
library(factoextra)
library(foreign)

text.clean = function(x){ 
  require("tm")
  x  =  gsub("<.*?>", " ", x)
  x  =  removeNumbers(x)  
  x  =  removeWords(x, stopwords("english"))
  x  =  removeWords(x, stopwords("russian"))
  x  =  removeWords(x, stopwords)
  x  =  stemDocument(x,language="russian")
  x  =  stripWhitespace(x)                  
  x  =  gsub("^\\s+|\\s+$", "", x)  
  x  =  removePunctuation(x)
  return(x)
}

sentiword_ua_ru <- read_rds("sentiword_ua_ru.RDS")

tidysent <- function(x){
  x %>% 
    as_data_frame() %>% 
    mutate(id_text=c(1:length(x)),nwords=stringr::str_count(value,"\\S+"))  %>%  
    unnest_tokens(sentence,value,token = "sentences") %>% 
    group_by(id_text) %>% 
    mutate(id=c(1:n())) %>% 
    unnest_tokens(words,sentence,token = "ngrams", n = 3) %>% 
    separate(words,c("word1","word2","word3"),sep = " ") %>% 
    left_join(sentiword_ua_ru,by=c("word1"="words")) %>% 
    left_join(sentiword_ua_ru,by=c("word2"="words")) %>% 
    left_join(sentiword_ua_ru,by=c("word3"="words")) %>% 
    filter(!is.na(polarity.x)) %>% 
    mutate(polarity=polarity.x*ifelse(!is.na(polarity.y),polarity.y,1)*ifelse(!is.na(polarity.y)&!is.na(polarity),
                                                                              polarity,1)) %>% 
    select(id_text,id,nwords,word1, word2, word3, polarity) %>% 
    group_by(id_text) %>% 
    summarize(nwords = mean(nwords,na.rm=T), polarity=sum(polarity, na.rm=T)/(nwords+1))
}

sw <- data_frame(word=c("без", "більш", "більше", "буде", "начебто", "би", "був", "була", "були", "було", "бути", "вам", "вас", "адже", "увесь", "уздовж", "раптом", "замість", 
                        "поза", "униз", "унизу", "усередині", "в", "навколо", "от", "втім", "усі", "завжди", "усього", "усіх", "усю", "ви", "де", "так", "давай", "давати", "навіть", 
                        "для", "до", "досить", "інший", "його", "йому", "її", "її", "їй", "якщо", "є", "ще", "же", "за", "за винятком", "тут", "з", "через","або", "їм", "мати", 
                        "іноді", "їх", "якось", "хто", "коли", "крім", "хто", "куди", "чи", "або", "між", "мене", "мені", "багато", "може", "моє", "мої", "мій", "ми", "на", "назавжди",
                        "над", "треба", "нарешті", "нас", "наш", "не", "його", "ні", "небудь", "ніколи", "їм", "їх", "нічого", "але", "ну", "про", "однак", "він", "вона", "вони", 
                        "воно", "знову", "від", "тому", "дуже", "перед", "по", "під", "після", "потім", "тому", "тому що", "майже", "при", "про", "раз", "хіба", "свою", "себе", 
                        "сказати", "з", "зовсім", "так", "також", "такі", "такий", "там", "ті", "тебе", "тем", "тепер", "те", "тоді", "того", "теж", "тієї", "тільки", "тому", "той", 
                        "отут", "ти", "уже", "хоч", "хоча", "чого", "чогось", "чий", "чому", "через", "що", "щось", "щоб", "ледве", "чиє", "чия", "ця", "ці", "це", "цю", "цього", 
                        "цьому", "цей","і","у","та","я","а","й","як","які","бо","із","який","тим","нам","б","всі","ж","яку","зі","яких","всіх","цим","1997","1991","1992","1998","2008",
                        "2009","2010","2011","2012","2013","2014","2015","2016","2017","рік","все","роком","році","нехай","хай","року","яка","них","ним","1996","то","-","го","усім","має",
                        "яким","задля","буду","тих","всім","лише","вже","саме","звісно","1","т","3","17","это",
                        "щодо","модератор","александр","роман","игорь","любовь","татьяна","галина","геннадий",
                        "людмила","можна","гаразд","тобто","добре","зараз","карина","владимир","очень",
                        "очень","какие","почему","такое","самом","оно","скорее","вроде","цих","ніж","кроме","така",
                        "таки","свои","какое","таке","кем","кого","хтось","такая","ось","думаєте","10","0","20",
                        "кем","пока","тех","такие","нина","вообще","му","ваше","нє","своей","самое","5","взагалі",
                        "нему","чём","её","нём","друзі","почула","ваши","чим","моё","чему","якій","цієї","эта",
                        "марта","сергей","вопрос","вважаю","наприклад","например","ок","шо","хотя","такій","этих",
                        "тимур","света","типа","пожалуйста","неї","кому","гм","валентина","списибі","оксана",
                        "25","50","хочу","ребята","понимаете","кажуть","этим","который","понятно","далее","смотрите",
                        "которых","вибачте","десь","ага","скажите","дима","скажите","принципе","которая","якийсь",
                        "далі","ким","лет","говорят","такого","ч","деякі","нею","4","єслі","ше","неё","таких",
                        "років","которые","пусть","оці","года","91","колись","таку","столько","вами","ой","скажімо",
                        "вся","считаю","именно","всьо","оце","пані","тої","тіпа","якась","поэтому","якусь",
                        "наверное","бачу","пан","40","каких","само","какого","таком","думаю","скільки","кажу",
                        "знаете","сказав","30","яке","говорю","мере","такую","помню","сути","действительно","знаєте",
                        "якісь","2","олег","знаю","наталья","юлия","всё","артём","ещё","просто",tm::stopwords("ru")))

stopwords = unique(c("div", "href", "rel", "com", "relnofollow", "про", "что","dneprcity.net","novostimira.com","depo.ua","bbc.com",
                     "для", "relnofollow", "alt", "zero","img", "alignleft", "hspace", "vspace","studic.info","description","delo.ua",
                     "alignleft", "hspace", "vspace", "pbrp","altновини", "hrefhttpgsfmopinionhtml", "mode","strong", "pstrong", "targetblank",
                     "styletext-align", "justifi","altнапк", "classattachment-decosingl", "classreadmor","http:img.ura","pigua.info",
                     "ethereum", "hrefhttpresonanceua", "hrefhttpsinforesistorg","replyua.net","newsoboz.org","mc.async","manabalss.lv",
                     "hrefhttpblogiuaus", "hrefhttpvycherpnockua", "hrefhttpwwwkmugovuacontrolukpublisharticleartidampcatid","mid.ru",
                     "noneimg", "solid", "start", "stylefont-s", "stylefloat", "classfield-item","cackle.me","container","dtp.kiev.ua",
                     "classfield", "classfield-itemsdiv", "field-label-hiddendiv", "hrefhttpwwwaddthiscombookmarkphpv","replyua.netпро",
                     "srcbmimgcomuaberlinstoragefinancexebfcbeadfdeccjpg","cackle_widget.push","document.createelement","polytics","loading",
                     "eff", "i","classfield-item", "classfield", "classfield-itemsdiv", "field-label-hiddendiv", "datatyp","i.lb.ua",
                     "propertyrdfslabel", "datatyp", "propertyrdfslabel", "skospreflabel", "typeofskosconcept","mc.src","erve.ua","www.pravda.com.ua",
                     "reldcsubjecta", "classfield-tag", "field-type-taxonomy-term-refer","document.createelement","mc.type","lb.ua",
                     "classimagecach", "classlink", "classrtejustifyspan", "classtranslation-link", "classtranslationru","goo.gl","newsru.co.il",
                     "clearfixdiv", "data-colorbox-gallerygallery-node--ytzaljghei","document.getelementbyid","javascript","mon.gov.ua",
                     "propertyrdfslabel", "skospreflabel","этом","это","які","від","datatyp","-tag","function","innerhtml","sevastopolnews.info",
                     "hrefhttpkorupciyacom", "дуже", "там", "так", "але","span", "width", "classleftimgimg", "stylecolor", "stylefont-famili",
                     "hspace", "vspace", "clearal","classback-block","tabletrtd", "valigntop","document.location.protocol","zakarpatpost.net",
                     "hrefhttpwwwaddthiscombookmarkphpv", "even", "как", "titl","document.getelementsbytagname","text","true","sport.ua",
                     "sea", "black", "hold", "one","stylemargin", "color", "outlin", "pad", "none","nbsp","widget.js","www.unian.net",
                     "centerspan", "size", "stylefont-s", "font-siz", "divfont","s.nextsibling","s.parentnode.insertbefore","www.ukrinform.ru",
                     "justifi", "center", "width", "height", "classfeed-descriptionp","window.cackle_widget","xcoal","politics","newsru.co.il",
                     "pimg", "wp-post-imag", "margin", "sizesmax-width","justifystrong","joinfo.ua","1news","journal","unn.com.ua","newsonline24",
                     "srchttpimageunncomuaoriginaljpg", "altновости", "centerimg","styletextalign","stylefontsize","justify","fontsize","padding",
                     "helvetica","laquoР","raquo","httpnovostimiracomnewshtml","hrefhttprkrinuauploadspostsmatigeroyinyajpg","news_180574",
                     "hrefhttpskeletinfoorg","pem","leaders","hstrong","development","religious","targetblankstrong","politico","news_180572",
                     "che","glucosio","person","primarily","hrefhttpskeletinfoorg","classmetanav","clearall","newsoboz","politika","news_180571",
                     "stylefontfamily","arial","fontfamily","outline","sansserif","textalign","border","inherit","left","pspan","naviny.by",         
                     "justifyspan","rgb","styleboxsizing","small","googleadsectionend","womenbox.net","arianespace","polityka","porfyrios",
                     "classfielditem","classfielditemsdiv","fieldlabelhiddendiv", "also", "article", "Article","cellpadding","finanso.net",
                     "download", "google", "figure","fig", "groups","Google", "however","high", "human", "levels","alt","feed","image","src","http",
                     "jpg", "larger", "may", "number","class","новости","gazeta.ua","rossii.html","zn.ua","cellspacing","portal",
                     "shown", "study", "studies", "this","img","using", "two", "the", "Scholar","pubmedncbi", "PubMedNCBI","p","photocharles",
                     "view", "View", "the", "biol","div","via", "image", "doi", "one", "classbackblock","dubinsky.pro","posted","news_180560",
                     "analysis","nbspap","photocharl","dharapak","pimg", "srcbmimgcomuaberlinstoragenewsxabddbdbajpg", "alignleft", "nbspnbspnbsp", "href",                      
                     "fieldnamebody","fieldtypetextwithsummary","title","datatype","fieldtypeimage","typeoffoafimage","www.globallookpress.com",               
                     "classcolorbox","fieldnamefieldimage","fieldtypetaxonomytermreference","rdfsseealso","relogimage","evenidfirstp",
                     "sizesmaxwidth","wppostimage","sizedecosingle","styledisplayblock","classattachmentdecosingle","hrefhttpspolitekanet",                                 
                     "httpspolitekanetwpcontentuploadsefebimagexjpeg","httpspolitekanetwpcontentuploadsgettyimageskopiyaxjpg","replyua.net.так",
                     "httpspolitekanetwpcontentuploadsunianxjpg","httpspolitekanetwpcontentuploadsgettyimagesxjpg","joomla","slavdelo.dn.ua",
                     "classfielditems","odd", "classfielditem","fieldtypetext","stylewidth","classimage","classimageattachteaser","imageattachnode",              
                     "medium","tahomaarialhelveticasansserif","imagethumbnail","classimagecache","pxa", "fieldfieldnewslead","fieldfieldyandexnews",
                     "srchttpinmediakitemscachedddddbbecbesjpg","srchttpsgoroddpuapicnewsnewsimagesmjpg","srchttpwwwkanzasuauploadpicresizejpg",
                     "ampraquo","hrefhttpelvisticomnode","hrefhttpwwwgazetamistoteualajntranslyatsiyaforumudetsentralizatsiyaosnovaformuvannyanovoyisystemyupravlinnyatamistsevogorozvytku",
                     "forklog","hidden","hrefhttpgiuauseridurlhttpsaffnewstutbyfeconomicsfhtml","lineheight","overflow","colorspan", "pxbbrspan","sizeb",
                     "srcbmimgcomuaacommonimgicstarsemptyvioletgif","altimg","titleСЂР","hrefhttpvideobigmirnetuser","srchttpbmimgcomuavideoimgsxjpg", "table",
                     "mediumspan","sansarialsansserifspan","langruruspan","classfooterinfosocialitemfollows","classfooterinfosocialitemiconspan",
                     "hrefhttpwwwpravdacomuarusnews", "classfbcommentscount","що","который","которые","также","таким","новости","несмотря","в","на","и","не",
                     "что","у","о","по","с","про","за","з","до","і","що","экс","об","этом","из","к","от","та","для","екс","це","его","а","как","politeka","он",
                     "від","його","новости","это","твій","года","суті","при","під","после","того","через","будет","так","липня","том","уже","більше","як","все",
                     "но","щодо","які","которые","то","2017","против","также","далее","новостей","він","своей","також","еще","перед","который","є","под","сути",
                     "твой","знай","михаил","со","году","року","я","видео","проти","сил","із","сейчас","чтобы","dneprcity.net","же","й","может","над","или",
                     "стало","было","може","тому","буде","те","был","быть","между","фото","112","який","ua","только","чем","является","ли","более","21",
                     "26.07.17","без","було","вже","где","зі","ще","19","2018","laquo","мы","raquo","вона","кто","ми","своїй","ст","яких","были","ему","их",
                     "наших","него","особо","очень","себе","щоб","де","ее","зараз","котором","сам","своем","теперь","цього","але","если","которая","себя",
                     "тем","эти","https","больше","був","бути","два","навіщо","нас","таки",
                     "тогда","replyua.net","будут","йому","між","можно","ним","новость",
                     "п","ч","чи","12","50","mc","должен","им","именно","источник","которых","меньше","стал","эту","буду","має","стоит","требует","этот","11",
                     "http","была","высокий","день","имеет","її","мог","несколько","них","она","этого","вони","всего","вся","когда","которым","одним","почти",
                     "разі","стала","була","всі","вы","даже","дал","дать","кого","которое","м","нет","ни","новину","оборони","один","поки","потому","свою",
                     "стали","таким","этой","яка","якому","яку","script","the","var","ей","новини","одно","понад","проте","серед","такий","чому","вот","дати","есть",
                     "без", "більш", "більше", "буде", "начебто", "би", "був", "була", "були", "було", "бути", "вам", "вас", "адже", "увесь", "уздовж", "раптом", "замість", 
                     "поза", "униз", "унизу", "усередині", "в", "навколо", "от", "втім", "усі", "завжди", "усього", "усіх", "усю", "ви", "де", "так", "давай", "давати", "навіть", 
                     "для", "до", "досить", "інший", "його", "йому", "її", "її", "їй", "якщо", "є", "ще", "же", "за", "за винятком", "тут", "з", "через","або", "їм", "мати", 
                     "іноді", "їх", "якось", "хто", "коли", "крім", "хто", "куди", "чи", "або", "між", "мене", "мені", "багато", "може", "моє", "мої", "мій", "ми", "на", "назавжди",
                     "над", "треба", "нарешті", "нас", "наш", "не", "його", "ні", "небудь", "ніколи", "їм", "їх", "нічого", "але", "ну", "про", "однак", "він", "вона", "вони", 
                     "воно", "знову", "від", "тому", "дуже", "перед", "по", "під", "після", "потім", "тому", "тому що", "майже", "при", "про", "раз", "хіба", "свою", "себе", 
                     "сказати", "з", "зовсім", "так", "також", "такі", "такий", "там", "ті", "тебе", "тем", "тепер", "те", "тоді", "того", "теж", "тієї", "тільки", "тому", "той", 
                     "отут", "ти", "уже", "хоч", "хоча", "чого", "чогось", "чий", "чому", "через", "що", "щось", "щоб", "ледве", "чиє", "чия", "ця", "ці", "це", "цю", "цього", 
                     "цьому", "цей","і","у","та","я","а","й","як","які","бо","із","який","тим","нам","б","всі","ж","яку","зі","яких","всіх","цим","1997","1991","1992","1998","2008",
                     "2009","2010","2011","2012","2013","2014","2015","2016","2017","рік","все","роком","році","нехай","хай","року","яка","них","ним","1996","то"))

ui <- dashboardPage(#skin = "blue",
                    title="Corestone ML",
                    dashboardHeader(
                      title="Corestone ML",
                      tags$li(class = "dropdown",
                              tags$a(href="http://corestone.expert/", target="_blank", 
                                     tags$img(height = "20px", alt="Corestone", src="http://corestone.expert/static/icons/ic-navbar-logo.svg")
                              )
                      ),
                      dropdownMenuOutput("sys"),
                      tags$li(class = "dropdown",
                              tags$a(href = "https://github.com/RomanKyrychenko",
                                     target = "_blank",
                                     tags$img(height = "20px", 
                                              src = "https://raw.githubusercontent.com/oraza/sectarianviolencePK/master/www/github.png")
                              )
                      )
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Topic modeling", tabName = "Infoflow", icon = icon("vcard-o")),
                        menuItem("Semantic analysis", tabName = "sys2", icon = icon("line-chart")),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        hr(),
                        menuItem("Documentation", icon = icon("file-text-o"), 
                                 href = "https://github.com/RomanKyrychenko/topicmodels/blob/master/README.md"),
                        menuItem("Feedback & suggestion", icon = icon("envelope-o"),
                                 href = "mailto:?Roman.Kyrychenko@corestone.expert?subject=Feedback on Corestone work tools app"),
                        menuItem("Source code", icon = icon("file-code-o"), 
                                 href = "https://github.com/RomanKyrychenko/topicmodels"),
                        menuItem("Fork me @ github", icon = icon("code-fork"), 
                                 href = "https://github.com/RomanKyrychenko") 
                      )
                    ),
                    dashboardBody(
                      tags$head(tags$style(HTML('
                        .skin-blue .main-header .navbar {
                            background-color: rgb(255, 255, 255);
                        }
                        .skin-blue .main-header .logo {
                            background-color: #fd0000;
                            color: #fff;
                            border-bottom: 0 solid transparent;
                        }
                        .skin-blue .main-header .navbar .sidebar-toggle {
                            color: #f00;
                        }
                        .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
                            color: #fff;
                            background: #1e282c;
                            border-left-color: #ff0000;
                        }
                        
                              .content-wrapper, .right-side {
                              background-image: url(http://www.elasperger.org/wp-content/uploads/2013/10/atkinson-says-goodbye-to-beloved-character-mr-bean1.jpg);
                        }
                        .well {
                            min-height: 20px;
                            padding: 19px;
                            margin-bottom: 20px;
                            background-color: rgba(255, 255, 255, 0);
                            border: 1px solid rgba(191, 29, 29, 0);
                            /* border-radius: 4px; */
                            -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
                            box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
                        }
                        '))),
                      tabItems(
                        
                        tabItem(tabName = "Infoflow",
                               # h2("Topic modeling"),
                                fluidPage(
                                  
                                  sidebarPanel(
                                    fileInput('file1', 'Завантажте файл з даними',
                                              accept = c(".xlsx",".xlsm")),
                                    numericInput("cluster","Кількість тем",200,min=2,max=5000),
                                    selectInput("var","Оберіть колонку з текстом","Текст","Текст"),
                                    selectInput("zag","Оберіть колонку із заголовком","Заголовок","Заголовок"),
                                    checkboxInput("ton", "Додати аналіз тональностей", FALSE),
                                    tags$hr(),
                                    downloadButton('down',"Завантажити результат")
                                  ),
                                  mainPanel(
                                    #plotOutput("cloud")
                                  )
                                )
                        ),
                        tabItem(tabName = "sys2",
                                fluidPage(
                                  
                                  sidebarPanel(
                                    fileInput('file2', 'Завантажте файл з даними',
                                              accept = c(".xlsx",".xlsm")),
                                    #numericInput("cluster","Кількість тем",200,min=2,max=5000),
                                    selectInput("var2","Оберіть колонку з текстом для аналізу","Текст","Текст"),
                                    selectInput("var3","Оберіть групуючу змінну","Текст","Текст"),
                                    textInput("txt","Введіть слова, які бажаєте додатково виключити",""),
                                    checkboxInput("sw", "Включити стандартний список стоп-слів", FALSE),
                                    tags$hr(),
                                    downloadButton('down2',"Завантажити результат")
                                  ),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Plot", plotOutput("cloud")),
                                                tabPanel("Summary", plotOutput("compcloud")),
                                                tabPanel("Summary", plotOutput("graph")),
                                                tabPanel("Table", tableOutput("corresp"))
                                    )
                                    
                                  )
                                )
                      )
                    )
))


server <- function(input,output,server,session){
  df2 <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""),col_names = T,sheet = 1)
  })
  df3 <- reactive({
    inFile <- input$file2
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""),col_names = T,sheet = 1)
  })
  observe({
    updateSelectInput(
      session,
      "var",
      choices=names(df2()),
      selected = "Текст")
  })
  observe({
    updateSelectInput(
      session,
      "zag",
      choices=names(df2()),
      selected = "Заголовок")
  })
  df <- reactive({
    tem <- df2()
    tem <- tem[!is.na(tem[input$var]),]
    sped <- data_frame(second=c(7,10,19,33,63,123,12,22,44,81,159,301),k=c(5,10,20,40,80,160,5,10,20,40,80,160),size=c(2997,2997,2997,2997,2997,2997,5722,5722,5722,5722,5722,5722))
    fit<-lm(second~k+size,data=sped)
    time <- predict(fit, data_frame(k=input$cluster,size=nrow(tem)), interval = "prediction")[1]
    progress <- Progress$new(session, min=1, max=21)
    on.exit(progress$close())
    k <- input$cluster 
    if(is.null(tem$URL_ID)) tem$URL_ID <- openssl::md5(tem[[input$var]])
    progress$set(message = 'Старт аналізу',
                 detail = 'Це займе деякий час...')
    it_train = itoken(text.clean(tem[[input$var]]), 
                      preprocessor = tolower, 
                      tokenizer = word_tokenizer,
                      ids = tem$URL_ID, 
                      progressbar = T)
    progress$set(value = 1, detail="Текст оброблено")
    vocab = create_vocabulary(it_train,stopwords = stopwords, ngram = c(1L, 3L))
    
    progress$set(value = 2, detail="Створено словник")
    pruned_vocab = prune_vocabulary(vocab, term_count_min = 2, 
                                    doc_proportion_max = 0.5, doc_proportion_min = 0.00001)
    progress$set(value = 3, detail="Словник скорочено")
    vectorizer = vocab_vectorizer(pruned_vocab)
    progress$set(value = 4, detail="Векторизовано")

    dtm_train  = create_dtm(it_train, vectorizer)
    #dtm_train  = normalize(dtm_train, "l2")
    progress$set(value = 5, detail="Створено матрицю слів-документів")
    lda_model = LDA$new(n_topics = k)
    progress$set(value = 6, detail="Задано параметри моделі")
    progress$set(value = 7, detail=paste("Обчислення моделі, час для приготування кави","(Орієнтовно",time%/%60,"хвилин",round(time%%60),"секунд)"))
    doc_topic_distr = 
      lda_model$fit_transform(x = dtm_train, n_iter = 1000, 
                              convergence_tol = 0.00001, n_check_convergence = 25,  
                              progressbar = T)
    progress$set(value = 19, detail="Модель створено!")
    gammaDF <- as_tibble(doc_topic_distr)
    names(gammaDF) <- c(1:k)
    
    toptopics <- tibble(URL_ID = attr(doc_topic_distr,"dimnames")[[1]],
                        Тема = as.numeric(apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))][1])),
                        `Рівень відповідності` = apply(gammaDF,1,max))
    
    tem <- left_join(tem,toptopics,by="URL_ID")
    zag <- as.character(input$zag)
    slov <- left_join((tem %>% 
            group_by(Тема) %>% 
            summarise(`Рівень відповідності`=max(`Рівень відповідності`)[1])),
            (tem %>% select(input$zag, `Рівень відповідності`,Тема)),by=c("Рівень відповідності","Тема")) 
    slov$theme2 <- slov[[input$zag]] 
    slov <- slov %>% select(`Рівень відповідності`,Тема,theme2) 
    tem <- left_join(tem,(slov[!duplicated(slov[,'Тема']),] %>% select(Тема,theme2)),by=c("Тема")) %>% 
      select(-Тема) %>% 
      rename("Тема"="theme2")
    progress$set(value = 20, detail="Масив створено, створення Excel")
    if(input$ton==T) {
      sent <- tidysent(tem[[input$var]])
      tem$Тональність <- sent$polarity
    }
    #tem %>% mutate(Тема=ifelse(`Рівень відповідності`<0.15,"Потребує додаткової класифікації",Тема))
    tem
  })
  output$cloud <- renderPlot({
    ss <- unnest_tokens(data_frame(txt=input$txt),word,txt)
    df3() %>%
      select(Текст) %>%
      unnest_tokens("word", Текст) %>%
      anti_join(sw,by="word") %>% 
      anti_join(ss,by="word") %>% 
      group_by(word) %>% 
      summarize(n=n()) %>%
      with(wordcloud(word,n,max.words = 1000))
  })
  output$graph <- renderPlot({
    bg <- df3() %>% 
      select(Текст) %>%  
      unnest_tokens(word, Текст, token = "ngrams", n=2) 
    bg$id <- c(1:nrow(bg)) 
    rw <- bg %>% unnest_tokens(w, word) %>% group_by(w) %>% summarize(n=n()) %>% filter(n>10)
    bg %>% unnest_tokens(w, word) %>% semi_join(rw,by="w") %>% pairwise_cor(w, id, sort = TRUE) %>%
      filter(correlation > .3) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()
  })
  output$coresp <- renderPlot({
    wds <- df3() %>% 
      unnest_tokens(word, Текст,token = "ngrams", n=2) %>% 
      group_by(word) %>% 
      summarize(n=n()) %>% 
      anti_join(sw,by="word") %>% 
      filter(n<10)
    
    pc <- df3() %>% 
      group_by(input$var3) %>% 
      unnest_tokens(word, Текст,token = "ngrams", n=2) %>% 
      anti_join(sw,by="word")%>% 
      anti_join(wds,by="word")
    
    crsp <- CA(table(pc$word,pc[[input$var3]]))
    fviz_ca_biplot(crsp, col.row="orange", col.col ="steelblue") +
      theme_minimal()
  })
  output$compcloud <- renderPlot({
    df3() %>%
      unnest_tokens(word, Текст) %>%
      anti_join(sw,by="word") %>% 
      group_by(word,input$var3) %>% 
      summarize(n=n()) %>%
      arrange(desc(n)) %>% 
      acast(word ~ input$var3, value.var = "n", fill = 0) %>%
      comparison.cloud(max.words = 1000,colors = mycolors, random.order=FALSE,c(4,0.1), title.size=1)
  })
  output$down <- downloadHandler(
    filename = function() {paste0("Topics_",input$cluster,"_",as.character(Sys.Date()), '.xlsx')},
    content = function(file) {
      xlsx::write.xlsx(as.data.frame(df()), file, sheetName = "Теми", row.names = FALSE)
    }
  )}

options(shiny.maxRequestSize=30*1024^2) 
options(shiny.trace=TRUE)
shinyApp(ui,server)