library(elastic)
library(RCurl)

q = '{
"query": {
"bool": {
"must": [
        {
          "exists": {
              "field": "target"
          }
        }
      ]
    }
  }
}'

connect(es_host = "13.59.27.188", es_port = 9200)

es <- "13.59.27.188:9200"

res <- Search(index = paste0("urls_",gsub("-","",as.character(Sys.Date()-1))), body=q , 
              type = 'news',source = paste("title","fullhtml",sep=",") ,scroll = "1m")
out <- res$hits$hits
hits <- 1
while(hits != 0){
  res <- scroll(scroll_id = res$`_scroll_id`)
  hits <- length(res$hits$hits)
  if(hits > 0)
    out <- c(out, res$hits$hits)
}

out_df <- do.call("smartbind",lapply(out,function(x) as.data.frame(x,stringsAsFactors=F)))