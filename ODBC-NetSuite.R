
# Loading Libraries
## Here are the changes --- 
install.packages("RODBC")
library("RODBC")

# Establishing ODBC Connection
conn <- odbcConnect(dsn = "Netsuite-ODBC", uid = "gaurav.kishore@bistasolutions.com", pwd = "Netsuite@123")

# Connection Testing
odbcGetInfo(conn)

#Checking available Data Sources
odbcDataSources()

# # Fetching Data from Netsuite Server
# # Using SQL query to fetch data
# trans_lines <- RODBC::sqlQuery(
#          channel = conn,
#            query = "SELECT * FROM TRANSACTION_LINES")
# 
# trans_lines_filter 
# bb<- RODBC::sqlQuery(
#   channel = conn,
#   query = "SELECT TRANSACTION_ID, ITEM_ID, ITEM_COUNT, AMOUNT, ITEM_UNIT_PRICE,subsidiary_id FROM TRANSACTION_LINES  ")
# 
# trans <- RODBC::sqlQuery(
#   channel = conn,
#   query = "SELECT * FROM TRANSACTIONS")

# trans_filter <- RODBC::sqlQuery(
#   channel = conn,
#   query = "SELECT TRANSACTION_ID, TRANID, TRANDATE, TRANSACTION_TYPE, LOCATION_ID, ENTITY_ID FROM TRANSACTIONS")


system.time(new1 <- RODBC::sqlQuery(
  channel = conn,
  query = "SELECT TM.TRANSACTION_ID, TM.TRANID, TM.TRANDATE, TM.TRANSACTION_TYPE, TM.LOCATION_ID, TM.ENTITY_ID, TF.ITEM_ID, TF.ITEM_COUNT, TF.AMOUNT, TF.ITEM_UNIT_PRICE FROM TRANSACTION_LINES TF LEFT JOIN TRANSACTIONS TM ON TF.TRANSACTION_ID = TM.TRANSACTION_ID WHERE TF.subsidiary_id = 54 AND TM.TRANDATE > '2016-07-29'")
  )

tab <- sqlTables(conn)

system.time(new1 <- RODBC::sqlQuery(
  channel = conn,
  query = "SELECT owner, table_name
  FROM dba_tables")
)


odbcClose(conn)

# importing data

netsuite <- read.csv("thatsit-new-ns-filtered.csv", header = T)
colnames(netsuite)
colnames(netsuite) <- c("internal_id", "transaction_no","date","trans_type","location_id","customer_id", 
                        "product_id", "quantity", "amount", "unit_price", "subs_id" )



#Calculating outliers
# xx <- quantile(x = netsuite$quantity)
# inter_qrange <-  IQR(xx)
# #
# # # Inner outlier
# inner_outlier <- xx[2] - 1.5*inter_qrange
# #
# # #outer outlier
# outer_outlier <- xx[4] + 1.5*inter_qrange
# #
# # # removing detected outlier
# ss <- netsuite
# ss<- ss[ss$quantity >= inner_outlier, ]
# gg<- ss[ss$quantity  <= outer_outlier, ]


# applying regression
reg <- lm(formula = quantity ~ location_id +date+customer_id, data = netsuite)
summary(reg)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- median(x)
  y[x > (qnt[2] + H)] <- median(x)
  y
}

grp_cust <- netsuite %>% group_by(customer_id, product_id, location_id) %>% mutate(out =as.numeric(remove_outliers(quantity, na.rm = T)))

#grp <- grp_cust %>% group_by(customer_id, product_id, location_id) %>% summarise(qty =sum(out))
# 
# mon <- data.frame("Item" = as.character(), "Location" = as.character(),"Name" = as.character(),"AutoArimaPF" = as.numeric(),
#                   "ETS_PF" = as.numeric(), "CrostonPF" = as.numeric(), "RWF_PF" = as.numeric(), 
#                   "MeanfPF" = as.numeric(), "BestFit" = as.numeric() , stringsAsFactors = F)
output1 <- NULL
#agg <- NULL
grp_cust$date <- as.Date(grp_cust$date, format = "%d-%m-%Y")
cust <- grp_cust[which(grp_cust$date <= "2016-08-31") , ]

uniq <- unique(cust$product_id)
nam <- unique(cust$customer_id)
lo <- unique(cust$location_id)
system.time(for(i in 1:length(uniq)){
  ds1 <- cust[cust$product_id == as.character(uniq[i]),]
  lo <-  unique(ds1$location_id)
  nam <- unique(ds1$customer_id)
  # for(j in 1:length(nam) ){
  #   ds2 <- ds1[ds1$customer_id == as.character(nam[j]),]
  #   lo <-  unique(ds2$location_id)
  for(k in 1:length(lo)){
    ds3 <- ds1[ds1$location_id == as.character(lo[k]), ]
  #     # dd.agg <- aggregate(conv..Qty.cad. ~ mo + yr, ds2, FUN = sum)
      # dd.agg$date <- as.Date(paste(dd.agg$yr, dd.agg$mo, "01" ,sep = "-", format = "%Y-%m-%d"))
      #dat <- data_by_time(Order_date = ds3$Date, Quantity = ds3$Quantity, date.format = "%Y-%m-%d", na.want = F)
      
      # qty.date <- dd.agg[,4]
      # qty.agg <- dd.agg[,3]
      
      #month <- data_by_time(Order_date = ds2$Date,Quantity = ds2$conv..Qty.cad., date.format = "%Y-%m-%d", na.want = F)
      data <- transform(ds3,month=as.numeric(format(as.Date(date),"%m")))
      data <- transform(data,year=as.numeric(format(as.Date(date),"%Y")))
      data$date1 <- as.yearmon(paste(data$year, data$month, sep = "-"))
      data$date1 <- as.Date(data$date1)
      
      # # if(length(data)>0){
      # 
      bymonth <- aggregate(cbind(quantity) ~  date1 + product_id + location_id,
                           data=data,FUN=sum)
      st <- min(bymonth$date1)
      ed <- as.Date("2016-08-31")
      dat2 <- data.frame(
        date1 = seq(st, ed, by ="month")
      )
       zz <- merge(bymonth, dat2 ,all = TRUE)
       zz$quantity[is.na(zz$quantity)] <- 0
      zz1 <- zz %>% group_by(product_id) %>% mutate(out =as.numeric(remove_outliers(quantity, na.rm = T)))

       myts <- ts(zz1$out, end = c(2016, 8), frequency = 12)

       aa <- auto.arima(myts)
       fcast_aa <- forecast(aa, h = 1)
       aa_PointFcast <- fcast_aa$mean
       acc_aa <- accuracy(f = fcast_aa)[, "RMSE"]

       rw <- rwf(x = myts, h = 1)
       rw_PointFcast <- rw$mean
       acc_rw <- accuracy(f = rw)[, "RMSE"]

       if(length(myts) > 3){
       et <- ets(myts)
       fcast_et <- forecast(et, h = 1)
       ets_PointFcast <- fcast_et[2]
       acc_et <- accuracy(f = fcast_et)[, "RMSE"]
       }else{
         ets_PointFcast <- Inf
         acc_et <- Inf
       }


       crst <- croston(myts, h = 1)
       crst_PointFcast <- crst$mean
       acc_crst <- accuracy(crst)[, "RMSE"]

       mf <- meanf(x = myts, h = 1)
       mf_PointFcast <- mf$mean
       acc_mf <- accuracy(mf)[, "RMSE"]

       if(acc_et != Inf ){
       best.fit <- ifelse(acc_aa < min(acc_rw , acc_mf , acc_et , acc_crst), "acc_aa", ifelse(acc_rw < min(acc_aa, acc_mf , acc_et, acc_crst), "acc_rw",
                                                                                                ifelse(acc_mf<min(acc_rw, acc_aa, acc_et, acc_crst), "acc_mf",
                                                                                                       ifelse(acc_et <min(acc_rw, acc_mf, acc_aa, acc_crst), "acc_et",
                                                                                                              "acc_crst"))))

       }else{
       best.fit <- ifelse(acc_aa < min(acc_rw , acc_mf ,acc_crst), "acc_aa", ifelse(acc_rw < min(acc_aa, acc_mf ,acc_crst), "acc_rw",
                                                                                     ifelse(acc_mf<min(acc_rw, acc_aa,acc_crst), "acc_mf","acc_crst")))

       }
      item <- as.character(uniq[i])
      #na <- (nam[j])
      loctn <- (lo[k])


      output1 = rbind(output1, data.frame(item,loctn, as.numeric(aa_PointFcast),
                                          as.numeric(ets_PointFcast), as.numeric(crst_PointFcast),as.numeric(rw_PointFcast),
                                         as.numeric(mf_PointFcast), best.fit, acc_rw , acc_mf , acc_et , acc_crst, acc_aa))

      #by = rbind(by, data.frame(bymonth))
      # agg = rbind(agg, data.frame(item , loctn,na, qty))
      # }else{
      #   return(inf)
      # }
      
   # }
  }
 }
)
colnames(output1) <-  c("Item","Location", "AA_Forecast", "ETS_Forecast",
                        "Croston_Forecast", "RWF_Forecast", "MeanF_Forecast", "Best_Fit", "acc_rw" , "acc_mf" , "acc_et" , "acc_crst", "acc_aa")
by <- NULL
