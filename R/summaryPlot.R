summary.plot=function(DATA,x,y){
  simple = function(data , doc=0 , dep=0 , div=0 , person=0 , status=0 , month=0  ){
    ## 存成可用形式
    point=0
    data.reb = data[, c(which(colnames(data) == "doc") ,                #1
                        which(colnames(data) == "dep") ,                #2
                        which(colnames(data) == "div") ,                #3
                        which(colnames(data) == "person") ,             #4 
                        which(colnames(data) == "status") ,             #5
                        which(colnames(data) == "month") ,              #6           
                        which(colnames(data) == "個人處理時間") ,    #7
                        which(colnames(data) == "個人等待時間") ,       #8
                        which(colnames(data) == "個人總時間"),          #9
                        which(colnames(data) == "組別處理時間") ,    #10
                        which(colnames(data) == "組別等待時間") ,       #11
                        which(colnames(data) == "組別總時間"),          #12
                        which(colnames(data) == "處室處理時間") ,    #13
                        which(colnames(data) == "處室等待時間") ,       #14
                        which(colnames(data) == "處室總時間")         #15
                        
    )]
    ## 判斷哪些變數感興趣
    det.vec = c( doc , dep , div , person , status , month )
    if(sum(det.vec) > 1 ){
      intrest.col = unique(data.reb[,which(det.vec == 1)] , MARGIN=1)
    }
    if(sum(det.vec) == 1 ){
      intrest.col = matrix(0,length(unique(data.reb[,which(det.vec == 1)])),1)
      intrest.col[,1] = unique(data.reb[,which(det.vec == 1)])
      colnames(intrest.col) = colnames(data.reb)[which(det.vec == 1)]
    }
    
    ## output
    output.response = matrix(0 , length(intrest.col[,1]) , 4)
    intrest.name = colnames(data.reb)[which(det.vec == 1)]
    # output-time
    for(i in 1:length(intrest.col[,1])){
      tf = matrix(0,length(data.reb[,1]),6)   ## choose row
      
      for(j in 1:sum(det.vec)){
        tf[,which(colnames(data.reb) == colnames(intrest.col)[j])] = data.reb[,which(colnames(data.reb) == colnames(intrest.col)[j])] == intrest.col[i,j]
      }
      choose = tf[,which(det.vec != 0 )[1]] 
      if(sum(det.vec) > 1 ){
        for(j in 1:sum(det.vec)){
          choose = choose*tf[,which(det.vec != 0 )[j]] 
        }
      }
      
      if(person == 0  && div == 0 && dep !=0){
        point="dep"
        output.response[i,1] = sum(choose)
        output.response[i,2] = sum(as.numeric(data.reb[which(choose == 1),13]),na.rm = T)
        output.response[i,3] = sum(as.numeric(data.reb[which(choose == 1),14]),na.rm = T)
        output.response[i,4] = sum(as.numeric(data.reb[which(choose == 1),15]),na.rm = T)
        
      }else if(person == 0 && div != 0){
        point="div"
        output.response[i,1] = sum(choose)
        output.response[i,2] = sum(as.numeric(data.reb[which(choose == 1),10]),na.rm = T)
        output.response[i,3] = sum(as.numeric(data.reb[which(choose == 1),11]),na.rm = T)
        output.response[i,4] = sum(as.numeric(data.reb[which(choose == 1),12]),na.rm = T)
        
      }else{
        point="person"
        output.response[i,1] = sum(choose)
        output.response[i,2] = sum(as.numeric(data.reb[which(choose == 1),7]),na.rm = T)
        output.response[i,3] = sum(as.numeric(data.reb[which(choose == 1),8]),na.rm = T)
        output.response[i,4] = sum(as.numeric(data.reb[which(choose == 1),9]),na.rm = T)
        
      }
      
      
      
      colnames(output.response) = list("n.choose","processing_time","waiting_time"
                                       ,"total_time")
    }
    ### 計算 point辨理 的文件數 (同文件不重覆算)
    if(point != "doc" & point != 0){
      j = which(colnames(data.reb) == point) #尋找point 在data.reb中的位置
      num.po.1 = matrix(0 , length(unique(data.reb[,j])) , 2)
      num.po.1[,1] = unique(as.character(data.reb[,which(colnames(data.reb) == point)]))
      
      i = 1:length(unique(data.reb[,j]))
      num.po.1[,2] = sapply(i , function(i)length(
        unique(data.reb[which(data.reb[,j] == num.po.1[i,1]),1])) )
      colnames(num.po.1) = c(point , "num.po.1")
      
      ##計算 point 辨理 的次數數 (同文件會重覆算)
      j = which(colnames(data.reb) == point) #尋找point 在data.reb中的位置
      num.po.2 = matrix(0 , length(unique(data.reb[,j])) , 2)
      num.po.2[,1] = unique(as.character(data.reb[,which(colnames(data.reb) == point)]))
      
      i = 1:length(unique(data.reb[,j]))
      num.po.2[,2] = sapply(i , function(i)length(
        data.reb[which(data.reb[,j] == num.po.2[i,1]),1]) )
      colnames(num.po.2) = c(point , "num.po.2")
      
    }else{
      num.po.1 = matrix(1 , length(unique(data.reb[,j])) , 2)
      num.po.2 = matrix(1 , length(unique(data.reb[,j])) , 2)
    }
    
    
    
    
    num.po = cbind(num.po.1,num.po.2)
    output.final = cbind( intrest.col , output.response)
    
    return(list("output.final" = output.final , "num.po" = num.po))
    
  }
  
  #   waiting_time <- as.numeric(CELL$output.final[,"waiting_time"])
  #   dep <- CELL$output.final[, "dep"]
  #   div <- CELL$output.final[, "div"]
  ###清大vs處
  if(x=="國立清華大學"){
    if(y=="文"){
      CELL=simple(DATA,dep=1,div=0,person=0,doc=1,status=0,month=0)
      boxplot(log(as.numeric(CELL$output.final[,"waiting_time"]))~CELL$output.final[, "dep"],las = 2,main=paste(x,"(文)"))
      axis(4,at=c(-10:20),labels=round(exp(c(-10:20)),1))
    }else if(y=="人"){
      CELL=simple(DATA,dep=1,div=0,person=1,doc=0,status=0,month=0)
      boxplot(log(as.numeric(CELL$output.final[,"waiting_time"]))~CELL$output.final[, "dep"],las = 2,main=paste(x,"(人)"))
      axis(4,at=c(-10:20),labels=round(exp(c(-10:20)),1))
    }  
  } else if(length(which(DATA[, "dep"]==x)) != 0){ ###處vs組
    if(y=="文"){
      CELL=simple(DATA,dep=1,div=1,person=0,doc=1,status=0,month=0)
      NEW.CELL=CELL$output.final[which((CELL$output.final[, "dep"] == x) == 1),]
      boxplot(log(as.numeric(NEW.CELL[, "waiting_time"]))~as.character(NEW.CELL[, "div"]),las = 2,main=paste(x,"(文)"))
      axis(4,at=c(-10:20),labels=round(exp(c(-10:20)),1))
    } else if(y=="人"){
      CELL=simple(DATA,dep=1,div=1,person=1,doc=0,status=0,month=0)
      NEW.CELL=CELL$output.final[which((CELL$output.final[, "dep"] == x) == 1),]
      boxplot(log(as.numeric(NEW.CELL[, "waiting_time"]))~as.character(NEW.CELL[, "div"]),las = 2,main=paste(x,"(人)"))
      axis(4,at=c(-10:20),labels=round(exp(c(-10:20)),1))
    }
  } else if(length(which(DATA[, "div"]==x)) != 0){  ###組vs人
    if(y=="文"){
      CELL=simple(DATA,dep=0,div=1,person=1,doc=1,status=0,month=0)
      NEW.CELL=CELL$output.final[which((CELL$output.final[, "div"] == x) == 1),]
      boxplot(log(as.numeric(NEW.CELL[, "waiting_time"]))~as.character(NEW.CELL[, "person"]),las = 2,main=paste(x,"(文)"))
      axis(4,at=c(-10:20),labels=round(exp(c(-10:20)),1))
    } else if(y=="人"){
      print("格式錯誤,請重新輸入!")
    }
  }else{
    print("格式錯誤,請重新輸入!")
  }
  
}