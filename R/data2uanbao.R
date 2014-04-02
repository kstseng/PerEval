data2uanbao <- function(data){
  dataggyy = data
  #dataggyy = data[1:19979,-1]
  # dataggyy = data[19980:23893,-1]
  doc <- unique(as.character(dataggyy[,1]))
  i <- 1:length(doc)
  cc = as.character(dataggyy[, 8])
  tmp <- sapply(i, function(i){
    len <- sum(dataggyy[,1] == doc[i])
    j <- which(dataggyy[,1] == doc[i])
    m <- strsplit(cc[j[1]], "/")[[1]][2]
    return(rep(m, len))  
  })
  month <- unlist(tmp)
  
  dat <- cbind(dataggyy, month)
  
  ### 將日期轉換成新時間軸 (分鐘)
  #install.packages("timeDate")
  library("timeDate")
  NewTimeLine <- function(time,status){
    if(time == " "){
      return(-1000)
    }
    else{
      baseline <- as.POSIXct("2006/01/01 00:00:00") 
      ##判斷西元或民國  (轉換成西元)
      yyy <- as.numeric(strsplit(time,"/")[[1]][1])
      if(yyy < 1000){
        yyy<- as.character(yyy + 1911)
        time <- paste(yyy,strsplit(time,"/")[[1]][2],strsplit(time,"/")[[1]][3],sep = "/")
      }
      dts <- as.Date(strsplit(time," ")[[1]][1])
      tms <- strsplit(time," ")[[1]][2]
      
      Now <- as.POSIXct(time)
      eachday <- timeSequence(from=baseline,to=Now,FinCenter="Taipei")
      workingday <- eachday[isWeekday(eachday)]
      
      
      ##判斷是否假日
      if(isWeekend(dts)){  ##假日
        if(status == 0){
          return(length(workingday) * 9.5 *60)     
        }
        else if(status == 1){
          return(length(workingday) * 9.5 *60 + 1/60)
        }
        else{
          return(print("Wrong status"))
        }
      }
      
      else{   ##非假日
        lower <- as.POSIXct(paste(dts,"08:00:00"))
        upper <- as.POSIXct(paste(dts,"17:30:00"))
        
        #若非假日  判斷是否為上班時間
        if(Now < lower){      ##非上班時間  (當日上班時間前)
          if(status == 0){
            return(as.numeric(((length(workingday)-1) * 9.5)*60))      
          }
          else if(status == 1){
            return(as.numeric(((length(workingday)-1) * 9.5)*60) + 1/60)          
          }
          else{
            return(print("Wrong status"))
          }
        }
        else if(Now >= upper){ ##非上班時間  (當日上班時間後)
          if(status == 0){
            return(as.numeric((length(workingday))*9.5*60 ))
          }
          else if(status == 1){
            return(as.numeric(((length(workingday)) * 9.5)*60) + 1/60 )
          }
          else{
            return(print("Wrong status"))
          }
        }
        else{                  ##上班時間
          if(status == 0 | status == 1){
            xx <- as.numeric(Now)
            yy <- as.numeric(lower)
            return(as.numeric(((length(workingday)-1) * 9.5)*60) + (xx-yy)/60)
          }
          else{
            return(print("Wrong status"))
          }
        }  
      }
    }
  }
  
  ### 處理一篇公文, output 各個傳遞事件的情形
  handle_doc=function(data){
    
    
    lier1 = c(which(as.character(gsub(" ","",data[,"RcvUserID"])) == ""),
              which(as.character(gsub(" ","",data[,"RcvTime"])) == ""))
    lier = unique(lier1)
    lier = lier[ lier != length(data[,"DocNO"]) ]
    if(sum(lier)>0){data = data[-lier,]}
    l_d = length(data[,"DocNO"])
    
    k <- 1:l_d
    datas<- sapply(k, function(k) NewTimeLine(data[k,"SndTime"], 0))
    
    datar<- sapply(k, function(k) NewTimeLine(data[k,"RcvTime"], 1))
    
    wait_time_s=rep(0,l_d)
    wait_time_e=rep(0,l_d)
    start_time=rep(0,l_d)
    end_time=rep(0,l_d)
    b=0
    for(i in 1:(l_d-1)){
      #print(i)
      if((data[i+1,"TrnSts"]=="並會")| (data[i+1,"SndDesc"]=="並會")){b=1;q=1}
      if(b==1){
        index=0
        ####主管
        if((data[i,"RcvUserID"]==data[i+1,"SndUserID"])&((data[i+1,"TrnSts"]=="並會")|(data[i+1,"SndDesc"]=="並會"))){
          leader=data[i,"SndUserID"]
          ###計算並會個數nb
          j=i
          while(j!=0){
            if((data[j+1,"TrnSts"]=="並會")|data[j+1,"SndDesc"]=="並會"){
              j=j+1
            }
            else {
              nb=j-i
              j=0
            } 
          }
          #wait_time[i] = datar[i]-datas[i]
          wait_time_s[i]=datas[i]
          wait_time_e[i]=datar[i]
          start_time[i] = datar[i]
          end_time[i] = max(datas[(i+1):(i+nb)])
        }
        ####並會裡的承辦人
        else if((data[i,"TrnSts"]=="還承辦人"| ((data[i,"TrnSts"]=="公文退回" )& (data[i,"RcvUserID"]==leader)))&(q!=nb)){
          wait_time_s[i]=NA
          wait_time_e[i]=NA
          start_time[i] = NA
          end_time[i] = NA
          q=q+1
          index=c(index,i)
        }
        ####並會裡的路人
        else{
          tmp=i
          rr = 1
          while(rr==1){
            if(tmp < l_d){
              if(data[i,"RcvUserID"]!=data[tmp+1,"SndUserID"]){
                tmp=tmp+1
              }
              else{rr=0 ; start_time[i] = datar[i] ; end_time[i] = datas[tmp+1] }
            }
            else{rr=0 ; start_time[i] = 0 ; end_time[i] = 0 }
          }
          wait_time_s[i]=datas[i]
          wait_time_e[i]=datar[i]      
        }
        ###最後一個承辦人
        if((data[i,"TrnSts"]=="還承辦人"|((data[i,"TrnSts"]=="公文退回") & (data[i,"RcvUserID"]==leader)))&(q==nb)){
          b=0
          index=index[-1]
          index=c(index,i)
          #wait_time[i]=max(datar[index])-max(datas[index])
          wait_time_s[i]=max(datas[index])
          wait_time_e[i]=max(datar[index])
          start_time[i] = max(datar[index])
          end_time[i] = datas[i+1]
        }
      }
      if(b==0){
        tmp=i
        rr = 1
        while(rr==1){
          if(tmp < l_d){
            if(data[i,"RcvUserID"]!=data[tmp+1,"SndUserID"]){
              tmp=tmp+1
            }
            else{rr=0 ; start_time[i] = datar[i] ; end_time[i] = datas[tmp+1] }
          }
          else{rr=0 ; start_time[i] = 0 ; end_time[i] = 0 }
        }
        wait_time_s[i]=datas[i]
        wait_time_e[i]=datar[i]        
      }
      
    }
    
    
    #wait_time[l_d]=datar[l_d]-datas[l_d]
    wait_time_s[i]=datas[l_d]
    wait_time_e[i]=datar[l_d] 
    start_time[l_d] = 0
    end_time[l_d] = 0
    
    judge <- function(status){
      if (status %in% c("收文登記", "內部送會", "內會順會", "承辦改分", "創稿維護", 
                        "分文承辦", "公文改分", "公文移辦", "送文承辦", "文件退回",
                        "併案", "退回承辦人", "改變簽核形式", "文件送回")){
        s <- 1
      }else if (status %in% c("內部陳核", "分文陳核", "核判更正", "送文陳核", "陳核", 
                              "文件陳核")){ 
        s <- 2
      }else if (status %in% c("順會", "會辦改分", "分文會辦", "並會", "受會改分",
                              "後會", "送文會辦", "送登記桌", "還受會人")){
        s <- 3
      }else if (status %in% c("送文陳核", "陳核", "會辦陳核", "還承辦人")){ 
        s <- 4
      }else if (status %in% c("送存歸檔", "發文退回", "送發文", "送歸檔",
                              "文件送回", "送回")){
        s <- 5
      }else{
        s <- 0
      }                                  
    }
    status = sapply(data[,"TrnSts"], judge)
    
    
    l = length(wait_time_s)
    opt = cbind(data[,c(1,18,12,14,15)],c(status[-1],0),wait_time_s,wait_time_e,start_time,end_time)[-l,]
    if(sum(dim(opt)) == 0){ opt = t(as.matrix(opt))}
    tmpp=which((gsub(" ","",opt[,4])=="")&(opt[,3]!="總收文")&(opt[,3]!="總發文"))
    opt[tmpp,4] = "其他"
    
    return(opt)
  }
  
  ### 處理時間區間重疊
  cover_yes=function(x,M){
    if(is.vector(M)==1)M=t(as.matrix(M))
    yes=rep(0,nrow(M))
    for(i in 1:nrow(M)){
      if(x<=M[i,2]&&x>=M[i,1])yes[i]=1
    }
    yes
  }
  
  no_over=function(X){
    ttime=rep(0,nrow(X))
    loca=0
    for(k in 1:nrow(X)){
      if(X[k,2]-X[k,1]>(-1/59)&&X[k,2]-X[k,1]<(-1/61)){ttime[k]=1/60;loca=c(loca,k)}
      else if(X[k,2]-X[k,1]<0&&ttime[k]!=1/60){ttime[k]=NA;loca=c(loca,k)}
    }
    if(length(loca)==nrow(X)+1){}
    else{
      if(length(loca)>1){
        loca=loca[-1]
        NX=X[-loca,]
      }
      else {NX=X}
      if(is.vector(NX)==1)NX=t(as.matrix(NX))
      nttime=rep(0,nrow(NX))
      bk_pt=sort(unique(as.vector(NX)))
      if(length(bk_pt)==1){output=(NX[1,2]-NX[1,1])}
      else{
        new_intv=matrix(0,length(bk_pt)-1,3)
        
        for(i in 1:(length(bk_pt)-1)){
          new_intv[i,1:2]=bk_pt[c(i,i+1)]
          new_intv[i,3]=new_intv[i,2]-new_intv[i,1]
        }
        
        for(j in 1:nrow(new_intv)){
          j1=cover_yes(new_intv[j,1],NX)==1;j2=cover_yes(new_intv[j,2],NX)==1
          nttime[which(j1*j2==1)]=nttime[which(j1*j2==1)]+new_intv[j,3]/sum(j1*j2)  
        }
        output=nttime
      }
      k=1
      for(i in 1:length(ttime)){
        if(is.na(ttime[i])==0&&ttime[i]!=1/60){ttime[i]=output[k];k=k+1}
      }
    }
    ttime
  }
  
  handle_interval = function(target, ws, we, ps, pe){
    wait_time = c()
    proc_time = c()
    allobs = unique(target)
    for(i in 1:length(allobs)){
      obs = allobs[i]
      tset = which(target == obs)
      t_in = cbind(ws[tset], we[tset])
      naset = which(is.na(t_in[,1]))
      if(sum(naset)>0){wait_time[tset[-naset]] = no_over(t_in[-naset,])}
      else{wait_time[tset] = no_over(t_in)}
      wait_time[which(wait_time < 0)] = NA
      
      t_in = cbind(ps[tset], pe[tset])
      naset = which(is.na(t_in[,1]))
      if(sum(naset)>0){proc_time[tset[-naset]] = no_over(t_in[-naset,])}
      else{proc_time[tset] = no_over(t_in)}
    }
    return(list("wait_time" = wait_time , "proc_time" = proc_time))
  }
  
  ### 將資料變成丸包
  data2uanbao=function(data){
    
    
    ###轉換data type
    cn = colnames(data)
    data = as.matrix(data)
    colnames(data) = cn
    
    ###直接刪除不計時的狀況
    rr = which(data[,"TrnSts"]=="取消傳遞")
    rr = c(rr,rr-1)
    data=data[-rr,]
    
    qq <- c(which(data[,"TrnSts"]=="公文點收"),which(data[,"TrnSts"]=="改分"),
            which(data[,"TrnSts"]=="歸檔退回"),#which(data[,"TrnSts"]=="檔案製卷"),
            which(data[,"TrnSts"]=="發文歸檔"),which(data[,"TrnSts"]=="陳請判分"),
            which(data[,"TrnSts"]=="強制分派"),which(data[,"TrnSts"]=="副知"),
            which(data[,"TrnSts"]=="展辦"),#which(data[,"TrnSts"]=="取消傳遞"),
            which(data[,"TrnSts"]=="送用印"),which(data[,"TrnSts"]=="退文核閱"))
    
    delete.union <- unique(qq)
    
    
    data=data[-(delete.union),]
    
    #NewTimeLine(as.character(data[,21]),0)-NewTimeLine(as.character(data[,26]),0)
    #資料異動
    
    ###datas送件
    ###datar收件
    
    
    
    change=c(1)
    l = length(data[,"DocNO"])
    
    for(i in 1:(l-1)){
      #print(i)
      if(data[i,"DocNO"] != data[i+1,"DocNO"])
        change <- c(change,i+1)
    }
    change <- c(change,l+1)
    
    
    #資料異動26送件時間10
    #主角data[,18]
    #nb同一公文內並會次數
    
    
    print("第一階段開始")
    data_me = matrix(0,nrow=0,ncol=10)
    for(i in 1:(length(change)-1)){
      
      now = round(change[i+1] / change[length(change)], 4)*100
      print(paste("第一階段", now, "%"))
      
      tmp1 = handle_doc(data[change[i]:(change[i+1]-1),])
      data_me = rbind(data_me, tmp1)
    }
    print("第一階段結束")
    
    
    print("第二階段開始")
    ws = as.numeric(data_me[, 7])
    we = as.numeric(data_me[, 8])
    ps = as.numeric(data_me[, 9])
    pe = as.numeric(data_me[, 10])
    
    ### 處理人的時間區間
    people = as.character(data_me[,5])
    psn = handle_interval(people, ws, we, ps, pe)
    
    ### 處理處的時間區間
    deps = as.character(data_me[,3])
    dep = handle_interval(deps, ws, we, ps, pe)
    
    ### 處理組的時間區間
    divs0 = as.character(data_me[,4])
    divs = c()
    for(i in 1:length(divs0)){
      divs[i] = paste(deps[i], divs0[i])
    }
    
    div = handle_interval(divs, ws, we, ps, pe)
    
    print("第二階段結束")
    
    data_final = cbind(data_me[,1:6], psn$proc_time, psn$wait_time, psn$proc_time+psn$wait_time, 
                       div$proc_time, div$wait_time, div$proc_time+div$wait_time,
                       dep$proc_time, dep$wait_time, dep$proc_time+dep$wait_time)
    colnames(data_final) = iconv(c("doc","month","dep","div","person","status",
                                   "個人處理時間","個人等待時間","個人總時間",
                                   "組別處理時間","組別等待時間","組別總時間",
                                   "處室處理時間","處室等待時間","處室總時間"),"UTF-8")
    return(data_final)
  }
  
  ans = data2uanbao(dat)
  return(ans)
}