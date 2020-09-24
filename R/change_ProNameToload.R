#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param path
#' @examples
#' change_ProNameToload()
#' @return data.frame
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn} Penghao Xie \email{2019103106@@njau.edu.cn}
#' @references
#'
#' Yuan J, Zhao J, Wen T, Zhao M, Li R, Goossens P, Huang Q, Bai Y, Vivanco JM, Kowalchuk GA, Berendsen RL, Shen Q
#' Root exudates drive the soil-borne legacy of aboveground pathogen infection
#' Microbiome 2018,DOI: \url{doi: 10.1186/s40168-018-0537-x}
#' @export
#未下载下来的和不够完整的进行修改名称
#一个都没有下载下来的就去除后缀
#少于三分之二下载下来的写上写上等于号，继续下载

change_ProNameToload <- function(path){
  #这里我们提取符合要求的指定文件名称
  fl_1 =dir(path, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
  fl_1
  #-文件路径字符串化
  fl_2 <- as.character(fl_1)

  #-批量修改名称当时修改田间解释
  # 样本下下来至少三分之二了，就可以不用在下载了
  #其他请情况都需要继续下载数据，如果下下来的是0，那就改为原来的名字，如果不是，那就写下下来了多少个
  for (i in 1:length(fl_1)) {
    finnum <- sapply(strsplit(fl_2[i], "-"), `[`, 2)
    finnum
    orgnum <- sapply(strsplit(fl_2[i], "-"), `[`, 3)
    orgnum

    if (finnum == orgnum|as.numeric(finnum) >= as.numeric(orgnum) * 2/3
    ) {

    } else if ( as.numeric(finnum) == 0) {

      file.rename(fl_1[i],sapply(strsplit(fl_2[i], "-"), `[`, 1))
    } else if ( as.numeric(finnum) != 0 & as.numeric(finnum) <= as.numeric(orgnum) * 2/3 ) {

      a <- sapply(strsplit(fl_2[i], "-"), `[`, 1)
      b <- sapply(strsplit(fl_2[i], "-"), `[`, 2)
      c <- paste(a,b,sep = "=")
      file.rename(fl_1[i],c)
    }
  }


  #这里我们提取符合要求的指定文件名称
  fl_1 =dir(path, pattern = c("="), full.names = TRUE, ignore.case = TRUE)
  fl_1
  # i = 2

  if (length(fl_1) != 0) {
    for (i in 1:length(fl_1)) {
      paste(fl_1[i],"/SRA",sep = "")

      a =dir(paste(fl_1[i],"/SRA",sep = ""), pattern = c(""), full.names = FALSE, ignore.case = TRUE)
      a <- gsub(".sra","",a)
      a
      #---统计下载的文件数量和原始文件数量
      dowmnum <- length(a)
      orgnum <- length(as.character(read.delim(paste(fl_1[i],"SRR_Acc_List.txt",sep = "/"),header = FALSE)$V1))

      #--找到不同-另存为文件
      diffSam <- setdiff(as.character(read.delim(paste(fl_1[i],"SRR_Acc_List.txt",sep = "/"),header = FALSE)$V1),a)
      #--源文件改名
      file.rename(paste(fl_1[i],"SRR_Acc_List.txt",sep = "/"),paste(fl_1[i],"SRR_Acc_List_orig.txt",sep = "/"))
      #-将为下载的书提取出来存为文件名--就可以等待下次下载了
      write.table(data.frame(ID = diffSam),paste(fl_1[i],"SRR_Acc_List.txt",sep = "/"),row.names = F,col.names = F,quote = FALSE)
    }

  }



  #----尚未成功下载的，大概率也不会短时间内在另外一个夜晚下载得到，所以“=”部分的项目在最后下载。
  #这里我们提取符合要求的指定文件名称
  fl_1 =dir(path, pattern = c("="), full.names = TRUE, ignore.case = TRUE)


  if (length(fl_1) != 0) {
    for (i in 1:length(fl_1)) {
      #--源文件改名
      file.rename(fl_1[i],paste(fl_1[i],"-loading",sep = ""))
    }

  }

}
