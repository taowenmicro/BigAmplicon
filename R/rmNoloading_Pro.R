#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param movepath
#' @param sizepath
#' @param info_path
#' @examples
#' removeRepeat()
#' @return data.frame
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn} Penghao Xie \email{2019103106@@njau.edu.cn}
#' @references
#'
#' Yuan J, Zhao J, Wen T, Zhao M, Li R, Goossens P, Huang Q, Bai Y, Vivanco JM, Kowalchuk GA, Berendsen RL, Shen Q
#' Root exudates drive the soil-borne legacy of aboveground pathogen infection
#' Microbiome 2018,DOI: \url{doi: 10.1186/s40168-018-0537-x}
#' @export


# #--转移过来后看看这些项目是否能用-根据NCBI爬虫爬下来的大小和样本数量过滤
# movepath = "./add_prig_pro//"
# sizepath = "E:\\下载项目中样本大小\\program/"
# topath = "no_downingpro_sra_loading/"
# rmNoloading_Pro(movepath,sizepath,topath)
#
# dir.create(topath)

rmNoloading_Pro = function(movepath,sizepath,topath){

  proload =  dir(movepath, pattern = c(""), full.names = TRUE, ignore.case = TRUE)

  IDload =  dir( movepath , pattern = c(""), full.names = FALSE, ignore.case = TRUE)
  IDload


  library(fs)

  A1= c()
  B1 = c()
  C1 = c()
  i= 1
  for (i in 1: length(proload)) {

    A = sapply(strsplit(gsub("=","-",proload[i]), "-"), `[`, 1)
    A = sapply(strsplit(A, "/"), `[`, length(strsplit(A, "/")[[1]]))
    # A = "PRJEB8599"
    # A = "PRJEB2435"

    name <- read.csv(paste(sizepath,A,"size.csv",sep = "/"),header = TRUE)
    head(name)

    M <- mean(name$bases)/1000000
    MM <- max(name$bases)/1000000
    M
    MM
    A1[i] = M
    B1[i] = MM
    C1[i] = A
    if(is.na(M)){
      M = 1
      MM = 1000
    }
    if( M < 1000 & MM < 2000) {
      print("不移动")
    }
    if(M > 1000  | MM > 2000) {

      file_move(proload[i],topath)
    }
  }

  #---其次对于只有一个样本的项目也不做下载
  #——--首先下载多个样本的研究数据，一个样本很有可能是混样测序,处理起来相比很起来会很复杂，所以我们要进行分批处理
  fl_1 =dir(movepath, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
  fl_1
  numSam <- rep("0",length(fl_1))
  for (i in 1:length(fl_1)) {
    path = paste(fl_1 [i],"SRR_Acc_List.txt",sep = "/")
    numSam[i] <- nrow(read.delim(path,header = FALSE))

  }
  #--发现有少数项目测序样本数量超过几千个
  table(numSam)
  #-筛选不是一个样本的项目
  down <- data.frame(name = fl_1 ,numsample = numSam) %>%
    filter(numsample == 1)
  dim(down)
  #--统计了一下数量，竟然超过了117万个样本
  sum(as.numeric(as.character(down$numsample)))
  # dir.create("download_amplicon_No1_1")
  file_move(as.character(down$name),topath)

}

