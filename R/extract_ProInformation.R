#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param path
#' @examples
#' extract_ProInformation()
#' @return data.frame
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn} Penghao Xie \email{2019103106@@njau.edu.cn}
#' @references
#'
#' Yuan J, Zhao J, Wen T, Zhao M, Li R, Goossens P, Huang Q, Bai Y, Vivanco JM, Kowalchuk GA, Berendsen RL, Shen Q
#' Root exudates drive the soil-borne legacy of aboveground pathogen infection
#' Microbiome 2018,DOI: \url{doi: 10.1186/s40168-018-0537-x}
#' @export
#--提取项目信息，将每个NCBI上的项目的list文件内容提取出来合并，标记每个样本属于的项目信息
extract_ProInformation <- function(info_path){
  #-统计全部的样本和项目
  fl_1 =dir(info_path, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
  for (i in 1:length(fl_1)) {
    #--读取list中的样本名
    A = sapply(strsplit(gsub("=","-",fl_1[i]), "/"), `[`,length(strsplit(gsub("=","-",fl_1[i]), "/")[[1]]))
    A

    name <- as.character(read.delim(paste(info_path,A,"SRR_Acc_List.txt",sep = "/"),header = FALSE)$V1)
    name
    if (i == 1) {
      orgdata <- data.frame(ID = name,path = paste(path,A,sep = "/") )

    }
    if (i != 1) {
      data <- data.frame(ID = name,path = paste(path,A,sep = "/"))
      orgdata <- rbind(orgdata,data)
    }


  }

  #--去除重复
  orgdata = orgdata %>% distinct()
  head(orgdata)

  write.csv(orgdata,"./all_list_information.csv")
}
