#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param addpath
#' @param sinkPath
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

#--处理下载未完成的项目的补充下载完成后合并到原来下载一半的项目中#--------------
# addpath = "./suopro_pro_sra_loading_xph///"
# sinkPath = "./pro_sra_loading_xph//"
# add_to_sinkPath(addpath,sinkPath)

#--如果addpath文件夹中有其他文件则保留
add_to_sinkPath = function(addpath,sinkPath){
  fl_10 =dir(addpath, pattern = c(""), full.names = F, ignore.case = TRUE)
  fl_1 = sapply(strsplit(gsub("=","-",fl_10), "-"), `[`, 1)

  fl_n =dir(sinkPath, pattern = c(""), full.names = FALSE, ignore.case = TRUE)
  fl_n = sapply(strsplit(fl_n, "="), `[`, 1)


  fl_1all =dir(addpath, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
  i= 1
  for (i in 1:length(fl_1)) {
    fl_1[i]
    #--提取转移文件全部样本
    allsam =dir(paste(fl_1all[i],"/SRA",sep = ""), pattern = c(""), full.names = TRUE, ignore.case = TRUE)

    #--构造目标转移文件路径
    fl_nall =dir(sinkPath, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
    if(!is.na(fl_nall[match(fl_1[i],fl_n)])){
      sinkfile = paste(fl_nall[match(fl_1[i],fl_n)],"/SRA",sep = "")
      #--将补充下载的转移过去
      file_move(allsam,sinkfile)
      #-删除转移过后的这个项目文件夹--还没写

    } else {

      file_move(paste(addpath,fl_10[i],sep = "/"),sinkPath)
    }

  }
}

