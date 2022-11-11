#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param loadingpath
#' @param suoloadpath
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

#----抽提子文件再次下载loading
#----为下载完成的项目根据总表格生成二级下载文件-由我来进行全部的样本下载任务#----------
# i = 1
# loadingpath = "./pro_sra_loading_xph///"
# suoloadpath = "./suopro_pro_sra_loading_xph"
# info_path = info_path
# subloadingPro(loadingpath,suoloadpath,info_path)

subloadingPro = function(loadingpath,suoloadpath,info_path){
  dir.create(suoloadpath)
  subpro1 =dir(loadingpath, pattern = c(""), full.names = FALSE, ignore.case = TRUE)
  subpro0 =dir(loadingpath, pattern = c(""), full.names = TRUE, ignore.case = TRUE)
  for (i in 1:length(subpro1)) {
    subpro1[i]
    proid <- sapply(strsplit(subpro1[i], "="), `[`, 1)

    pro <- dir(suoloadpath, pattern = proid, full.names = FALSE, ignore.case = TRUE)
    if (length(pro) == 0) {
      dir_copy(paste(info_path,proid,sep = "/"),suoloadpath)

    }

    #--得到已下载的样本
    a =dir(paste(subpro0[i],"/SRA",sep = ""), pattern = c("sra"), full.names = FALSE, ignore.case = TRUE)
    a <- gsub(".sra","",a)
    a
    orgname <- as.character(read.delim(paste(paste(info_path,proid,sep = "/"),"SRR_Acc_List.txt",sep = "/"),header = FALSE)$V1)
    #---原始文件数量
    orgnum <- length(orgname)
    #--找到不属于这里面的样本
    diffSam <- setdiff(orgname,a)
    write.table(diffSam,paste(suoloadpath,proid,"SRR_Acc_List.txt",sep = "/"),
                row.names = F,col.names = F,quote = F)

  }
}
