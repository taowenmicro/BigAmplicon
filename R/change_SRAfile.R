#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param path
#' @examples
#' change_SRAfile()
#' @return data.frame
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn} Penghao Xie \email{2019103106@@njau.edu.cn}
#' @references
#'
#' Yuan J, Zhao J, Wen T, Zhao M, Li R, Goossens P, Huang Q, Bai Y, Vivanco JM, Kowalchuk GA, Berendsen RL, Shen Q
#' Root exudates drive the soil-borne legacy of aboveground pathogen infection
#' Microbiome 2018,DOI: \url{doi: 10.1186/s40168-018-0537-x}
#' @export
#--提取全部分类错误的样本（  other_sample）位置#---------------------------------
change_SRAfile <- function(path){
  fl_1 =dir(path, pattern = c(""), full.names = TRUE, ignore.case = TRUE)

  other_sample = c()
  for (i in 1:length(fl_1)) {
    #-提取子文件夹内内容
    fl_2 =dir(fl_1[i], pattern = c("other_allSample"), full.names = TRUE, ignore.case = TRUE)
    fl_2
    if (length(fl_2) == 0) {
      i = i +1
    }
    if (length(fl_2) != 0) {

      o_sra =dir(paste(fl_1[i],"/other_allSample/",sep = ""), pattern = c(""), full.names = TRUE, ignore.case = TRUE)
      o_sra
      other_sample = c(other_sample ,o_sra)
    }
  }
  #这便是全部分错的样本，还有听不少的。
  other_sample
  if (length(other_sample) != 0) {
    for (i in 1:length(other_sample)) {
      a = sapply(strsplit(other_sample[i], "/"), `[`, 5)
      a = gsub(".sra","",a)
      orgdata$ID = as.character(orgdata$ID)
      b = orgdata %>% filter(ID == a )

      B =dir(path, pattern = c(sapply(strsplit(as.character(b$path), "/"), `[`, 3)), full.names = TRUE, ignore.case = TRUE)
      B
      file_move(other_sample[i],paste(B,"/SRA",sep = ""))

    }
  }

}

