#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param overpath
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


#--转移这部分完成OVER的项目到完成的里面去。
# overpath = "./pro_sra_loading_xph/"
# sinkpath = "./pro_sum_sra_adding_xph//"
# overPro_to_sinkpath(overpath,sinkpath)



#--该函数讲完成的项目也就是标记over的项目转移到目标文件夹中
#如果目标文件中存在这个项目了，则不会移动
overPro_to_sinkpath =function(overpath,sinkpath){
  fl_1 =dir(overpath, pattern = c("over"), full.names = FALSE, ignore.case = TRUE)
  fl_sum =dir(sinkpath, pattern = c("over"), full.names = FALSE, ignore.case = TRUE)
  fl_sum = sapply(strsplit(fl_sum, "-"), `[`, 1)
  fl_1 = sapply(strsplit(fl_1, "-"), `[`, 1)
  a = match(fl_sum,fl_1)
  if (length(a[!is.na(a)]) == 0) {
    fl_1 =dir(overpath, pattern = c("over"), full.names = TRUE, ignore.case = TRUE)
    file_move(fl_1,sinkpath)
  } else {
    fl_1 =dir(overpath, pattern = c("over"), full.names = TRUE, ignore.case = TRUE)
    fl_1 = fl_1[-a[!is.na(a)]]
    file_move(fl_1,sinkpath)
  }

}
