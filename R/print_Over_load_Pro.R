#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param path
#' @examples
#' print_Over_load_Pro()
#' @return data.frame
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn} Penghao Xie \email{2019103106@@njau.edu.cn}
#' @references
#'
#' Yuan J, Zhao J, Wen T, Zhao M, Li R, Goossens P, Huang Q, Bai Y, Vivanco JM, Kowalchuk GA, Berendsen RL, Shen Q
#' Root exudates drive the soil-borne legacy of aboveground pathogen infection
#' Microbiome 2018,DOI: \url{doi: 10.1186/s40168-018-0537-x}
#' @export
#统计下载完成后的下载量统计#-----------------
print_over_Pro <- function(){

  #--计算目前成功的项目数量
  fl_1 =dir(path, pattern = c("over"), full.names = TRUE, ignore.case = TRUE)
  length(fl_1)
  #----计算下载成功项目中的样本数量
  num <- as.numeric(sapply(strsplit(fl_1, "-"), `[`, 2))
  sum(num)
  #--统计全部下载的文件大小，由于list等几个文本实在是太小，所以这里统计全部为over的文件夹的大小
  size <- dir_info(path =fl_1, recurse = TRUE) %>%
    # group_by(directory = path_dir(path)) %>%
    tally(wt = size, sort = TRUE)



  ss = paste(path," 时间节点 ",Sys.Date( )," ; ","研究数量(study) : ",
             length(fl_1)," ; ","样本数量(sample) : ",sum(num),
             "; 文件大小(size):",size$n,sep = "")

  return(ss)
}
#---------------------尚未完成的项目统计#-----------
print_load_Pro <- function(){

  fl_1 =dir(path, pattern = c("="), full.names = TRUE, ignore.case = TRUE)

  length(fl_1)

  #----计算下载成功项目中的样本数量
  num <- as.numeric(sapply(strsplit(gsub("=","-",fl_1), "-"), `[`, 2))
  sum(num)

  #--统计全部下载的文件大小，由于list等几个文本实在是太小，所以这里统计全部为over的文件夹的大小
  size <- dir_info(path =fl_1, recurse = TRUE) %>%
    # group_by(directory = path_dir(path)) %>%
    tally(wt = size, sort = TRUE)

  size

  ss = paste(path,"-时间节点-",Sys.Date( )," ; ","未完成研究数量(study) : ",
             length(fl_1)," ; ","未完样本(sample) : ",sum(num),
             "; 未完成样本文件大小(size):",size$n,sep = "")
  # ss = paste(path," 时间节点 ",Sys.Date( )," ; ","未完成研究数量(study) : ",
  #       length(fl_1),";","未完成样本数量(sample)",sum(num),
  #       ";-未完成文件大小(size):",size$n,sep = "")
  return(ss)

}

print_Over_load_Pro<- function(){
  a = print_over_Pro()
  b = print_load_Pro()
  print(c(a,b))

}
