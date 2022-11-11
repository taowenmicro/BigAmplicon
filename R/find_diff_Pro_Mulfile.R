#' project from NCBI, related to amplicon and env microbiome project
#'
#' @param info_path
#' @param act_path
#' @param no_path
#' @param load_path
#' @param addPro
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


#---原始安排下载文件和实际完成文件做一个比对，找出没有的

# info_path = "./Tomato_xph_add_35_已安排番茄青枯病/"
# act_path = "./pro_sum_sra_adding_xph/"
# no_path = "./no_downingpro_sra_loading/"
# load_path = "./pro_sra_loading_xph/"
# addPro = "./add_prig_pro"
# find_diff_Pro_Mulfile (info_path = info_path ,act_path = act_path,no_path = no_path,load_path = load_path,addPro = addPro )

find_diff_Pro_Mulfile = function(info_path,act_path,no_path = NULL,load_path = NULL,addPro){
  dir.create(addPro)
  #-统计全部的样本和项目
  fl_orig =dir(info_path, pattern = c(""), full.names = F, ignore.case = TRUE)
  fl_act =dir(act_path, pattern = c(""), full.names = F, ignore.case = TRUE)

  #--读取list中的样本名
  A= sapply(strsplit(gsub("=","-",fl_act), "-"), `[`, 1)
  A= sapply(strsplit(A, "/"), `[`, length(strsplit(A, "/")[[1]]))
  A = gsub(".over","",A)
  A
  #--不需要下载的项目
  if (!is.null(no_path)) {
    fl_no =dir(no_path, pattern = c(""), full.names = F, ignore.case = TRUE)
    A1= sapply(strsplit(gsub("=","-",fl_no), "-"), `[`, 1)
    A1= sapply(strsplit(A1, "/"), `[`, length(strsplit(A1, "/")[[1]]))
    A1 = gsub(".over","",A1)
    A1
    A= c(A,A1)
  }
  A
  #-
  if (!is.null(load_path)) {
    fl_load =dir(load_path, pattern = c(""), full.names = F, ignore.case = TRUE)
    A2= sapply(strsplit(gsub("=","-",fl_load), "-"), `[`, 1)
    A2= sapply(strsplit(A2, "/"), `[`, length(strsplit(A2, "/")[[1]]))
    A2 = gsub(".over","",A2)
    A2
    A= c(A,A2)
  }

  A

  print(setdiff(fl_orig,A))

  if (length(setdiff(fl_orig,A)) != 0) {
    for (i in 1:length(setdiff(fl_orig,A))) {
      dir_copy(paste(info_path,setdiff(fl_orig,A),sep = "/")[i],paste("./add_prig_pro",setdiff(fl_orig,A),sep = "/")[i])
    }
  }
}
